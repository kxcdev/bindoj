(* Copyright 2022 Kotoi-Xie Consultancy, Inc. This file is a part of the

==== Bindoj (https://kxc.dev/bindoj) ====

software project that is developed, maintained, and distributed by
Kotoi-Xie Consultancy, Inc. (https://kxc.inc) which is also known as KXC.

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy
of the License at http://www.apache.org/licenses/LICENSE-2.0. Unless required
by applicable law or agreed to in writing, software distributed under the
License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
OF ANY KIND, either express or implied. See the License for the specific
language governing permissions and limitations under the License.
                                                                              *)
(* Acknowledgements  --- AnchorZ Inc. ---  The current/initial version or a
significant portion of this file is developed under the funding provided by
AnchorZ Inc. to satisfy its needs in its product development workflow.
                                                                              *)
open Kxclib.Json
open Kxclib
open Bindoj_apidir_shared

module Typed_type_desc = Bindoj_typedesc.Typed_type_desc

open[@warning "-33"] Log0

(* TODO - port back to bindoj *)

module type ApiDirManifest = sig
  val registry_info : unit -> invocation_point_collection * type_decl_collection
end

exception Bad_response of jv*Typed_type_desc.boxed_type_decl*string option

module type ScopedJsonFetcher = sig
  module IoStyle: Utils.IoStyle
  val perform_get :
    urlpath:string
    -> headers:string list
    -> query_params:(string*string) list
    -> jv IoStyle.t
  val perform_post :
    urlpath:string
    -> headers:string list
    -> query_params:(string*string) list
    -> body:jv
    -> jv IoStyle.t
end

module MakeApiClient
         (Dir : ApiDirManifest)
         (Fetcher : ScopedJsonFetcher)
= struct
  module IoStyle = Fetcher.IoStyle
  type 'x io = 'x IoStyle.t
  module IoOps = MonadOps(IoStyle)
  open IoOps

  open Bindoj_base

  type ('reqty, 'respty) invp =
    ('reqty, 'respty) invocation_point_info
  type invp' = untyped_invocation_point_info

  type handler =
    | Handler :
        ('reqty, 'respty) invp *
          ('reqty -> 'respty io)
        -> handler

  (* TODO - common code with ApiServerBridge *)
  let registry_info = Dir.registry_info()
  let invocation_points, type_decls =
    let invocation_points, type_decls = registry_info in
    let type_decls =
      type_decls
      |-> (fun tdis ->
        tdis |&> (fun tdi -> tdi.tdi_name) |>
          verbose "type_decls: %a" List.(pp pp_string)) in
    invocation_points, type_decls

  let tdenv :
        Typed_type_desc.boxed_type_decl StringMap.t =
    type_decls
    |> foldl (fun acc info ->
           acc |> StringMap.add info.tdi_name info.tdi_decl
         ) StringMap.empty

  let invp_count =
    let counter = ref 0 in
    invocation_points |!> (fun (Invp invp) ->
      incr counter;
      match invp.ip_responses with
      | [`default, _resp] -> ()
      | _ -> failwith' "ApiHandlerBridge: panic - we currently could only \
                        handle invp with exactly one default response type desc: \
                        name=%s; urlpath=%s;"
               invp.ip_name invp.ip_urlpath);
    !counter

  let invp_index_get, invp_index_post =
    let post_index = Hashtbl.create invp_count in
    let get_index = Hashtbl.create invp_count in
    invocation_points |!> (fun ((Invp invp) as invp') ->
      let path = invp.ip_urlpath in
      let index, index_label = match invp.ip_method with
        | `post -> post_index, "post"
        | `get -> get_index, "get " in
      verbose "invp.%s added: [%s]" path index_label;
      Hashtbl.replace index invp.ip_urlpath invp'
    );
    get_index, post_index

  module Internals = struct
    let ttd_name (type t) ((module Td) : t Typed_type_desc.typed_type_decl) =
      Td.decl.td_name
    let ttd_of_media_type ({ mt_type; _ }) = mt_type
    let process_response ttd jv =
      let resp_type_name = (Typed_type_desc.Typed.decl ttd).td_name in
      match Bindoj_codec.Json.of_json ~env:tdenv ttd jv with
      | exception exn ->
         Bad_response (
             jv,
             Typed_type_desc.Boxed ttd,
             Printexc.to_string exn |> some
           ) |> IoStyle.inject_error
      | None ->
         Bad_response (
             jv,
             Typed_type_desc.Boxed ttd,
             sprintf "Bindoj_codec.Json.to_json (%s) returns None"
               resp_type_name |> some
           ) |> IoStyle.inject_error
      | Some x -> return x

  end open Internals

  let perform_json_post :
        'req 'resp.
        ?additional_headers:string list
        -> ?additional_query_params:(string*string) list
        -> ('req, 'resp) invp
        -> 'req
        -> 'resp io =
    fun ?additional_headers ?additional_query_params invp reqbody ->
    let req = match invp.ip_method, invp.ip_request_body with
      | `get, _ -> invalid_arg' "perform_json_post called on GET invp: %s" invp.ip_name
      | `post, None -> invalid_arg' "POST method must have a request body definition: %s" invp.ip_name
      | `post, Some desc ->
         let ttd = ttd_of_media_type desc.rq_media_type in
         reqbody |> Bindoj_codec.Json.to_json ~env:tdenv ttd in
    let resp_ttd =
      (* TODO - now assuming there is one and exactly one response desc *)
      match invp.ip_responses with
      | [`default, desc] -> ttd_of_media_type desc.rs_media_type
      | _ -> failwith' "panic @%s" __LOC__ in
    let urlpath = invp.ip_urlpath in
    let query_params = additional_query_params |? [] in
    let headers = additional_headers |? [] in
    Fetcher.perform_post ~urlpath ~headers ~query_params ~body:req
    >>= process_response resp_ttd

  let perform_json_get :
        'resp.
        ?additional_headers:string list
        -> ?additional_query_params:(string*string) list
        -> (unit, 'resp) invp -> 'resp io =
    fun ?additional_headers ?additional_query_params invp ->
    let resp_ttd =
      (* TODO - now assuming there is one and exactly one response desc *)
      match invp.ip_responses with
      | [`default, desc] -> ttd_of_media_type desc.rs_media_type
      | _ -> failwith' "panic @%s" __LOC__ in
    let urlpath = invp.ip_urlpath in
    let query_params = additional_query_params |? [] in
    let headers = additional_headers |? [] in
    Fetcher.perform_get ~urlpath ~headers ~query_params
    >>= process_response resp_ttd

  (* TODO - add method to check completeness of type_decl_collection *)
end
