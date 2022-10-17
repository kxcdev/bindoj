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
open Kxclib
open Kxclib.Json
open Bindoj_base
open Bindoj_apidir_shared

exception Bad_response of jv * Typed_type_desc.boxed_type_decl * string option

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

module type T = sig
  type 'resp io

  val perform_json_post :
    ?additional_headers:string list ->
    ?additional_query_params:(string * string) list ->
    ('req, 'resp) invocation_point_info -> 'req -> 'resp io

  val perform_json_get :
    ?additional_headers:string list ->
    ?additional_query_params:(string * string) list ->
    (unit, 'resp) invocation_point_info -> 'resp io
end

module Make (Dir : ApiDirManifest) (Fetcher : ScopedJsonFetcher) = struct
  module IoStyle = Fetcher.IoStyle
  include Apidir_base.Make(Dir)(IoStyle)
  open IoOps

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

  let perform_json_post :
        'req 'resp.
        ?additional_headers:string list
        -> ?additional_query_params:(string*string) list
        -> ('req, 'resp) invocation_point_info
        -> 'req
        -> 'resp io =
    fun ?additional_headers ?additional_query_params invp reqbody ->
    let req = match invp.ip_method, invp.ip_request_body with
      | `get, _ -> invalid_arg' "perform_json_post called on GET invp: %s" invp.ip_name
      | `post, None -> invalid_arg' "POST method must have a request body definition: %s" invp.ip_name
      | `post, Some desc ->
         let ttd = Utils.ttd_of_media_type desc.rq_media_type in
         reqbody |> Bindoj_codec.Json.to_json ~env:tdenv ttd in
    let resp_ttd =
      (* TODO.future - now assuming there is one and exactly one response desc #216 *)
      match invp.ip_responses with
      | [`default, desc] -> Utils.ttd_of_media_type desc.rs_media_type
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
        -> (unit, 'resp) invocation_point_info -> 'resp io =
    fun ?additional_headers ?additional_query_params invp ->
    let resp_ttd =
      (* TODO.future - now assuming there is one and exactly one response desc #216 *)
      match invp.ip_responses with
      | [`default, desc] -> Utils.ttd_of_media_type desc.rs_media_type
      | _ -> failwith' "panic @%s" __LOC__ in
    let urlpath = invp.ip_urlpath in
    let query_params = additional_query_params |? [] in
    let headers = additional_headers |? [] in
    Fetcher.perform_get ~urlpath ~headers ~query_params
    >>= process_response resp_ttd
end
