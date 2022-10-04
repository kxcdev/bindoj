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

module type ApiDirManifest = sig
  val registry_info : unit -> invocation_point_collection * type_decl_collection
end

(* TODO.future - make it take a Configurator, or make it generative #220 *)
module ApiHandlerBridge
         (Dir : ApiDirManifest)
         (IoStyle : sig
            type 'x t
            val return : 'x -> 'x t
            val bind : 'x t -> ('x -> 'y t) -> 'y t
          end)
= struct
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

  let handler_registry_post = Hashtbl.create 0
  let handler_registry_get = Hashtbl.create 0

  let register_post_handler (type reqty) (type respty) :
     (reqty, respty) invp ->
     (reqty -> respty io) -> unit =
    fun invp func ->
    Hashtbl.replace handler_registry_post
      (Invp invp)
      (Handler (invp, func))

  let register_get_handler (type respty) :
     (unit, respty) invp ->
     (unit -> respty io) -> unit =
    fun invp func ->
    Hashtbl.replace handler_registry_get
      (Invp invp)
      (Handler (invp, func))

  let registry_info = Dir.registry_info()
  let invocation_points, type_decls =
    let invocation_points, type_decls = registry_info in
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

  let path_index_get, path_index_post =
    let post_index = Hashtbl.create invp_count in
    let get_index = Hashtbl.create invp_count in
    invocation_points |!> (fun ((Invp invp) as invp') ->
      let index = match invp.ip_method with
        | `post -> post_index
        | `get -> get_index in
      Hashtbl.replace index invp.ip_urlpath invp'
    );
    get_index, post_index

  module Internals = struct
    let ttd_name (type t) ((module Td) : t Typed_type_desc.typed_type_decl) =
      Td.decl.td_name
    let ttd_of_media_type ({ mt_type; _ }) = mt_type
  end open Internals

  let handle_json_post : invp' -> jv -> jv io =
    fun invp reqbody ->
    match Hashtbl.find_opt handler_registry_post invp with
    | None ->
       invalid_arg
         "no handler registered for the requested api"
    | Some (Handler (invp, handler)) ->
       let req = match invp.ip_method, invp.ip_request_body with
         | `get, _ -> invalid_arg' "handle_json_post got GET invp: %s" invp.ip_name
         | `post, None -> invalid_arg' "POST method must have a request body definition: %s" invp.ip_name
         | `post, Some desc ->
            let ttd = ttd_of_media_type desc.rq_media_type in
            (match reqbody |> Bindoj_codec.Json.of_json ~env:tdenv ttd with
             | None -> Utils.bad_request "invalid json format for type %s: %a"
                         (ttd_name ttd) Utils.pp_jv reqbody
             | Some req -> req) in
       let resp_ttd =
         (* TODO.future - now assuming there is one and exactly one response desc #216 *)
         match invp.ip_responses with
         | [`default, desc] -> ttd_of_media_type desc.rs_media_type
         | _ -> failwith' "panic @%s" __LOC__ in
       handler req >|= (Bindoj_codec.Json.to_json ~env:tdenv resp_ttd)

  let handle_json_get : invp' -> jv io =
    fun invp ->
    match Hashtbl.find_opt handler_registry_get invp with
    | None ->
       invalid_arg
         "no handler registered for the requested api"
    | Some (Handler (invp, handler)) ->
       (match invp.ip_method with
        | `post -> invalid_arg' "handle_json_get got Post invp: %s" invp.ip_name
        | `get -> ()
        | _ -> .);
       let resp_ttd =
         (* TODO.future - now assuming there is one and exactly one response desc #216 *)
         match invp.ip_responses with
         | [`default, desc] -> ttd_of_media_type desc.rs_media_type
         | _ -> failwith' "panic @%s" __LOC__ in
       handler (() |> Obj.magic) >|= (Bindoj_codec.Json.to_json ~env:tdenv resp_ttd)

  let handle_path_json_post : string -> jv -> jv io =
    fun path reqbody ->
    match Hashtbl.find_opt path_index_post path with
    | None -> raise (Utils.Exceptions.Unrecognized_route path)
    | Some invp -> handle_json_post invp reqbody
  let handle_path_json_get : string -> jv io =
    fun path ->
    match Hashtbl.find_opt path_index_get path with
    | None -> raise (Utils.Exceptions.Unrecognized_route path)
    | Some invp -> handle_json_get invp

  (* TODO.future - add method to check completeness of handles #219 *)
  (* TODO.future - add method to check completeness of type_decl_collection #219 *)
end
