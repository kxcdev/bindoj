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
open Bindoj_apidir_shared

module type T = sig
  type 'resp io

  val register_get_handler :
    (unit, 'respty) invocation_point_info -> (unit -> 'respty io) -> unit
  val register_post_handler :
    ('reqty, 'respty) invocation_point_info -> ('reqty -> 'respty io) -> unit

  val handle_json_get : untyped_invocation_point_info  -> Json.jv io
  val handle_json_post : untyped_invocation_point_info  -> Json.jv -> Json.jv io

  val handle_path_json_get : string -> Json.jv io
  val handle_path_json_post : string -> Json.jv -> Json.jv io
end

(* TODO.future - make it take a Configurator, or make it generative #220 *)
module Make (Dir : ApiDirManifest) (IoStyle : Monadic) = struct
  include Apidir_base.Make(Dir)(IoStyle)
  open IoOps

  type ('reqty, 'respty) invp = ('reqty, 'respty) invocation_point_info
  type invp' = untyped_invocation_point_info

  type handler =
    | Handler : ('reqty, 'respty) invp * ('reqty -> 'respty io) -> handler

  let handler_registry_post : (invp', handler) Hashtbl.t = Hashtbl.create 0
  let handler_registry_get : (invp', handler) Hashtbl.t = Hashtbl.create 0

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
            let ttd = Utils.ttd_of_media_type desc.rq_media_type in
            (match reqbody |> Bindoj_codec.Json.of_json ~env:tdenv ttd with
             | None -> Utils.bad_request "invalid json format for type %s: %a"
                         (Utils.ttd_name ttd) Utils.pp_jv reqbody
             | Some req -> req) in
       let resp_ttd =
         (* TODO.future - now assuming there is one and exactly one response desc #216 *)
         match invp.ip_responses with
         | [`default, desc] -> Utils.ttd_of_media_type desc.rs_media_type
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
         | [`default, desc] -> Utils.ttd_of_media_type desc.rs_media_type
         | _ -> failwith' "panic @%s" __LOC__ in
       handler (() |> Obj.magic) >|= (Bindoj_codec.Json.to_json ~env:tdenv resp_ttd)

  let handle_path_json_post : string -> jv -> jv io =
    fun path reqbody ->
    match Hashtbl.find_opt index_post path with
    | None -> raise (Utils.Exceptions.Unrecognized_route path)
    | Some invp -> handle_json_post invp reqbody

  let handle_path_json_get : string -> jv io =
    fun path ->
    match Hashtbl.find_opt index_get path with
    | None -> raise (Utils.Exceptions.Unrecognized_route path)
    | Some invp -> handle_json_get invp

  (* TODO.future - add method to check completeness of handles #219 *)
end
