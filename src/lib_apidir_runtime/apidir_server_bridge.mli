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
open Bindoj_apidir_shared

module TupleJsonResponse : (Apidir_base.JsonResponse with type t = int * Json.jv)

module type T = sig
  type 'resp io

  val register_get_handler :
    (unit, 'respty) invocation_point_info -> (unit -> (int * 'respty) io) -> unit
  val register_post_handler :
    ('reqty, 'respty) invocation_point_info -> ('reqty -> (int * 'respty) io) -> unit

  val handle_json_get : untyped_invocation_point_info -> TupleJsonResponse.t io
  val handle_json_post : untyped_invocation_point_info -> Json.jv -> TupleJsonResponse.t io

  val handle_path_json_get : string -> TupleJsonResponse.t io
  val handle_path_json_post : string -> Json.jv -> TupleJsonResponse.t io
end

module Make :
  functor (Dir : ApiDirManifest) (IoStyle : Monadic) ->
    sig
      include module type of Apidir_base.Make(Dir)(IoStyle)

      type handler =
        | Handler : ('reqty, 'respty) invocation_point_additional_info
                  * ('reqty -> (int * 'respty) io) -> handler

      val handler_registry_get :
        (invocation_point_meta, handler) Hashtbl.t
      val handler_registry_post :
        (invocation_point_meta, handler) Hashtbl.t

      include T with type 'resp io = 'resp IoStyle.t
    end with module IoStyle = IoStyle
