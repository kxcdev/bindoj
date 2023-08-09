(* Copyright 2022-2023 Kotoi-Xie Consultancy, Inc. This file is a part of the

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

module type JsonResponse = sig
  type t

  (** The HTTP status code of the response *)
  val status : t -> int

  (** The body of the response in JSON *)
  val body : t -> Json.jv
end

module Make (Dir : ApiDirManifest) (IoStyleP : Monadic) = struct
  type 'x io = 'x IoStyleP.t
  module IoStyle = IoStyleP
  module IoOps = MonadOps(IoStyle)

  let registry_info = Dir.registry_info()
  let invocation_points, type_decls =
    let invocation_points, type_decls = registry_info in
    invocation_points, type_decls
  let tdenv = tdenv_of_registry_info registry_info

  let invp_count =
    let counter = ref 0 in
    invocation_points |!> (fun _ -> incr counter;);
    !counter

  let index_get, index_post =
    let post_index = Hashtbl.create invp_count in
    let get_index = Hashtbl.create invp_count in
    invocation_points |!> (fun ((Invp invp) as invp') ->
      let index = match invp.ip_method with
        | `post -> post_index
        | `get -> get_index in
      Hashtbl.replace index invp.ip_urlpath invp'
    );
    get_index, post_index

  (* TODO.future - add method to check completeness of type_decl_collection #219 *)
end
