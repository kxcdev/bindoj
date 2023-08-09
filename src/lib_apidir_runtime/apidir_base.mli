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
open Bindoj_apidir_shared

module type JsonResponse = sig
  type t

  (** The HTTP status code of the response *)
  val status : t -> int

  (** The body of the response in JSON *)
  val body : t -> Json.jv
end

module Make :
  functor (Dir : ApiDirManifest) (IoStyle : Monadic) ->
    sig
      type 'x io = 'x IoStyle.t
      module IoStyle : module type of IoStyle
      module IoOps : module type of Kxclib.MonadOps(IoStyle)

      val registry_info : registry_info
      val invocation_points : invocation_point_collection
      val type_decls : type_decl_collection
      val tdenv : Bindoj_base.tdenv
      val invp_count : int
      val index_get : (string, untyped_invocation_point_info) Hashtbl.t
      val index_post : (string, untyped_invocation_point_info) Hashtbl.t
    end with module IoStyle = IoStyle
