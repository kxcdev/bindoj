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
include Type_desc
open Bindoj_runtime

type 'a typed_type_decl =
  (type_decl, 'a) generic_typed_type_decl

type boxed_type_decl =
  | Boxed : 'a typed_type_decl -> boxed_type_decl

module Typed = struct
  let mk decl reflect = mk_generic_typed_type_decl decl reflect

  let decl : 'a typed_type_decl -> type_decl =
    fun (type a) ((module A) : a typed_type_decl) -> A.decl

  let reflect : 'a typed_type_decl -> 'a Refl.result =
    fun (type a) ((module A) : a typed_type_decl) -> Lazy.force A.reflect

  let to_refl : 'a typed_type_decl -> 'a Refl.t =
    fun (type a) ((module A) : a typed_type_decl) -> A.reflect

  let cast (a: 'a typed_type_decl) (b: 'b typed_type_decl) (value: 'a) : 'b option =
    if decl a = decl b then Some (Obj.magic value : 'b)
    else None

  let box (a: 'a typed_type_decl) : boxed_type_decl = Boxed a

  let unbox (Boxed a) : 'a typed_type_decl = Obj.magic a
end
