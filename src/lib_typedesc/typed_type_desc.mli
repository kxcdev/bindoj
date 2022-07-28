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
include module type of Type_desc
open Bindoj_runtime

type 'a typed_type_decl =
  (type_decl, 'a) generic_typed_type_decl

type boxed_type_decl =
  | Boxed : 'a typed_type_decl -> boxed_type_decl

module Typed : sig
  val mk : type_decl -> 'a Refl.t -> 'a typed_type_decl

  val decl : 'a typed_type_decl -> type_decl

  val reflect : 'a typed_type_decl -> 'a Refl.result

  val to_refl : 'a typed_type_decl -> 'a Refl.t

  val cast : 'a typed_type_decl -> 'b typed_type_decl -> 'a -> 'b option

  val box : 'a typed_type_decl -> boxed_type_decl

  val unbox : boxed_type_decl -> 'a typed_type_decl
end
