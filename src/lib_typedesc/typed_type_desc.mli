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
(** This module provides functionalities to handle typed types. *)
include module type of Type_desc
open Bindoj_runtime

(** Represents a typed type declaration that is parameterized by 'a. *)
type 'a typed_type_decl =
  (type_decl, 'a) generic_typed_type_decl

(** Represents a boxed type declaration. *)
type boxed_type_decl =
  | Boxed : 'a typed_type_decl -> boxed_type_decl

val of_generic_boxed : type_decl boxed_generic_typed_type_decl -> boxed_type_decl

module Typed : sig
  (** This module provides functionalities to handle {!typed_type_decl}. *)

  val mk : type_decl -> 'a Refl.t -> 'a typed_type_decl
  (** Creates a new typed type declaration of the given type declaration and reflect. *)

  val decl : 'a typed_type_decl -> type_decl
  (** Returns the type declaration of the given typed type declaration. *)

  val reflect : 'a typed_type_decl -> 'a Refl.result
  (** Returns the result of the reflect of the given typed type declaration. *)

  val to_refl : 'a typed_type_decl -> 'a Refl.t
  (** Returns the reflect of the given typed type declaration. *)

  val cast : 'a typed_type_decl -> 'b typed_type_decl -> 'a -> 'b option
  (** Casts the given typed type declaration to one of a different type, if possible. *)

  val box : 'a typed_type_decl -> boxed_type_decl
  (** Makes the given typed type declaration boxed. *)

  val unbox : boxed_type_decl -> 'a typed_type_decl
  (** Makes the given boxed type declaration unboxed. *)
end

module Type_decl_environment : sig
  (** Holds a map of external format codecs for a user primitive descriptor. *)
  type 'camlrepr user_primitive_descriptor = {
      external_format_codecs :
        ('camlrepr External_format.codec')
          External_format.label_map
    }

  (** Represents a boxed user primitive descriptor. *)
  type boxed_user_primitive_descriptor =
    | Boxed_prim :
        'camlrepr user_primitive_descriptor*'camlrepr typed_type_decl
        -> boxed_user_primitive_descriptor

  (** An environment containing mappings for primitive and alias identifiers. *)
  type env = {
      prim_ident_typemap : boxed_user_primitive_descriptor StringMap.t;
      (** definition for user defined primitives, that is,
          coretypes with Ident desc that denotate a user defined primitive type. *)

      alias_ident_typemap : boxed_type_decl StringMap.t;
      (** definition for user defined aliases, that is,
          coretypes with Ident desc that refers to another {!Type_desc.type_decl}. *)
    }

  val empty : env
  (** An empty environment. *)
end

type tdenv = Type_decl_environment.env
