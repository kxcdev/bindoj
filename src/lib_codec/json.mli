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
(** This module provides functions for interconversion between JSON values and OCaml values. *)
open Bindoj_base
open Bindoj_runtime
open Typed_type_desc
open Kxclib.Json

module Json_config = Bindoj_codec_config.Json_config

val get_json_discriminator_value : 'a typed_type_decl -> 'a -> string

val validate_spreading_type :
  [< `direct of coretype | `nested of type_decl * Coretype.codec]
  -> [`record_decl of type_decl * record_field list * Coretype.codec
      | `variant_decl of type_decl * variant_constructor list * Coretype.codec ]

val explain_encoded_json_shape'
  : ('shape, 'field_shape) json_shape_explaner
  -> ((type_decl -> 'shape)
  -> Coretype.ident -> string -> 'shape)
  -> type_decl -> 'shape
(** Creates a json shape explanation of the give typed type declaration with the given json shape explaner. *)

val explain_encoded_json_shape : env:tdenv -> 't typed_type_decl -> json_shape_explanation
(** Creates a json shape explanation of the give typed type declaration. *)

val of_json' : env:tdenv -> 'a typed_type_decl -> jv -> 'a OfJsonResult.t
(** Converts the provided JSON value to the corresponding OCaml value, if possible.
    @param env Environment where the type declarations are added.
    @param typed_type_decl A typed type declaration that specifies how to convert the value.
    @param jv JSON value to be converted.
    @return The converted OCaml value encapsulated in an OfJsonResult type. *)

val of_json : env:tdenv -> 'a typed_type_decl -> jv -> 'a option
(** Converts the provided JSON value to the corresponding OCaml value.
    This function acts same as {!of_json'}.
    @param env Environment where the type declarations are added.
    @param typed_type_decl A typed type declaration that specifies how to convert the value.
    @param jv JSON value to be converted.
    @return The converted OCaml value if possible, else [None]. *)

val to_json : env:tdenv -> 'a typed_type_decl -> 'a -> jv
(** Converts an OCaml value to a JSON value.
    @param env Environment where the type declarations are added.
    @param typed_type_decl A typed type declaration that specifies how to convert the value.
    @param 'a OCaml value to be converted.
    @return The converted JSON value. *)
