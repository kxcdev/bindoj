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
open Bindoj_base
open Bindoj_runtime
open Typed_type_desc

module Config : sig
  type json_variant_style = [
    | `flatten
  (*| `nested *)
  (*| `tuple *)
  ]

  type json_tuple_style = [
    | `arr
    | `obj of [`default]
  ]

  type json_mangling_style = [
    | `default
    (** the default mangling style mangles as the following

        {2 type name}
        snake_case to UpperCamelCase, e.g.
        - ocaml: `some_type_name`
        - typescript: `SomeTypeName`

        {2 field name}
        snake_case to lowerCamelCase, e.g.
        - ocaml: `some_field_foo`
        - json: `someFieldFoo`

        {2 variant name}
        Capitalized_snake_case to kebab-case
        - ocaml: `Some_branch_foo`
        - json: `some-branch-foo`
      *)
    | `no_mangling (** do not mangle *)
  ]

  type ('pos, 'kind) config +=
    | Config_json_name : string -> ('pos, [`json_name]) config
    | Config_json_name_of_variant_arg : string -> ([`variant_constructor], [`json_name_of_variant_arg]) config
    | Config_json_variant_style :
      json_variant_style -> ([`variant_constructor], [`json_variant_style]) config
    | Config_json_variant_discriminator :
      string -> ([`type_decl], [`json_variant_discriminator]) config
    | Config_json_tuple_style :
      json_tuple_style -> ([< `variant_constructor | `coretype], [`json_tuple_style]) config
    | Config_json_mangling_style : json_mangling_style -> ('pos, [`json_mangling_style]) config
    | Config_json_custom_encoder : string -> ([`coretype], [`json_custom_encoder]) config
    | Config_json_custom_decoder : string -> ([`coretype], [`json_custom_decoder]) config
    | Config_json_custom_shape_explanation : json_shape_explanation -> ([`coretype], [`json_custom_shape_explanation]) config

  val tuple_index_to_field_name : int -> string

  module Json_config : sig
    val name : string -> ([< pos], [`json_name]) config
    val get_name_opt : [< pos] configs -> string option

    val name_of_variant_arg : string -> ([`variant_constructor], [`json_name_of_variant_arg]) config
    val get_name_of_variant_arg : string -> [`variant_constructor] configs -> string

    (**
      The default field name for the argument of a variant constructor.
      Use [Json_config.name_of_variant_arg] for [variant_constructor .. (`tuple_like ..)] to override.
    *)
    val default_name_of_variant_arg : string

    val default_variant_style : json_variant_style
    val variant_style : json_variant_style -> ([`variant_constructor], [`json_variant_style]) config
    val get_variant_style : [`variant_constructor] configs -> json_variant_style

    val default_variant_discriminator : string
    val variant_discriminator : string -> ([`type_decl], [`json_variant_discriminator]) config
    val get_variant_discriminator : [`type_decl] configs -> string

    val default_tuple_style : json_tuple_style
    val tuple_style : json_tuple_style -> ([< `variant_constructor | `coretype], [`json_tuple_style]) config
    val get_tuple_style : [< `coretype | `variant_constructor] configs -> json_tuple_style

    val default_mangling_style : json_mangling_style
    val mangling_style : json_mangling_style -> ([< pos], [`json_mangling_style]) config
    val no_mangling : ([< pos], [`json_mangling_style]) config
    val default_mangling : ([< pos], [`json_mangling_style]) config
    val get_mangling_style : [< pos] configs -> json_mangling_style

    val mangled : [ `type_name | `field_name | `discriminator_value ] -> [< pos] configs -> string -> string

    val get_mangled_name_of_type : type_decl -> string
    val get_mangled_name_of_field : [`type_decl] configs -> record_field -> string
    val get_mangled_name_of_discriminator : [`type_decl] configs -> variant_constructor -> string

    val custom_encoder : string -> ([`coretype], [`json_custom_encoder]) config
    val get_custom_encoder : [`coretype] configs -> string option

    val custom_decoder : string -> ([`coretype], [`json_custom_decoder]) config
    val get_custom_decoder : [`coretype] configs -> string option

    val custom_shape_explanation : json_shape_explanation -> ([`coretype], [`json_custom_shape_explanation]) config
    val get_custom_shape_explanation : [`coretype] configs -> json_shape_explanation option
  end
end

include module type of Config

open Kxclib.Json

val get_json_discriminator_value : 'a typed_type_decl -> 'a -> string

val explain_encoded_json_shape : env:tdenv -> 't typed_type_decl -> json_shape_explanation

val of_json' : env:tdenv -> 'a typed_type_decl -> jv -> 'a OfJsonResult.t
val of_json : env:tdenv -> 'a typed_type_decl -> jv -> 'a option
val to_json : env:tdenv -> 'a typed_type_decl -> 'a -> jv
