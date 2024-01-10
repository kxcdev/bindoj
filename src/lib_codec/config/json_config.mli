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
(** This module provides functionalities to configure the JSON shape. *)

open Bindoj_base
open Bindoj_runtime
open Typed_type_desc

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

      {2 when a version substring is present}

      {3 major version + minor version}
      {4 type name}
      - ocaml: `version_info_v1_0`
      - typescript: `VersionInfoV1_0`
      {4 variant/kind/enum name}
      - ocaml`Version_info_v1_0`
      - json: `version-info-v1_0`
      {4 field name}
      - ocaml: `version_info_v1_0`
      - json: `versionInfoV1_0`

      {3 major version only}
      {4 type name}
      - ocaml: `version_info_v1`
      - typescript: `VersionInfoV1`
      {4 variant/kind/enum name}
      - ocaml: `Version_info_v1`
      - json: `version-info-v1`
      {4 field name}
      - ocaml: `version_info_v1`
      - json: `versionInfoV1`

      {3 with patch number}
      {4 type name}
      - ocaml: `version_info_v1_0_1`
      - typescript: `VersionInfoV1_0_1`
      {4 variant/kind/enum name}
      - ocaml: `Version_info_v1_0_1`
      - json: `version-info-v1_0_1`
      {4 field name}
      - ocaml: `version_info_v1_0_1`
      - json: `versionInfoV1_0_1`
    *)
  | `default_no_preserving_version_substring
  (** the mangling style is almost same as default, but the version substring like `v1_0` is not presrved. *)
  | `no_mangling (** do not mangle *)
]

type json_nested_field_style = [
  | `nested (** nested field stay as a nested *)
  | `spreading
  (** nested field's internal fields being spreading;
      would result in error if the field does not ultimately
      resolves to a {!Bindoj_typedesc.Type_desc.Record_decl} / {!Bindoj_typedesc.Type_desc.Variant_decl} kinded {!Bindoj_typedesc.Type_desc.type_decl} *)
  ]

type variant_arg_name_style = [
  | `exactly of string
  | `value (** ["value"], the default *)
  | `arg (** ["arg"] *)
  | `kind_name of json_mangling_style option
  (** if argument is [None], the exact same string of the kind field is used;
      otherwise, the kind name will be mangled accordingly to the supplied mangling style *)
  ]

type ('pos, 'kind) config +=
  | Config_json_name : string -> ('pos, [`json_name]) config
  | Config_json_name_of_variant_arg :
    variant_arg_name_style -> ([< `variant_constructor | `type_decl], [`json_name_of_variant_arg]) config
  | Config_json_variant_style :
    json_variant_style -> ([`variant_constructor], [`json_variant_style]) config
  | Config_json_variant_discriminator :
    string -> ([`type_decl], [`json_variant_discriminator]) config
  | Config_json_tuple_style :
    json_tuple_style -> ([< `variant_constructor | `coretype], [`json_tuple_style]) config
  | Config_json_nested_field_style : json_nested_field_style -> ([< `record_field | `variant_tuple_argument], [`json_nested_field_style]) config
  | Config_json_mangling_style : json_mangling_style -> ('pos, [`json_mangling_style]) config
  | Config_json_custom_encoder : string -> ([`coretype], [`json_custom_encoder]) config
  | Config_json_custom_decoder : string -> ([`coretype], [`json_custom_decoder]) config
  | Config_json_custom_shape_explanation : json_shape_explanation -> ([`coretype], [`json_custom_shape_explanation]) config

val tuple_index_to_field_name : int -> string

val name : string -> ([< pos], [`json_name]) config
val get_name_opt : [< pos] configs -> string option

val name_of_variant_arg : variant_arg_name_style -> ([< `variant_constructor | `type_decl], [`json_name_of_variant_arg]) config
val get_name_of_variant_arg_opt : [< `variant_constructor | `type_decl] configs -> variant_arg_name_style option

(**
  default : [ `value ].
  The default field name for the argument of a variant constructor.
  Use [Json_config.name_of_variant_arg] for [variant_constructor .. (`tuple_like ..)] to override.
*)
val default_name_of_variant_arg : variant_arg_name_style

val default_variant_style : json_variant_style
val variant_style : json_variant_style -> ([`variant_constructor], [`json_variant_style]) config
val get_variant_style : [`variant_constructor] configs -> json_variant_style

val default_variant_discriminator : string
val variant_discriminator : string -> ([`type_decl], [`json_variant_discriminator]) config
val get_variant_discriminator : [`type_decl] configs -> string

val default_tuple_style : json_tuple_style
val tuple_style : json_tuple_style -> ([< `variant_constructor | `coretype], [`json_tuple_style]) config
val get_tuple_style : [< `coretype | `variant_constructor] configs -> json_tuple_style

(** default : [ `nested ] *)
val default_nested_field_style : json_nested_field_style
val nested_field_style : json_nested_field_style -> ([< `record_field | `variant_tuple_argument], [`json_nested_field_style]) config
val get_nested_field_style : [< `record_field | `variant_tuple_argument] configs -> json_nested_field_style

(** default : [ `default true ] *)
val default_mangling_style : json_mangling_style
val mangling_style : json_mangling_style -> ([< pos], [`json_mangling_style]) config
val no_mangling : ([< pos], [`json_mangling_style]) config
val default_mangling : ([< pos], [`json_mangling_style]) config
val default_mangling_no_preserving_version_substring : ([< pos], [`json_mangling_style]) config
val get_mangling_style_opt : [< pos] configs -> json_mangling_style option

val mangled :
  [ `type_name | `field_name | `discriminator_value | `string_enum_case ]
  -> json_mangling_style -> string -> string

val get_mangled_name_of_type :
  ?inherited:json_mangling_style
  -> ?escaping_charmap:(char -> string option)
  -> type_decl -> string * json_mangling_style
val get_mangled_name_of_field :
  ?inherited:json_mangling_style
  -> record_field -> string * json_mangling_style
val get_mangled_name_of_discriminator' :
  ?inherited:json_mangling_style
  -> [`variant_constructor] configs -> string -> string * json_mangling_style
val get_mangled_name_of_discriminator :
  ?inherited:json_mangling_style
  -> variant_constructor -> string * json_mangling_style
val get_mangled_name_of_discriminator_field' :
  ?inherited:json_mangling_style
  -> [`type_decl] configs -> string
val get_mangled_name_of_discriminator_field :
  ?inherited:json_mangling_style
  -> type_decl -> string
val get_mangled_name_of_variant_arg' :
  ?inherited:json_mangling_style
  -> [`type_decl] configs
  -> [`variant_constructor] configs -> string -> string
  val get_mangled_name_of_variant_arg :
  ?inherited:json_mangling_style
  -> [`type_decl] configs
  -> variant_constructor -> string
val get_mangled_name_of_string_enum_case :
  ?inherited:json_mangling_style
  -> Coretype.string_enum_case -> string

val custom_encoder : string -> ([`coretype], [`json_custom_encoder]) config
val get_custom_encoder : [`coretype] configs -> string option

val custom_decoder : string -> ([`coretype], [`json_custom_decoder]) config
val get_custom_decoder : [`coretype] configs -> string option

val custom_shape_explanation : json_shape_explanation -> ([`coretype], [`json_custom_shape_explanation]) config
val get_custom_shape_explanation : [`coretype] configs -> json_shape_explanation option

val check_field_name_collision : type_decl -> bool

module Bindoj_private : sig
  val collect_coretypes_folder :
    ?json_tuple_style:json_tuple_style
    -> including_optional_fields:bool
    -> ([> `prim of string
        | `uninhabitable
        | `option
        | `list
        | `map
        | `ident of string
        | `string_enum
        | `self ] -> 'a option)
    -> 'a Runtime.StringMap.t
    -> Coretype.desc
    -> 'a Runtime.StringMap.t
end
