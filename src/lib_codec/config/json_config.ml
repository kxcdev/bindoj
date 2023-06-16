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
open Typed_type_desc
open Runtime

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
  | `no_mangling
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

let tuple_index_to_field_name i = "_" ^ string_of_int i

let name name = Config_json_name name

let get_name_opt configs =
  Configs.find (function
    | Config_json_name s -> Some s
    | _ -> None
  ) configs

let name_of_variant_arg name = Config_json_name_of_variant_arg name

let get_name_of_variant_arg default =
  Configs.find_or_default ~default (function
    | Config_json_name_of_variant_arg s -> Some s
    | _ -> None
  )
let default_name_of_variant_arg = "arg"

let default_variant_style : json_variant_style = `flatten
let variant_style style = Config_json_variant_style style
let get_variant_style =
  Configs.find_or_default ~default:default_variant_style (function
    | Config_json_variant_style s -> Some s
    | _ -> None
  )

let default_variant_discriminator = "kind"
let variant_discriminator discriminator = Config_json_variant_discriminator discriminator
let get_variant_discriminator =
  Configs.find_or_default ~default:default_variant_discriminator (function
    | Config_json_variant_discriminator s -> Some s
    | _ -> None
  )

let default_tuple_style = `arr
let tuple_style style = Config_json_tuple_style style
let get_tuple_style configs =
  Configs.find_or_default ~default:default_tuple_style (function
    | Config_json_tuple_style s -> Some s
    | _ -> None
  ) configs

let default_mangling_style = `default
let mangling_style style = Config_json_mangling_style style
let no_mangling = Config_json_mangling_style `no_mangling
let default_mangling = Config_json_mangling_style `default

let get_mangling_style_opt configs =
  Configs.find (function
    | Config_json_mangling_style s -> Some s
    | _ -> None) configs

let mangled kind mangling_style name =
  match mangling_style with
  | `no_mangling -> name
  | `default ->
    let open Bindoj_common.Mangling in
    name |> (match kind with
    | `type_name -> snake_to_upper_camel
    | `field_name -> snake_to_lower_camel
    | `discriminator_value -> cap_snake_to_kebab
    | `string_enum_case -> snake_to_kebab)

let get_mangled_name_of_type : ?inherited:json_mangling_style -> type_decl -> string * json_mangling_style =
  fun ?(inherited=default_mangling_style) { td_name; td_configs; _ } ->
    let style = get_mangling_style_opt td_configs |? inherited in
    td_configs |> get_name_opt |? td_name
    |> mangled `type_name style, style

let get_mangled_name_of_field : ?inherited:json_mangling_style -> record_field -> string * json_mangling_style =
  fun ?(inherited=default_mangling_style) { rf_name; rf_configs; _ } ->
    let style = get_mangling_style_opt rf_configs |? inherited in
    rf_configs |> get_name_opt |? rf_name
    |> mangled `field_name style, style

let get_mangled_name_of_discriminator : ?inherited:json_mangling_style -> variant_constructor -> string * json_mangling_style =
  fun ?(inherited=default_mangling_style) { vc_name; vc_configs; _ } ->
    let style = get_mangling_style_opt vc_configs |? inherited in
    vc_configs |> get_name_opt |? vc_name
    |> mangled `discriminator_value style, style

let get_mangled_name_of_string_enum_case : ?inherited:json_mangling_style -> Coretype.string_enum_case -> string =
  fun ?(inherited=default_mangling_style) (name, configs, _) ->
    let style = get_mangling_style_opt configs |? inherited in
    configs |> get_name_opt |? name
    |> mangled `string_enum_case style

let custom_encoder encoder_name = Config_json_custom_encoder encoder_name
let get_custom_encoder =
  Configs.find (function | Config_json_custom_encoder s -> Some s | _ -> None)

let custom_decoder decoder_name = Config_json_custom_decoder decoder_name
let get_custom_decoder =
  Configs.find (function | Config_json_custom_decoder s -> Some s | _ -> None)

let custom_shape_explanation json_shape_explanation = Config_json_custom_shape_explanation json_shape_explanation
let get_custom_shape_explanation =
  Configs.find (function | Config_json_custom_shape_explanation s -> Some s | _ -> None)
