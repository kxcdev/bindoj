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
  | `default_no_preserving_version_substring
  | `no_mangling
]

type json_nested_field_style = [
  | `nested
  | `spreading
  ]
(** default : [ `nested ] *)

type variant_arg_name_style = [
  | `exactly of string
  | `value
  | `arg
  | `kind_name of json_mangling_style option
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

let tuple_index_to_field_name i = "_" ^ string_of_int i

let name name = Config_json_name name

let get_name_opt configs =
  Configs.find (function
    | Config_json_name s -> Some s
    | _ -> None
  ) configs

let name_of_variant_arg name = Config_json_name_of_variant_arg name

let get_name_of_variant_arg_opt configs =
  Configs.find (function
    | Config_json_name_of_variant_arg s -> Some s
    | _ -> None
  ) configs

let default_name_of_variant_arg : variant_arg_name_style = `value

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

let default_nested_field_style = `nested
let nested_field_style style = Config_json_nested_field_style style
let get_nested_field_style configs =
  Configs.find_or_default ~default:default_nested_field_style (function
    | Config_json_nested_field_style s -> Some s
    | _ -> None) configs

let default_mangling_style = `default
let mangling_style style = Config_json_mangling_style style
let no_mangling = Config_json_mangling_style `no_mangling
let default_mangling = Config_json_mangling_style `default
let default_mangling_no_preserving_version_substring = Config_json_mangling_style `default_no_preserving_version_substring

let get_mangling_style_opt configs =
  Configs.find (function
    | Config_json_mangling_style s -> Some s
    | _ -> None) configs

let mangled kind mangling_style name =
  match mangling_style with
  | `no_mangling -> name
  | `default | `default_no_preserving_version_substring ->
    let preserve_version_substring =
      mangling_style = `default
    in
    let open Bindoj_common.Mangling in
    let mangler = match kind with
      | `type_name -> snake_to_upper_camel
      | `field_name -> snake_to_lower_camel
      | `discriminator_value -> cap_snake_to_kebab
      | `string_enum_case -> snake_to_kebab in
    mangler ~preserve_version_substring name

let get_mangled_name_of_type : ?inherited:json_mangling_style -> type_decl -> string * json_mangling_style =
  fun ?(inherited=default_mangling_style) { td_name; td_configs; _ } ->
    let style = get_mangling_style_opt td_configs |? inherited in
    td_configs |> get_name_opt |? (mangled `type_name style td_name)
    , style

let get_mangled_name_of_field : ?inherited:json_mangling_style -> record_field -> string * json_mangling_style =
  fun ?(inherited=default_mangling_style) { rf_name; rf_configs; _ } ->
    let style = get_mangling_style_opt rf_configs |? inherited in
    rf_configs |> get_name_opt |? (mangled `field_name style rf_name)
    , style

let get_mangled_name_of_discriminator' =
  fun ?(inherited=default_mangling_style) vc_configs vc_name ->
    let style = get_mangling_style_opt vc_configs |? inherited in
    vc_configs |> get_name_opt |? (mangled `discriminator_value style vc_name)
    , style

let get_mangled_name_of_discriminator : ?inherited:json_mangling_style -> variant_constructor -> string * json_mangling_style =
  fun ?inherited { vc_name; vc_configs; _ } ->
    get_mangled_name_of_discriminator' ?inherited vc_configs vc_name

let get_mangled_name_of_discriminator_field': ?inherited:json_mangling_style -> [`type_decl] configs -> string =
  fun ?(inherited=default_mangling_style) td_configs ->
    let style = get_mangling_style_opt td_configs |? inherited in
    td_configs |> get_variant_discriminator
    |> mangled `field_name style

let get_mangled_name_of_discriminator_field =
  fun ?inherited { td_configs; _ } ->
    get_mangled_name_of_discriminator_field' ?inherited td_configs

let get_mangled_name_of_variant_arg' =
  fun ?(inherited=default_mangling_style) td_configs vc_configs vc_name ->
    let mangling_style = get_mangling_style_opt vc_configs |? inherited in
    get_name_of_variant_arg_opt vc_configs
    ||?! (fun () -> get_name_of_variant_arg_opt td_configs)
    |? default_name_of_variant_arg
    |> function
    | `exactly name -> name
    | `value -> "value"
    | `arg -> "arg"
    | `kind_name style ->
      get_mangled_name_of_discriminator' ~inherited:(style |? mangling_style) vc_configs vc_name
      |> fst

let get_mangled_name_of_variant_arg =
  fun ?inherited td_configs { vc_name; vc_configs; _ } ->
    get_mangled_name_of_variant_arg' ?inherited td_configs vc_configs vc_name

let get_mangled_name_of_string_enum_case : ?inherited:json_mangling_style -> Coretype.string_enum_case -> string =
  fun ?(inherited=default_mangling_style) (name, configs, _) ->
    let style = get_mangling_style_opt configs |? inherited in
    configs |> get_name_opt |? (mangled `string_enum_case style name)

let custom_encoder encoder_name = Config_json_custom_encoder encoder_name
let get_custom_encoder =
  Configs.find (function | Config_json_custom_encoder s -> Some s | _ -> None)

let custom_decoder decoder_name = Config_json_custom_decoder decoder_name
let get_custom_decoder =
  Configs.find (function | Config_json_custom_decoder s -> Some s | _ -> None)

let custom_shape_explanation json_shape_explanation = Config_json_custom_shape_explanation json_shape_explanation
let get_custom_shape_explanation =
  Configs.find (function | Config_json_custom_shape_explanation s -> Some s | _ -> None)

let check_field_name_collision =
  let add_field name fs =
    if List.mem name fs then None
    else Some(name :: fs)
  in
  let validated f = function
  | `direct _ -> failwith "non-nested argument/field cannot be spread."
  | `nested ({ td_kind = Alias_decl _; _ }, _) -> failwith "Alias decl cannot be spread."
  | `nested (td, _) -> f td
  in
  let open MonadOps(Option) in
  let rec go_fields
    base_mangling_style
    (fs: string list list option) fields: string list list option =
    List.fold_left (fun fs field ->
      let json_field_name, _ =
        get_mangled_name_of_field
          ~inherited:base_mangling_style
          field in
      match get_nested_field_style field.rf_configs with
      | `nested -> fs >>= (List.map (add_field json_field_name) &> sequence_list)
      | `spreading ->
        validated (fun td -> go default_mangling_style td fs) field.rf_type
    ) fs fields
  and go :
    json_mangling_style
    -> type_decl
    -> string list list option
    -> string list list option =
    fun
      base_mangling_style
      ({ td_configs; td_kind; _ } as td)
      (fs: string list list option) ->
    let base_mangling_style = get_mangling_style_opt td_configs |? base_mangling_style in
    match td_kind with
    | Alias_decl _ -> fs
    | Record_decl fields -> go_fields base_mangling_style fs fields
    | Variant_decl ctors ->
      let discriminator_fname =
        get_mangled_name_of_discriminator_field
          ~inherited:base_mangling_style
          td in
      let fs = fs >>= (List.map (add_field discriminator_fname) &> sequence_list) in
      ctors |&> (fun ctor ->
        let base_mangling_style = get_mangling_style_opt ctor.vc_configs |? base_mangling_style in
        match ctor.vc_param with
        | `inline_record fields | `reused_inline_record { td_kind = Record_decl fields; _ } ->
          go_fields base_mangling_style fs fields
        | `reused_inline_record _ ->
          failwith' "type decl of reused inline record '%s' must be record decl." ctor.vc_name
        | `no_param | `tuple_like [] -> fs
        | `tuple_like [ va ] when get_nested_field_style va.va_configs = `spreading ->
          va.va_type |> validated (fun td ->
            let base_mangling_style = get_mangling_style_opt va.va_configs |? default_mangling_style in
            go base_mangling_style td fs)
        | `tuple_like _ ->
          let arg_fname =
            get_mangled_name_of_variant_arg
              ~inherited:base_mangling_style
              td_configs
              ctor
          in
          fs >>? (List.map (add_field arg_fname) &> sequence_list)
      )
      |> sequence_list
      >|= List.concat
  in
  fun td -> go default_mangling_style td (Some []) |> Option.is_some
