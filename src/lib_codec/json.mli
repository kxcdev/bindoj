(* Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

open Bindoj_base
open Typed_type_desc

type json_variant_style = [
  | `flatten
(*| `nested *)
(*| `tuple *)
]

module Config : sig
  type ('pos, 'kind) config +=
    | Config_json_name : string -> ('pos, [`json_name]) config
    | Config_json_variant_style :
      json_variant_style -> ([`variant_constructor], [`json_variant_style]) config
    | Config_json_variant_discriminator :
      string -> ([`type_decl], [`json_variant_discriminator]) config
    | Config_json_custom_encoder : string -> ([`coretype], [`json_custom_encoder]) config
    | Config_json_custom_decoder : string -> ([`coretype], [`json_custom_decoder]) config

  module Json_config : sig
    val name : string -> ([< pos], [`json_name]) config
    val get_name : string -> [< pos] configs -> string

    (**
      The default field name for the argument of a variant constructor.
      Use [Json_config.name] for [variant_constructor .. (`tuple_like ..)] to override.
    *)
    val default_name_of_variant_arg : string

    val default_variant_style : json_variant_style
    val variant_style : json_variant_style -> ([`variant_constructor], [`json_variant_style]) config
    val get_variant_style : [`variant_constructor] configs -> json_variant_style

    val default_variant_discriminator : string
    val variant_discriminator : string -> ([`type_decl], [`json_variant_discriminator]) config
    val get_variant_discriminator : [`type_decl] configs -> string

    val custom_encoder : string -> ([`coretype], [`json_custom_encoder]) config
    val get_custom_encoder : [`coretype] configs -> string option

    val custom_decoder : string -> ([`coretype], [`json_custom_decoder]) config
    val get_custom_decoder : [`coretype] configs -> string option
  end
end

include module type of Config

val of_json : env:boxed_type_decl StringMap.t -> 'a typed_type_decl -> Kxclib.Json.jv -> 'a option
val to_json : env:boxed_type_decl StringMap.t -> 'a typed_type_decl -> 'a -> Kxclib.Json.jv
