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
open Bindoj_typedesc.Type_desc
open Bindoj_runtime

include module type of Bindoj_codec.Json.Config

type json_schema
type ('tag, 'datatype_expr) foreign_language +=
  | Foreign_language_JSON_Schema :
    (json_schema, Bindoj_openapi.V3.Schema_object.t) foreign_language
val json_schema :
  (json_schema, Bindoj_openapi.V3.Schema_object.t) foreign_language

module Json_config : sig
  include module type of Bindoj_codec.Json.Config.Json_config

  val custom_json_schema :
    Bindoj_openapi.V3.Schema_object.t
    -> ([`coretype], [`foreign_type_expression]) config
end

type builtin_codec = {
  encoder: Ppxlib.expression;
  decoder: Ppxlib.expression;
  (* validator stuffs are meant to go here *)
}

module Builtin_codecs : sig
  val unit : builtin_codec
  val bool : builtin_codec
  val int : builtin_codec
  val int53p : builtin_codec
  val float : builtin_codec
  val string : builtin_codec
  val uchar : builtin_codec
  val byte : builtin_codec
  val bytes : builtin_codec
  val option : builtin_codec
  val list : builtin_codec
  val uninhabitable : builtin_codec
  val map : builtin_codec
  val all : (string * builtin_codec) list
end

val builtin_codecs : (string * builtin_codec) list

val gen_builtin_encoders :
  ?attrs:Ppxlib.attributes -> type_decl -> Ppxlib.value_binding list
val gen_builtin_decoders :
  ?attrs:Ppxlib.attributes -> type_decl -> Ppxlib.value_binding list

type json_shape_explanation_resolution =
  string (** td_name, i.e. type name *) -> [
    | `no_resolution (** resolve as [`unresolved] *)
    | `default (** resolve as [`named(type_name, type_name^"_json_shape_explanation")] *)
    | `open_ of string (** resolve as [`named(type_name, m^"."^type_name^"_json_shape_explanation")] *)
    | `in_module of string (*** resolve as [`named(type_name, m^".json_shape_explanation")] *)
    ]

val gen_json_encoder :
  ?self_contained:bool
  -> ?codec:Coretype.codec
  -> type_decl -> Ppxlib.value_binding
val gen_json_decoder_result :
  ?self_contained:bool
  -> ?json_shape_explanation_style:
    [ `inline of json_shape_explanation_resolution option
    | `reference ]
  -> ?codec:Coretype.codec
  -> type_decl -> Ppxlib.value_binding
val gen_json_decoder_option :
  ?codec:Coretype.codec
  -> type_decl -> Ppxlib.value_binding

val gen_json_shape_explanation :
  ?json_shape_explanation_resolution:json_shape_explanation_resolution
  -> ?codec:Coretype.codec
  -> type_decl -> Ppxlib.value_binding

val gen_discriminator_value_accessor :  ?codec:Coretype.codec -> type_decl -> Ppxlib.value_binding

val gen_json_codec :
  ?self_contained:bool
  -> ?gen_json_shape_explanation:bool
  -> ?discriminator_value_accessor:bool
  -> ?json_shape_explanation_resolution:json_shape_explanation_resolution
  -> ?codec:Coretype.codec
  -> type_decl -> Ppxlib.structure

val gen_json_encoder_signature : ?codec:Coretype.codec -> type_decl -> Ppxlib.value_description
val gen_json_decoder_result_signature : ?codec:Coretype.codec -> type_decl -> Ppxlib.value_description
val gen_json_decoder_option_signature : ?codec:Coretype.codec -> type_decl -> Ppxlib.value_description
val gen_json_shape_explanation_signature : ?codec:Coretype.codec -> type_decl -> Ppxlib.value_description
val gen_discriminator_value_accessor_signature : ?codec:Coretype.codec -> type_decl -> Ppxlib.value_description
val gen_json_codec_signature :
  ?gen_json_shape_explanation:bool
  -> ?discriminator_value_accessor:bool
  -> ?codec:Coretype.codec
  -> type_decl
  -> Ppxlib.signature

open Bindoj_openapi.V3

exception Incompatible_with_openapi_v3 of string

val gen_json_schema : ?openapi:bool -> type_decl -> Schema_object.t

val gen_openapi_schema : type_decl -> Schema_object.t
