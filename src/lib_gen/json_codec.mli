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
(** This module provides functions to generate OCaml values and functions. *)
open Bindoj_typedesc.Type_desc

module Json_config = Bindoj_gen_config.Json_config

(* include module type of Bindoj_codec.Json.Config
 *)
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
(** Returns builtin encoders of the given type declaration. *)

val gen_builtin_decoders :
  ?attrs:Ppxlib.attributes -> type_decl -> Ppxlib.value_binding list
(** Returns builtin decoders of the given type declaration. *)

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
(** Generates a json encoder function of the given type declaration.
    @param ?self_contained If [true], generates builtin encoders.
    @param ?codec
    If [codec] is [`default], [val <type_name>_to_json] is generated.
    If [codec] is [`in_module _], [val to_json] is generated.
    @param type_decl Type declaration to be generated.
    @return value_binding of a encoder function. *)

val gen_json_decoder_result :
  ?self_contained:bool
  -> ?json_shape_explanation_style:
    [ `inline of json_shape_explanation_resolution option
    | `reference ]
  -> ?codec:Coretype.codec
  -> type_decl -> Ppxlib.value_binding
(** Generates a json decoder function of the given type declaration.
    The generated function has type [?path:jvpath -> jv -> t OfJsonResult.t].
    @param ?self_contained If [true], generates builtin decoders.
    @param ?codec
    If [codec] is [`default], [val <type_name>_of_json'] is generated.
    If [codec] is [`in_module _], [val of_json'] is generated.
    @param type_decl Type declaration to be generated.
    @return value_binding of a decoder function. *)

val gen_json_decoder_option :
  ?implementation_style: [
    | `refer_existing_result_variant_json_decoder
      (** generate a reference to an already defined JSON decoder that returns a result type *)
    | `embed_full_implementation of [
      | `self_contained (** generates builtin decoders. *)
      | `non_self_contained
    ] (** embed a full JSON decoder implementation.
          see also [?self_contained:bool] on {!gen_json_encoder} *)
  ]
  -> ?codec:Coretype.codec
  -> type_decl -> Ppxlib.value_binding
(** Generates a json decoder function of the given type declaration.
    The generated function has type [jv -> t option].
    @param ?implementation_style Specifies how the generated functions are implemented.
    @param ?codec
    If [codec] is [`default], [val <type_name>_of_json] is generated.
    If [codec] is [`in_module _], [val of_json] is generated.
    @param type_decl Type declaration to be generated.
    @return value_binding of a decoder function. *)

val gen_json_shape_explanation :
  ?json_shape_explanation_resolution:json_shape_explanation_resolution
  -> ?codec:Coretype.codec
  -> type_decl -> Ppxlib.value_binding
(** Generates a value_binding of {!type-Bindoj_runtime.json_shape_explanation}.
    @param ?json_shape_explanation_resolution How ident is resolved.
    @param ?codec
    If [codec] is [`default], [val <type_name>_json_shape_explanation] is generated.
    If [codec] is [`in_module _], [val json_shape_explanation] is generated.
    @param type_decl Type declaration to be generated.
    @return value_binding of json_shape_explanation. *)

val gen_discriminator_value_accessor : ?codec:Coretype.codec -> type_decl -> Ppxlib.value_binding
(** Generates a function of the given type declaration of {!Bindoj_typedesc.Type_desc.Variant_decl}.
    The generated has type [t -> string].
    @param ?codec
    If [codec] is [`default], [val <type_name>_json_discriminator_value] is generated.
    If [codec] is [`in_module _], [val json_discriminator_value] is generated.
    @param type_decl Type declaration to be generated.
    @return value_binding of a discriminator accessor function. *)

val gen_json_codec :
  ?self_contained:bool
  -> ?gen_json_shape_explanation:bool
  -> ?discriminator_value_accessor:bool
  -> ?json_shape_explanation_resolution:json_shape_explanation_resolution
  -> ?codec:Coretype.codec
  -> type_decl -> Ppxlib.structure
(** Generates the whole structure containing the result of
    {!gen_json_encoder}, {!gen_json_decoder_result}, {!gen_json_decoder_option},
    {!gen_json_shape_explanation} and {!gen_discriminator_value_accessor}.
    @param ?self_contained If [true], generates builtin encoders/decoders in encoder/decoder functions.
    @param ?gen_json_shape_explanation If present, [val <type_name>_json_shape_explanation] is also generated.
    @param ?discriminator_value_accessor
    If [true] and the given [type_decl] is [Variant_decl], [val <type_name>_json_discriminator_value] is also generated.
    @param ?json_shape_explanation_resolution How ident is resolved when generating [json_shape_explanation].
    @param ?codec
    If [codec] is [`default], [type t] and [val <type_name>_<value_name>] are generated.
    If [codec] is [`in_module _], [type <type_name>] and [val <value_name>] are generated.
    @param type_decl type declaration to be generated.
    @return Ppxlib.structure. *)

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
(** Generates the whole signature containing the result of
    {!gen_json_encoder_signature}, {!gen_json_decoder_result_signature}, {!gen_json_decoder_option_signature},
    {!gen_json_shape_explanation_signature} and {!gen_discriminator_value_accessor_signature}.
    @param ?gen_json_shape_explanation If present, [val <type_name>_json_shape_explanation] is also generated.
    @param ?discriminator_value_accessor
    If [true] and the given [type_decl] is [Variant_decl], [val <type_name>_json_discriminator_value] is also generated.
    @param ?codec
    If [codec] is [`default], [type t] and [val <type_name>_<value_name>] are generated.
    If [codec] is [`in_module _], [type <type_name>] and [val <value_name>] are generated.
    @param type_decl type declaration to be generated.
    @return Ppxlib.structure. *)

open Bindoj_openapi.V3

exception Incompatible_with_openapi_v3 of string

val gen_json_schema : ?openapi:bool -> type_decl -> Schema_object.t
(** Generates json schema object. *)

val gen_openapi_schema : type_decl -> Schema_object.t
(** Generates a json schema object of OpenAPI. *)
