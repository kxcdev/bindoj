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

open Bindoj_typedesc.Type_desc

include module type of Bindoj_codec.Json.Config

type builtin_codec = {
  encoder: Ppxlib.expression;
  decoder: Ppxlib.expression;
  (* validator stuffs are meant to go here *)
}

module Builtin_codecs : sig
  val unit : builtin_codec
  val bool : builtin_codec
  val int : builtin_codec
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

val gen_json_encoder :
  ?self_contained:bool
  -> ?codec:Coretype.codec
  -> type_decl -> Ppxlib.value_binding
val gen_json_decoder :
  ?self_contained:bool
  -> ?codec:Coretype.codec
  -> type_decl -> Ppxlib.value_binding

val gen_openapi_schema : type_decl -> Json.jv
