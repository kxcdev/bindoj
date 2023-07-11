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
(** This module provides functions to generate OCaml code. *)
open Bindoj_typedesc.Type_desc

val gen_structure_with_json_codec :
  ?self_contained:bool
  -> ?gen_json_shape_explanation:bool
  -> ?discriminator_value_accessor:bool
  -> ?json_shape_explanation_resolution: Json_codec.json_shape_explanation_resolution
  -> ?codec:Coretype.codec
  -> ?type_decl:[ `expr of Ppxlib.expression | `path of string ]
  -> formatter:ppf
  -> type_decl
  -> unit
(** Writes the OCaml code of the structures of {!Caml_datatype.gen_structure} and {!Json_codec.gen_json_codec}.
    @param ?self_contained If [true], generates builtin encoders/decoders in encoder/decoder functions.
    @param ?gen_json_shape_explanation If present, [val <type_name>_json_shape_explanation] is also generated.
    @param ?discriminator_value_accessor
    If [true] and the given [type_decl] is [Variant_decl], [val <type_name>_json_discriminator_value] is also generated.
    @param ?json_shape_explanation_resolution How ident is resolved when generating [json_shape_explanation].
    @param ?codec
    If [codec] is [`default], [type t] and [val <type_name>_<value_name>] are generated.
    If [codec] is [`in_module _], [type <type_name>] and [val <value_name>] are generated.
    @param ?type_decl the expression of / the path to the [type_decl] value.
    If present, [val <type_name>_decl] and [val <type_name>_typed_decl] are also generated.
    @param formatter Formatter in which the results are written.
    @param type_decl type declaration to be generated. *)

val gen_signature_with_json_codec :
  ?gen_json_shape_explanation:bool
  -> ?discriminator_value_accessor:bool
  -> ?codec:Coretype.codec
  -> gen_type_decl:bool
  -> formatter:ppf
  -> type_decl
  -> unit
(** Writes the OCaml code of the signatures of {!Caml_datatype.gen_signature} and {!Json_codec.gen_json_codec_signature}.
    @param ?gen_json_shape_explanation If present, [val <type_name>_json_shape_explanation] is also generated.
    @param ?discriminator_value_accessor
    If [true] and the given [type_decl] is [Variant_decl], [val <type_name>_json_discriminator_value] is also generated.
    @param ?codec
    If [codec] is [`default], [type t] and [val <type_name>_<value_name>] are generated.
    If [codec] is [`in_module _], [type <type_name>] and [val <value_name>] are generated.
    @param ?gen_type_decl If [true], [val <type_name>_decl] and [val <type_name>_typed_decl] are also generated.
    @param formatter Formatter in which the results are written.
    @param type_decl type declaration to be generated. *)
