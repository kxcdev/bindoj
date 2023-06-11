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
open Bindoj_base.Type_desc

val type_declaration_of_type_decl :
  ?type_name:string
  -> ?attrs:Ppxlib.attribute list
  -> type_decl
  -> Ppxlib.type_declaration

val gen_reflect :
  ?codec:Coretype.codec
  -> type_decl
  -> Ppxlib.value_binding

(**
  generate the whole structure containing the results of [type_declaration_of_type_decl] and [gen_reflect].

  @param refl if [true], a field [type_name_reflect] will be generated.
              if additionally [?type_decl] is given, then a field [typed_decl]
              will also be generated.

  @param generators additional generators e.g. [Json_codec.gen_json_encoder].

  @param type_decl the expression of / the path to the [type_decl] value.
  If present, [val <type_name>_decl] and [val <type_name>_typed_decl] are also generated.
  If [codec] is [`in_module _], [val decl] and [val typed_decl] are generated instead.
*)
val gen_structure :
  ?type_name:string
  -> ?refl:bool
  -> ?attrs:Ppxlib.attribute list
  -> ?codec:Coretype.codec
  -> ?generators:(?codec:Coretype.codec -> type_decl -> Ppxlib.structure) list
  -> ?type_decl:[`path of string | `expr of Ppxlib.expression]
  -> type_decl -> Ppxlib.structure

val gen_reflect_signature :
  ?refl_type_abbr:string
  -> ?codec:Coretype.codec
  -> type_decl
  -> Ppxlib.value_description

(**
  generate the whole signature containing the results of [type_declaration_of_type_decl] and [gen_reflect_signature].

  @param refl if [true], a field [type_name_reflect] will be generated.
              if additionally [?type_decl] is given, then a field [typed_decl]
              will also be generated.

  @param generators additional generators e.g. [Json_codec.gen_json_encoder_signature].

  @param type_decl the expression of / the path to the [type_decl] value.
  If present, [val <type_name>_decl] and [val <type_name>_typed_decl] are also generated.
  If [codec] is [`in_module _], [val decl] and [val typed_decl] are generated instead.
*)
val gen_signature :
  ?type_name:string
  -> ?refl:bool
  -> ?attrs:Ppxlib.attribute list
  -> ?codec:Coretype.codec
  -> ?generators:(?codec:Coretype.codec -> type_decl -> Ppxlib.signature) list
  -> ?type_decl:bool
  -> ?refl_type_abbr:string
  -> ?type_decl_type_abbr:string
  -> ?typed_type_decl_type_abbr:string
  -> type_decl -> Ppxlib.signature
