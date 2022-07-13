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

  @param generators additional generators e.g. [Json_codec.gen_json_encoder].

  @param type_decl the expression of / the path to the [type_decl] value.
  If present, [val <type_name>_decl] and [val <type_name>_typed_decl] are also generated.
  If [codec] is [`in_module _], [val decl] and [val typed_decl] are generated instead.
*)
val gen_structure :
  ?type_name:string
  -> ?attrs:Ppxlib.attribute list
  -> ?codec:Coretype.codec
  -> ?generators:(?codec:Coretype.codec -> type_decl -> Ppxlib.value_binding) list
  -> ?type_decl:[`path of string | `expr of Ppxlib.expression]
  -> type_decl -> Ppxlib.structure
