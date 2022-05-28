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

open Ppxlib
open Ast_helper
open Bindoj_base.Type_desc
open Bindoj_gen.Caml_datatype

let gen_with_json_codec ?(recursive = false) ?self_contained ?flavor ?codec decl =
  let open Bindoj_gen.Json_codec in
  let recursive = if recursive then Recursive else Nonrecursive in
  Astlib.Pprintast.structure Format.std_formatter [
    Str.type_ Recursive [type_declaration_of_type_decl ~show:true decl];
    Str.value recursive [gen_json_encoder ?self_contained ?flavor ?codec decl];
    Str.value recursive [gen_json_decoder ?self_contained ?flavor ?codec decl];
  ]
