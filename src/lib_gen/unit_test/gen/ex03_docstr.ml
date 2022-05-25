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

module Ex = Bindoj_test_common_typedesc_examples.Ex03
open Bindoj_base.Type_desc
open Bindoj_gen.Caml_datatype
open Bindoj_gen.Json_codec

let name = "ex03_docstr"

let gen () =
  let open Ppxlib in
  let open Ast_builder.Default in
  let loc = Location.none in
  Astlib.Pprintast.structure Format.std_formatter [
    (pstr_type ~loc Recursive [type_declaration_of_type_decl ~show:true Ex.decl_with_docstr]);
    (pstr_value ~loc Recursive
       [gen_json_encoder ~self_contained:true Ex.decl_with_docstr]);
    (pstr_value ~loc Recursive
       [gen_json_decoder ~self_contained:true Ex.decl_with_docstr]);
  ]
