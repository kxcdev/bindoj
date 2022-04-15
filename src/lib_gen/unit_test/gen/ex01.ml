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

let ex01 : type_decl =
  { td_name = "student";
    td_kind =
      Record_kind
        ([{ rf_name = "admission_year"; rf_type = "int"; rf_codec = `default_codec }, `nodoc;
          { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec }, `nodoc;]),
      `nodoc; }

let () =
  let open Ppxlib in
  let open Ast_builder.Default in
  let loc = Location.none in
  Astlib.Pprintast.structure Format.std_formatter [
    (pstr_type ~loc Recursive [type_declaration_of_type_decl ex01]);
    (pstr_value ~loc Nonrecursive
       [gen_json_encoder ~self_contained:true ex01]);
    (pstr_value ~loc Nonrecursive
       [gen_json_decoder ~self_contained:true ex01]);
  ]
