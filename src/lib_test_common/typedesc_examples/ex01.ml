(* Copyright 2022 Kotoi-Xie Consultancy, Inc.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

(* Acknowledgements - AnchorZ Inc.
The initial version or a significant portion of this file is developed
under the funding of AnchorZ Inc. to satisfy its needs in
product development. *)

open Bindoj_base.Type_desc
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_gen_ts.Typescript_datatype

let cty_int = Coretype.mk_prim `int
let cty_string = Coretype.mk_prim `string

let decl : type_decl =
  record_decl "student" [
    record_field "admission_year" cty_int;
    record_field "name" cty_string;
  ]

let decl_with_docstr : type_decl =
  record_decl "student" [
    record_field "admission_year" cty_int ~doc:(`docstr "addmission_year field");
    record_field "name" cty_string ~doc:( `docstr "name field");
  ] ~doc:(`docstr "definition of student type")

let fwrt : (unit, unit) fwrt_decl =
  "student", FwrtTypeEnv.(
    init
    |> bind_object ~annot:() "student"
      [ field ~annot:() "admission_year" cty_int;
        field ~annot:() "name" cty_string; ]
  )

let ts_ast : ts_ast option =
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "student";
          tsa_type_parameters = [];
          tsa_type_desc =
            `type_literal
              [ { tsps_modifiers = [];
                  tsps_name = "admission_year";
                  tsps_type_desc = `type_reference "number"; };
                { tsps_modifiers = [];
                  tsps_name = "name";
                  tsps_type_desc = `type_reference "string"; }; ]; } ]
