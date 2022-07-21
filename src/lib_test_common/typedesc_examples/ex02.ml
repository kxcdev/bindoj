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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex02"

let discriminator = "kind"

let cty_int = Coretype.mk_prim `int
let cty_string = Coretype.mk_prim `string

let decl : type_decl =
  variant_decl "person" [
    variant_constructor "Anonymous" `no_param;
    variant_constructor "With_id" (`tuple_like [cty_int]);
    variant_constructor "Student" (`inline_record [
      record_field "student_id" cty_int;
      record_field "name" cty_string;
    ]);
    variant_constructor "Teacher" (`inline_record [
      record_field "faculty_id" cty_int;
      record_field "name" cty_string;
      record_field "department" cty_string;
    ])
  ]

let decl_with_docstr : type_decl =
  variant_decl "person" [
    variant_constructor "Anonymous" `no_param
      ~doc:(`docstr "Anonymous constructor");

    variant_constructor "With_id" (`tuple_like [cty_int])
      ~doc:(`docstr "With_id constructor");

    variant_constructor "Student" (`inline_record [
      record_field "student_id" cty_int
        ~doc:(`docstr "student_id field in Student constructor");
      record_field "name" cty_string
        ~doc:(`docstr "name field in Student constructor");
    ]) ~doc:(`docstr "Student constructor");

    variant_constructor "Teacher" (`inline_record [
      record_field "faculty_id" cty_int
        ~doc:(`docstr "faculty_id field in Teacher constructor");
      record_field "name" cty_string
        ~doc:(`docstr "name field in Teacher constructor");
      record_field "department" cty_string
        ~doc:(`docstr "dapartment field in Teacher constructor");
    ]) ~doc:(`docstr "Teacher constructor")

  ] ~doc:(`docstr "definition of person type")

let fwrt : (unit, unit) fwrt_decl =
  let parent = "person" in
  let annot = () in
  "person", FwrtTypeEnv.(
    init
    |> bind_object ~annot "person" []
    |> bind_constructor ~parent ~annot "Anonymous"
    |> bind_constructor ~parent ~annot "With_id" ~args:[cty_int]
    |> bind_constructor ~parent ~annot "Student" ~fields:[
      field ~annot "student_id" cty_int;
      field ~annot "name" cty_string]
    |> bind_constructor ~parent ~annot "Teacher" ~fields:[
      field ~annot "faculty_id" cty_int;
      field ~annot "name" cty_string;
      field ~annot "department" cty_string]
  )

let ts_ast : ts_ast option =
  let discriminator = "kind" in
  let arg_fname = "arg" in
  let var_v = "__bindoj_v" in
  let var_x = "__bindoj_x" in
  let var_fns = "__bindoj_fns" in
  let ret = "__bindoj_ret" in
  let anonymous =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "Anonymous"); } ] in
  let with_id =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "With_id"); };
        { tsps_modifiers = [];
          tsps_name = arg_fname;
          tsps_type_desc = `type_reference "number"; }; ] in
  let student =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "Student"); };
        { tsps_modifiers = [];
          tsps_name = "student_id";
          tsps_type_desc = `type_reference "number"; };
        { tsps_modifiers = [];
          tsps_name = "name";
          tsps_type_desc = `type_reference "string"; } ] in
  let teacher =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "Teacher"); };
        { tsps_modifiers = [];
          tsps_name = "faculty_id";
          tsps_type_desc = `type_reference "number"; };
        { tsps_modifiers = [];
          tsps_name = "name";
          tsps_type_desc = `type_reference "string"; };
        { tsps_modifiers = [];
          tsps_name = "department";
          tsps_type_desc = `type_reference "string"; } ] in
  let person = [
    "With_id", with_id;
    "Teacher", teacher;
    "Student", student;
    "Anonymous", anonymous;
  ] in
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "person";
          tsa_type_parameters = [];
          tsa_type_desc = `union (List.map snd person); };
      `function_declaration
        { tsf_modifiers = [`export];
          tsf_name = "analyze_person";
          tsf_type_parameters = [ret];
          tsf_parameters =
            Util.Ts_ast.(case_analyzer_parameters { discriminator; var_x; var_v; var_fns; ret; } person);
          tsf_type_desc =
            `func_type
              { tsft_parameters =
                  [ { tsp_name = var_x;
                      tsp_type_desc = `type_reference "person"; } ];
                tsft_type_desc = `type_reference ret; };
          tsf_body =
            Util.Ts_ast.(case_analyzer_body "person" { discriminator; var_x; var_v; var_fns; ret; } person); } ]
