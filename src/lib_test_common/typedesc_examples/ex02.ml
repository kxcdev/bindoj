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
open Bindoj_gen.Json_codec
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_gen_ts.Typescript_datatype

let kind_fname = "kind"

let decl : type_decl =
{ td_name = "person";
  td_kind =
    Variant_kind
      ([ Cstr_tuple { ct_name = "Anonymous";
                        ct_args = [];
                        ct_codec = `default_codec;
                        ct_flvconfigs = [Flvconfig_flat_kind
                                            { kind_fname=Some kind_fname; arg_fname=None; }]
              }, `nodoc;
            Cstr_tuple { ct_name = "With_id";
                        ct_args = ["int"];
                        ct_codec = `default_codec;
                        ct_flvconfigs = [Flvconfig_flat_kind
                                            { kind_fname=Some kind_fname; arg_fname=Some "arg"; }]
              }, `nodoc;
            Cstr_record { cr_name = "Student";
                          cr_fields =
                            [{ rf_name = "student_id"; rf_type = "int"; rf_codec = `default_codec; }, `nodoc;
                            { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec; }, `nodoc];
                          cr_codec = `default_codec;
                          cr_flvconfigs = [Flvconfig_flat_kind
                                            { kind_fname=Some kind_fname; arg_fname=None; }]
              }, `nodoc;
            Cstr_record { cr_name = "Teacher";
                          cr_fields =
                            [{ rf_name = "faculty_id"; rf_type = "int"; rf_codec = `default_codec; }, `nodoc;
                            { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec; }, `nodoc;
                            { rf_name = "department"; rf_type = "string"; rf_codec = `default_codec; }, `nodoc ];
                          cr_codec = `default_codec;
                          cr_flvconfigs = [Flvconfig_flat_kind
                                            { kind_fname=Some kind_fname; arg_fname=None; }]
              }, `nodoc]),
      `nodoc;
    td_flvconfigs = [];
  }

let decl_with_docstr : type_decl =
  { td_name = "person";
    td_kind =
      Variant_kind
        [ Cstr_tuple { ct_name = "Anonymous";
                        ct_args = [];
                        ct_codec = `default_codec;
                        ct_flvconfigs = [Flvconfig_flat_kind
                                          { kind_fname=Some kind_fname; arg_fname=None; }]
            }, `docstr "Anonymous constructor";
          Cstr_tuple { ct_name = "With_id";
                        ct_args = ["int"];
                        ct_codec = `default_codec;
                        ct_flvconfigs = [Flvconfig_flat_kind
                                          { kind_fname=Some kind_fname; arg_fname=Some "arg"; }]
            }, `docstr "With_id constructor";
          Cstr_record {
              cr_name = "Student";
              cr_fields = [
                  { rf_name = "student_id"; rf_type = "int"; rf_codec = `default_codec },
                  `docstr "student_id field in Student constructor";
                  { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec },
                  `docstr "name field in Student constructor";
                ];
              cr_codec = `default_codec;
              cr_flvconfigs = [Flvconfig_flat_kind { kind_fname=Some kind_fname; arg_fname=None; }];
            }, `docstr "Student constructor";
          Cstr_record {
              cr_name = "Teacher";
              cr_fields = [
                  { rf_name = "faculty_id"; rf_type = "int"; rf_codec = `default_codec },
                  `docstr "faculty_id field in Teacher constructor";
                  { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec },
                  `docstr "name field in Teacher constructor";
                  { rf_name = "department"; rf_type = "string"; rf_codec = `default_codec },
                  `docstr "dapartment field in Teacher constructor";
                ];
              cr_codec = `default_codec;
              cr_flvconfigs = [Flvconfig_flat_kind { kind_fname=Some kind_fname; arg_fname=None; }]
            }, `docstr "Teacher constructor"], `docstr "definition of person type";
    td_flvconfigs = [];
  }

let fwrt : (unit, unit) fwrt_decl =
  "person", FwrtTypeEnv.(
    init
    |> bind ~annot:() "person" []
    |> bind ~parent:"person" ~annot:() ~kind_fname "Teacher"
      [ item ~annot:() "faculty_id" ["int"];
        item ~annot:() "name" ["string"];
        item ~annot:() "department" ["string"]; ]
    |> bind ~parent:"person" ~annot:() ~kind_fname "Student"
      [ item ~annot:() "student_id" ["int"];
        item ~annot:() "name" ["string"]; ]
    |> bind ~parent:"person" ~annot:() ~kind_fname "With_id"
      [ item ~annot:() "arg" ["int"]; ]
    |> bind ~parent:"person" ~annot:() ~kind_fname "Anonymous" []
  )

let ts_ast : ts_ast option =
  let kind_fname = "kind" in
  let arg_fname = "arg" in
  let var_v = "__bindoj_v" in
  let var_x = "__bindoj_x" in
  let var_fns = "__bindoj_fns" in
  let ret = "__bindoj_ret" in
  let anonymous =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = kind_fname;
          tsps_type_desc = `literal_type (`string_literal "Anonymous"); } ] in
  let with_id =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = kind_fname;
          tsps_type_desc = `literal_type (`string_literal "With_id"); };
        { tsps_modifiers = [];
          tsps_name = arg_fname;
          tsps_type_desc = `type_reference "number"; }; ] in
  let student =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = kind_fname;
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
          tsps_name = kind_fname;
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
  let people = [anonymous; with_id; student; teacher] in
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "person";
          tsa_type_parameters = [];
          tsa_type_desc = `union people; };
      `function_declaration
        { tsf_modifiers = [`export];
          tsf_name = "analyze_person";
          tsf_type_parameters = [ret];
          tsf_parameters =
            Util.Ts_ast.(case_analyzer_parameters { kind_fname; var_x; var_v; var_fns; ret; } people);
          tsf_type_desc =
            `func_type
              { tsft_parameters =
                  [ { tsp_name = var_x;
                      tsp_type_desc = `type_reference "person"; } ];
                tsft_type_desc = `type_reference ret; };
          tsf_body =
            Util.Ts_ast.(case_analyzer_body "person" { kind_fname; var_x; var_v; var_fns; ret; } people); } ]
