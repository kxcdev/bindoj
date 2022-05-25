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

let decl : type_decl =
  { td_name = "person";
    td_kind =
      Variant_kind
        ([ Cstr_tuple { ct_name = "Anonymous";
                        ct_args = [];
                        ct_codec = `default_codec;
                        ct_flvconfigs = [Flvconfig_flat_kind
                                            { kind_fname=Some "kind"; arg_fname=None; }]
              }, `nodoc;
            Cstr_tuple { ct_name = "With_id";
                        ct_args = ["int"];
                        ct_codec = `default_codec;
                        ct_flvconfigs = [Flvconfig_flat_kind
                                            { kind_fname=Some "kind"; arg_fname=Some "arg"; }]
              }, `nodoc;
            Cstr_record { cr_name = "Student";
                          cr_fields =
                            [{ rf_name = "student_id"; rf_type = "int"; rf_codec = `default_codec; }, `nodoc;
                            { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec; }, `nodoc];
                          cr_codec = `default_codec;
                          cr_flvconfigs = [Flvconfig_flat_kind
                                            { kind_fname=Some "kind"; arg_fname=None; }]
              }, `nodoc;
            Cstr_record { cr_name = "Teacher";
                          cr_fields =
                            [{ rf_name = "faculty_id"; rf_type = "int"; rf_codec = `default_codec; }, `nodoc;
                            { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec; }, `nodoc;
                            { rf_name = "department"; rf_type = "string"; rf_codec = `default_codec; }, `nodoc ];
                          cr_codec = `default_codec;
                          cr_flvconfigs = [Flvconfig_flat_kind
                                            { kind_fname=Some "kind"; arg_fname=None; }]
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
                                          { kind_fname=Some "kind"; arg_fname=None; }]
            }, `docstr "Anonymous constructor";
          Cstr_tuple { ct_name = "With_id";
                        ct_args = ["int"];
                        ct_codec = `default_codec;
                        ct_flvconfigs = [Flvconfig_flat_kind
                                          { kind_fname=Some "kind"; arg_fname=Some "arg"; }]
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
              cr_flvconfigs = [Flvconfig_flat_kind { kind_fname=Some "kind"; arg_fname=None; }];
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
              cr_flvconfigs = [Flvconfig_flat_kind { kind_fname=Some "kind"; arg_fname=None; }]
            }, `docstr "Teacher constructor"], `docstr "definition of person type";
    td_flvconfigs = [];
  }

let fwrt : (unit, unit) fwrt_decl =
  FwrtTypeEnv.init
  |> FwrtTypeEnv.bind ~annot:() "person" []
  |> FwrtTypeEnv.bind ~parent:(Some "person") ~annot:() ~kind_fname:"kind"
    "Anonymous" []
  |> FwrtTypeEnv.bind ~parent:(Some "person") ~annot:() ~kind_fname:"kind"
    "With_id" [{ ff_name = "arg"; ff_type = ["int"]; ff_annot = () }, `nodoc]
  |> FwrtTypeEnv.bind ~parent:(Some "person") ~annot:() ~kind_fname:"kind"
    "Student" [
    { ff_name = "student_id"; ff_type = ["int"]; ff_annot = () }, `nodoc;
    { ff_name = "name"; ff_type = ["string"]; ff_annot = () }, `nodoc;
  ]
  |> FwrtTypeEnv.bind ~parent:(Some "person") ~annot:() ~kind_fname:"kind"
    "Teacher" [
    { ff_name = "faculty_id"; ff_type = ["int"]; ff_annot = () }, `nodoc;
    { ff_name = "name"; ff_type = ["string"]; ff_annot = () }, `nodoc;
    { ff_name = "department"; ff_type = ["string"]; ff_annot = () }, `nodoc;
  ]
  |> fun env -> ("person", env)
