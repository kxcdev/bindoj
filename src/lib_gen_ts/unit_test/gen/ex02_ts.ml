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

let ex02 : type_decl =
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
                                            { kind_fname=Some "kind"; arg_fname=None; }];
                       }, `nodoc;
           Cstr_record { cr_name = "Teacher";
                         cr_fields =
                           [{ rf_name = "faculty_id"; rf_type = "int"; rf_codec = `default_codec; }, `nodoc;
                            { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec; }, `nodoc;
                            { rf_name = "department"; rf_type = "string"; rf_codec = `default_codec; }, `nodoc ];
                         cr_codec = `default_codec;
                         cr_flvconfigs = [Flvconfig_flat_kind
                                            { kind_fname=Some "kind"; arg_fname=Some "arg"; }];
                       }, `nodoc]), `nodoc; }

let () =
  print_endline (gen_ts_type ex02);
  print_endline (gen_ts_case_analyzer ex02)
