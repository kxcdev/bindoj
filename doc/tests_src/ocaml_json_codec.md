<!-- Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. -->

```ocaml
# (* preparation *);;
# open Kxclib;;
# open Doctests_utils.Test_utils;;
```

## Basic OCaml JSON Codec Generation

`Bindoj.Caml_gen.Json_codec.gen_encoder`/`Bindoj.Caml_gen.Json_codec.gen_decoder`
are functions generating JSON encoder/decoder from
`Bindoj.Type_desc.type_declaration`. The following are JSON encoder/decoder
generation examples: `student` of record type and `person` of variant type.

### simple record type : student
```ocaml
# open Bindoj.Type_desc;;
# #show_type type_decl;;
type nonrec type_decl =
  type_decl = {
  td_name : string;
  td_kind : generic_kind with_docstr;
  td_flvconfigs : [ `type_decl ] flavor_configs;
}
# let student_desc =
    { td_name = "student";
      td_kind =
        Record_kind
          ([{ rf_name = "admission_year"; rf_type = "int"; rf_codec = `default_codec }, `nodoc;
            { rf_name = "full_name"; rf_type = "string"; rf_codec = `default_codec }, `nodoc;]),
        `nodoc; };;
Lines 2-7, characters 5-18:
Error: Some record fields are undefined: td_flvconfigs
```

### simple variant type : person
```ocaml
# open Bindoj.Caml_gen.Json_codec;;
# #show_type type_decl;;
type nonrec type_decl =
  type_decl = {
  td_name : string;
  td_kind : generic_kind with_docstr;
  td_flvconfigs : [ `type_decl ] flavor_configs;
}
# let person_desc =
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
        `nodoc; };;
Lines 2-34, characters 5-18:
Error: Some record fields are undefined: td_flvconfigs
```

### `Bindoj.Caml_gen.Json_codec`
```ocaml
# Bindoj.(
   let student_encoder = Caml_gen.Json_codec.gen_json_encoder student_desc in
   Caml.Structure.([binding student_encoder] |> printf "%a@?" pp_caml));;
Line 2, characters 63-75:
Error: Unbound value student_desc
# Bindoj.(
   let student_decoder = Caml_gen.Json_codec.gen_json_decoder student_desc in
   Caml.Structure.([binding student_decoder] |> printf "%a@?" pp_caml));;
Line 2, characters 63-75:
Error: Unbound value student_desc
# Bindoj.(
   let person_encoder = Caml_gen.Json_codec.gen_json_encoder person_desc in
   Caml.Structure.([binding person_encoder] |> printf "%a@?" pp_caml));;
Line 2, characters 62-73:
Error: Unbound value person_desc
# Bindoj.(
   let person_decoder = Caml_gen.Json_codec.gen_json_decoder person_desc in
   Caml.Structure.([binding person_decoder] |> printf "%a@?" pp_caml));;
Line 2, characters 62-73:
Error: Unbound value person_desc
```
