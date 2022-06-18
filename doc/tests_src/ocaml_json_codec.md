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
        `nodoc;
      td_flvconfigs = [] };;
val student_desc : type_decl =
  {td_name = "student";
   td_kind =
    (Record_kind
      [({rf_name = "admission_year"; rf_type = "int";
         rf_codec = `default_codec},
        `nodoc);
       ({rf_name = "full_name"; rf_type = "string";
         rf_codec = `default_codec},
        `nodoc)],
     `nodoc);
   td_flvconfigs = Bindoj.Type_desc.FlavorConfigs.[]}
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
        `nodoc;
      td_flvconfigs = [] };;
val person_desc : type_decl =
  {td_name = "person";
   td_kind =
    (Variant_kind
      [(Cstr_tuple
         {Bindoj.Type_desc.ct_name = "Anonymous"; ct_args = [];
          ct_codec = `default_codec;
          ct_flvconfigs =
           Bindoj.Type_desc.FlavorConfigs.(::)
            (Bindoj_gen.Json_codec.Flvconfig_flat_kind
              {kind_fname = Some "kind"; arg_fname = None},
             Bindoj.Type_desc.FlavorConfigs.[])},
        `nodoc);
       (Cstr_tuple
         {Bindoj.Type_desc.ct_name = "With_id"; ct_args = ["int"];
          ct_codec = `default_codec;
          ct_flvconfigs =
           Bindoj.Type_desc.FlavorConfigs.(::)
            (Bindoj_gen.Json_codec.Flvconfig_flat_kind
              {kind_fname = Some "kind"; arg_fname = Some "arg"},
             Bindoj.Type_desc.FlavorConfigs.[])},
        `nodoc);
       (Cstr_record
         {Bindoj.Type_desc.cr_name = "Student";
          cr_fields =
           [({rf_name = "student_id"; rf_type = "int";
              rf_codec = `default_codec},
             `nodoc);
            ({rf_name = "name"; rf_type = "string";
              rf_codec = `default_codec},
             `nodoc)];
          cr_codec = `default_codec;
          cr_flvconfigs =
           Bindoj.Type_desc.FlavorConfigs.(::)
            (Bindoj_gen.Json_codec.Flvconfig_flat_kind
              {kind_fname = Some "kind"; arg_fname = None},
             Bindoj.Type_desc.FlavorConfigs.[])},
        `nodoc);
       (Cstr_record
         {Bindoj.Type_desc.cr_name = "Teacher";
          cr_fields =
           [({rf_name = "faculty_id"; rf_type = "int";
              rf_codec = `default_codec},
             `nodoc);
            ({rf_name = "name"; rf_type = "string";
              rf_codec = `default_codec},
             `nodoc);
            ({rf_name = "department"; rf_type = "string";
              rf_codec = `default_codec},
             `nodoc)];
          cr_codec = `default_codec;
          cr_flvconfigs =
           Bindoj.Type_desc.FlavorConfigs.(::)
            (Bindoj_gen.Json_codec.Flvconfig_flat_kind
              {kind_fname = Some "kind"; arg_fname = None},
             Bindoj.Type_desc.FlavorConfigs.[])},
        `nodoc)],
     `nodoc);
   td_flvconfigs = Bindoj.Type_desc.FlavorConfigs.[]}
```

### `Bindoj.Caml_gen.Json_codec`
```ocaml
# Bindoj.(
   let student_encoder = Caml_gen.Json_codec.gen_json_encoder student_desc in
   Caml.Structure.([binding student_encoder] |> printf "%a@?" pp_caml));;
let encode_student_json =
  (fun
     { admission_year = __bindoj_gen_json_encoder_var_0;
       full_name = __bindoj_gen_json_encoder_var_1 }
     ->
     `obj
       [("admission_year", (encode_int_json __bindoj_gen_json_encoder_var_0));
       ("full_name", (encode_string_json __bindoj_gen_json_encoder_var_1))] :
  student -> Kxclib.Json.jv)
- : unit = ()
# Bindoj.(
   let student_decoder = Caml_gen.Json_codec.gen_json_decoder student_desc in
   Caml.Structure.([binding student_decoder] |> printf "%a@?" pp_caml));;
let decode_student_json =
  (function
   | `obj __bindoj_gen_json_decoder_var_param ->
       let (>>=) = Option.bind in
       ((List.assoc_opt "admission_year" __bindoj_gen_json_decoder_var_param)
          >>= decode_int_json)
         >>=
         ((fun __bindoj_gen_json_decoder_var_0 ->
             ((List.assoc_opt "full_name" __bindoj_gen_json_decoder_var_param)
                >>= decode_string_json)
               >>=
               (fun __bindoj_gen_json_decoder_var_1 ->
                  Some
                    {
                      admission_year = __bindoj_gen_json_decoder_var_0;
                      full_name = __bindoj_gen_json_decoder_var_1
                    })))
   | _ -> None : Kxclib.Json.jv -> student option)
- : unit = ()
# Bindoj.(
   let person_encoder = Caml_gen.Json_codec.gen_json_encoder person_desc in
   Caml.Structure.([binding person_encoder] |> printf "%a@?" pp_caml));;
let encode_person_json =
  (function
   | Anonymous -> `obj [("kind", (`str "Anonymous"))]
   | With_id (__bindoj_gen_json_encoder_var_0) ->
       `obj
         [("kind", (`str "With_id"));
         ("arg", (encode_int_json __bindoj_gen_json_encoder_var_0))]
   | Student
       { student_id = __bindoj_gen_json_encoder_var_0;
         name = __bindoj_gen_json_encoder_var_1 }
       ->
       `obj
         [("kind", (`str "Student"));
         ("student_id", (encode_int_json __bindoj_gen_json_encoder_var_0));
         ("name", (encode_string_json __bindoj_gen_json_encoder_var_1))]
   | Teacher
       { faculty_id = __bindoj_gen_json_encoder_var_0;
         name = __bindoj_gen_json_encoder_var_1;
         department = __bindoj_gen_json_encoder_var_2 }
       ->
       `obj
         [("kind", (`str "Teacher"));
         ("faculty_id", (encode_int_json __bindoj_gen_json_encoder_var_0));
         ("name", (encode_string_json __bindoj_gen_json_encoder_var_1));
         ("department", (encode_string_json __bindoj_gen_json_encoder_var_2))] :
  person -> Kxclib.Json.jv)
- : unit = ()
# Bindoj.(
   let person_decoder = Caml_gen.Json_codec.gen_json_decoder person_desc in
   Caml.Structure.([binding person_decoder] |> printf "%a@?" pp_caml));;
let decode_person_json =
  (fun __bindoj_orig ->
     (Kxclib.Jv.pump_field "kind" __bindoj_orig) |>
       (function
        | `obj (("kind", `str "Anonymous")::[]) -> Some Anonymous
        | `obj
            (("kind", `str "With_id")::("arg",
                                        __bindoj_gen_json_decoder_var_0)::[])
            ->
            let (>>=) = Option.bind in
            (decode_int_json __bindoj_gen_json_decoder_var_0) >>=
              ((fun __bindoj_gen_json_decoder_var_0 ->
                  Some (With_id __bindoj_gen_json_decoder_var_0)))
        | `obj
            (("kind", `str "Student")::__bindoj_gen_json_decoder_var_param)
            ->
            let (>>=) = Option.bind in
            ((List.assoc_opt "student_id" __bindoj_gen_json_decoder_var_param)
               >>= decode_int_json)
              >>=
              ((fun __bindoj_gen_json_decoder_var_0 ->
                  ((List.assoc_opt "name" __bindoj_gen_json_decoder_var_param)
                     >>= decode_string_json)
                    >>=
                    (fun __bindoj_gen_json_decoder_var_1 ->
                       Some
                         (Student
                            {
                              student_id = __bindoj_gen_json_decoder_var_0;
                              name = __bindoj_gen_json_decoder_var_1
                            }))))
        | `obj
            (("kind", `str "Teacher")::__bindoj_gen_json_decoder_var_param)
            ->
            let (>>=) = Option.bind in
            ((List.assoc_opt "faculty_id" __bindoj_gen_json_decoder_var_param)
               >>= decode_int_json)
              >>=
              ((fun __bindoj_gen_json_decoder_var_0 ->
                  ((List.assoc_opt "name" __bindoj_gen_json_decoder_var_param)
                     >>= decode_string_json)
                    >>=
                    (fun __bindoj_gen_json_decoder_var_1 ->
                       ((List.assoc_opt "department"
                           __bindoj_gen_json_decoder_var_param)
                          >>= decode_string_json)
                         >>=
                         (fun __bindoj_gen_json_decoder_var_2 ->
                            Some
                              (Teacher
                                 {
                                   faculty_id =
                                     __bindoj_gen_json_decoder_var_0;
                                   name = __bindoj_gen_json_decoder_var_1;
                                   department =
                                     __bindoj_gen_json_decoder_var_2
                                 })))))
        | _ -> None) : Kxclib.Json.jv -> person option)
- : unit = ()
```
