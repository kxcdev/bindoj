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
  td_configs : [ `type_decl ] configs;
  td_kind : type_decl_kind;
  td_doc : doc;
}
# let student_desc =
    record_decl "student" [
      record_field "admission_year" (Coretype.mk_prim `int);
      record_field "full_name" (Coretype.mk_prim `string);
    ];;
val student_desc : type_decl =
  {td_name = "student"; td_configs = Bindoj.Type_desc.Configs.[];
   td_kind =
    Record_decl
     [{rf_name = "admission_year";
       rf_type =
        {Bindoj.Type_desc.Coretype.ct_desc =
          Bindoj.Type_desc.Coretype.Prim `int;
         ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
       rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc};
      {rf_name = "full_name";
       rf_type =
        {Bindoj.Type_desc.Coretype.ct_desc =
          Bindoj.Type_desc.Coretype.Prim `string;
         ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
       rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc}];
   td_doc = `nodoc}
```

### simple variant type : person
```ocaml
# open Bindoj.Caml_gen.Json_codec;;
# #show_type type_decl;;
type nonrec type_decl =
  type_decl = {
  td_name : string;
  td_configs : [ `type_decl ] configs;
  td_kind : type_decl_kind;
  td_doc : doc;
}
# let person_desc =
    variant_decl "person" [
      variant_constructor "Anonymous" `no_param;
      variant_constructor "With_id" (`tuple_like [Coretype.mk_prim `int]);
      variant_constructor "Student" (`inline_record [
        record_field "student_id" (Coretype.mk_prim `int);
        record_field "name" (Coretype.mk_prim `string);
      ]);
      variant_constructor "Teacher" (`inline_record [
        record_field "faculty_id" (Coretype.mk_prim `int);
        record_field "name" (Coretype.mk_prim `string);
        record_field "department" (Coretype.mk_prim `string);
      ])
    ];;
val person_desc : type_decl =
  {td_name = "person"; td_configs = Bindoj.Type_desc.Configs.[];
   td_kind =
    Variant_decl
     [{vc_name = "Anonymous"; vc_param = `no_param;
       vc_configs = Bindoj.Type_desc.Configs.[]; vc_doc = `nodoc};
      {vc_name = "With_id";
       vc_param =
        `tuple_like
          [{Bindoj.Type_desc.Coretype.ct_desc =
             Bindoj.Type_desc.Coretype.Prim `int;
            ct_configs = Bindoj_typedesc.Type_desc.Configs.[]}];
       vc_configs = Bindoj.Type_desc.Configs.[]; vc_doc = `nodoc};
      {vc_name = "Student";
       vc_param =
        `inline_record
          [{rf_name = "student_id";
            rf_type =
             {Bindoj.Type_desc.Coretype.ct_desc =
               Bindoj.Type_desc.Coretype.Prim `int;
              ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
            rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc};
           {rf_name = "name";
            rf_type =
             {Bindoj.Type_desc.Coretype.ct_desc =
               Bindoj.Type_desc.Coretype.Prim `string;
              ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
            rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc}];
       vc_configs = Bindoj.Type_desc.Configs.[]; vc_doc = `nodoc};
      {vc_name = "Teacher";
       vc_param =
        `inline_record
          [{rf_name = "faculty_id";
            rf_type =
             {Bindoj.Type_desc.Coretype.ct_desc =
               Bindoj.Type_desc.Coretype.Prim `int;
              ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
            rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc};
           {rf_name = "name";
            rf_type =
             {Bindoj.Type_desc.Coretype.ct_desc =
               Bindoj.Type_desc.Coretype.Prim `string;
              ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
            rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc};
           {rf_name = "department";
            rf_type =
             {Bindoj.Type_desc.Coretype.ct_desc =
               Bindoj.Type_desc.Coretype.Prim `string;
              ct_configs = Bindoj_typedesc.Type_desc.Configs.[]};
            rf_configs = Bindoj.Type_desc.Configs.[]; rf_doc = `nodoc}];
       vc_configs = Bindoj.Type_desc.Configs.[]; vc_doc = `nodoc}];
   td_doc = `nodoc}
```

### `Bindoj.Caml_gen.Json_codec`
```ocaml
# Bindoj.(
   let student_encoder = Caml_gen.Json_codec.gen_json_encoder student_desc in
   Caml.Structure.([binding student_encoder] |> printf "%a@?" pp_caml));;
let student_to_json =
  (fun { admission_year = x0; full_name = x1 } ->
     `obj
       [("admission_year", (int_to_json x0));
       ("full_name", (string_to_json x1))] : student -> Kxclib.Json.jv)
  [@@warning "-39"]
- : unit = ()
# Bindoj.(
   let student_decoder = Caml_gen.Json_codec.gen_json_decoder student_desc in
   Caml.Structure.([binding student_decoder] |> printf "%a@?" pp_caml));;
let student_of_json =
  (function
   | `obj param ->
       let (>>=) = Option.bind in
       ((List.assoc_opt "admission_year" param) >>= int_of_json) >>=
         ((fun x0 ->
             ((List.assoc_opt "full_name" param) >>= string_of_json) >>=
               (fun x1 -> Some { admission_year = x0; full_name = x1 })))
   | _ -> None : Kxclib.Json.jv -> student option)[@@warning "-39"]
- : unit = ()
# Bindoj.(
   let person_encoder = Caml_gen.Json_codec.gen_json_encoder person_desc in
   Caml.Structure.([binding person_encoder] |> printf "%a@?" pp_caml));;
let person_to_json =
  (function
   | Anonymous -> `obj [("kind", (`str "Anonymous"))]
   | With_id (x0) ->
       `obj [("kind", (`str "With_id")); ("arg", (int_to_json x0))]
   | Student { student_id = x0; name = x1 } ->
       `obj
         [("kind", (`str "Student"));
         ("student_id", (int_to_json x0));
         ("name", (string_to_json x1))]
   | Teacher { faculty_id = x0; name = x1; department = x2 } ->
       `obj
         [("kind", (`str "Teacher"));
         ("faculty_id", (int_to_json x0));
         ("name", (string_to_json x1));
         ("department", (string_to_json x2))] : person -> Kxclib.Json.jv)
  [@@warning "-39"]
- : unit = ()
# Bindoj.(
   let person_decoder = Caml_gen.Json_codec.gen_json_decoder person_desc in
   Caml.Structure.([binding person_decoder] |> printf "%a@?" pp_caml));;
let person_of_json =
  (fun __bindoj_orig ->
     (Kxclib.Jv.pump_field "kind" __bindoj_orig) |>
       (function
        | `obj (("kind", `str "Anonymous")::[]) -> Some Anonymous
        | `obj (("kind", `str "With_id")::("arg", x0)::[]) ->
            let (>>=) = Option.bind in
            (int_of_json x0) >>= ((fun x0 -> Some (With_id x0)))
        | `obj (("kind", `str "Student")::param) ->
            let (>>=) = Option.bind in
            ((List.assoc_opt "student_id" param) >>= int_of_json) >>=
              ((fun x0 ->
                  ((List.assoc_opt "name" param) >>= string_of_json) >>=
                    (fun x1 -> Some (Student { student_id = x0; name = x1 }))))
        | `obj (("kind", `str "Teacher")::param) ->
            let (>>=) = Option.bind in
            ((List.assoc_opt "faculty_id" param) >>= int_of_json) >>=
              ((fun x0 ->
                  ((List.assoc_opt "name" param) >>= string_of_json) >>=
                    (fun x1 ->
                       ((List.assoc_opt "department" param) >>=
                          string_of_json)
                         >>=
                         (fun x2 ->
                            Some
                              (Teacher
                                 {
                                   faculty_id = x0;
                                   name = x1;
                                   department = x2
                                 })))))
        | _ -> None) : Kxclib.Json.jv -> person option)[@@warning "-39"]
- : unit = ()
```
