<!-- Copyright 2022 Kotoi-Xie Consultancy, Inc. This file is a part of the

==== Bindoj (https://kxc.dev/bindoj) ====

software project that is developed, maintained, and distributed by
Kotoi-Xie Consultancy, Inc. (https://kxc.inc) which is also known as KXC.

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy
of the License at http://www.apache.org/licenses/LICENSE-2.0. Unless required
by applicable law or agreed to in writing, software distributed under the
License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
OF ANY KIND, either express or implied. See the License for the specific
language governing permissions and limitations under the License.
                                                                             -->
<!-- Acknowledgements  --- AnchorZ Inc. ---  The current/initial version or a
significant portion of this file is developed under the funding provided by
AnchorZ Inc. to satisfy its needs in its product development workflow.
                                                                             -->
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
  td_doc : Bindoj_base.Runtime.doc;
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
  td_doc : Bindoj_runtime.doc;
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
       [("admissionYear", (int_to_json x0));
       ("fullName", (string_to_json x1))] : student -> Kxclib.Json.jv)
  [@@warning "-39"]
- : unit = ()
# Bindoj.(
   let student_decoder = Caml_gen.Json_codec.gen_json_decoder_result student_desc in
   Caml.Structure.([binding student_decoder] |> printf "%a@?" pp_caml));;
let student_of_json' =
  (fun x ->
     let rec of_json_impl path =
       function
       | `obj param ->
           let (>>=) = Result.bind in
           (((List.assoc_opt "admissionYear" param) |>
               (function
                | Some a -> Ok a
                | None ->
                    Error
                      ("mandatory field 'admissionYear' does not exist",
                        path)))
              >>= (int_of_json ((`f "admissionYear") :: path)))
             >>=
             ((fun x0 ->
                 (((List.assoc_opt "fullName" param) |>
                     (function
                      | Some a -> Ok a
                      | None ->
                          Error
                            ("mandatory field 'fullName' does not exist",
                              path)))
                    >>= (string_of_json ((`f "fullName") :: path)))
                   >>= (fun x1 -> Ok { admission_year = x0; full_name = x1 })))
       | jv ->
           Error
             ((Printf.sprintf
                 "an object is expected for a record value, but the given is of type '%s'"
                 (let open Kxclib.Json in string_of_jv_kind (classify_jv jv))),
               path) in
     (of_json_impl [] x) |>
       (Result.map_error
          (fun (msg, path) ->
             let msg =
               match path with
               | [] -> Printf.sprintf "%s at root" msg
               | path ->
                   Printf.sprintf "%s at path %s" msg
                     ((path |> List.rev) |> Kxclib.Json.unparse_jvpath) in
             (msg, path,
               (`with_warning
                  ("not considering any config if exists",
                    (`named
                       ("Student",
                         (`object_of
                            [`mandatory_field ("admissionYear", `integral);
                            `mandatory_field ("fullName", `string)])))))))) :
  Kxclib.Json.jv -> student Bindoj_runtime.OfJsonResult.t)[@@warning "-39"]
- : unit = ()
# Bindoj.(
   let person_encoder = Caml_gen.Json_codec.gen_json_encoder person_desc in
   Caml.Structure.([binding person_encoder] |> printf "%a@?" pp_caml));;
let person_to_json =
  (function
   | Anonymous -> `obj [("kind", (`str "anonymous"))]
   | With_id (x0) ->
       `obj [("kind", (`str "with-id")); ("arg", (int_to_json x0))]
   | Student { student_id = x0; name = x1 } ->
       `obj
         [("kind", (`str "student"));
         ("studentId", (int_to_json x0));
         ("name", (string_to_json x1))]
   | Teacher { faculty_id = x0; name = x1; department = x2 } ->
       `obj
         [("kind", (`str "teacher"));
         ("facultyId", (int_to_json x0));
         ("name", (string_to_json x1));
         ("department", (string_to_json x2))] : person -> Kxclib.Json.jv)
  [@@warning "-39"]
- : unit = ()
# Bindoj.(
   let person_decoder = Caml_gen.Json_codec.gen_json_decoder_result person_desc in
   Caml.Structure.([binding person_decoder] |> printf "%a@?" pp_caml));;
let person_of_json' =
  (fun x ->
     let rec of_json_impl path __bindoj_orig =
       (__bindoj_orig |> (Kxclib.Jv.pump_field "kind")) |>
         (function
          | `obj (("kind", `str "anonymous")::_) -> Ok Anonymous
          | `obj (("kind", `str "with-id")::param) ->
              (match List.assoc_opt "arg" param with
               | Some arg ->
                   let (>>=) = Result.bind in
                   (int_of_json ((`f "arg") :: path) arg) >>=
                     ((fun x0 -> Ok (With_id x0)))
               | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("kind", `str "student")::param) ->
              let (>>=) = Result.bind in
              (((List.assoc_opt "studentId" param) |>
                  (function
                   | Some a -> Ok a
                   | None ->
                       Error
                         ("mandatory field 'studentId' does not exist", path)))
                 >>= (int_of_json ((`f "studentId") :: path)))
                >>=
                ((fun x0 ->
                    (((List.assoc_opt "name" param) |>
                        (function
                         | Some a -> Ok a
                         | None ->
                             Error
                               ("mandatory field 'name' does not exist",
                                 path)))
                       >>= (string_of_json ((`f "name") :: path)))
                      >>=
                      (fun x1 -> Ok (Student { student_id = x0; name = x1 }))))
          | `obj (("kind", `str "teacher")::param) ->
              let (>>=) = Result.bind in
              (((List.assoc_opt "facultyId" param) |>
                  (function
                   | Some a -> Ok a
                   | None ->
                       Error
                         ("mandatory field 'facultyId' does not exist", path)))
                 >>= (int_of_json ((`f "facultyId") :: path)))
                >>=
                ((fun x0 ->
                    (((List.assoc_opt "name" param) |>
                        (function
                         | Some a -> Ok a
                         | None ->
                             Error
                               ("mandatory field 'name' does not exist",
                                 path)))
                       >>= (string_of_json ((`f "name") :: path)))
                      >>=
                      (fun x1 ->
                         (((List.assoc_opt "department" param) |>
                             (function
                              | Some a -> Ok a
                              | None ->
                                  Error
                                    ("mandatory field 'department' does not exist",
                                      path)))
                            >>= (string_of_json ((`f "department") :: path)))
                           >>=
                           (fun x2 ->
                              Ok
                                (Teacher
                                   {
                                     faculty_id = x0;
                                     name = x1;
                                     department = x2
                                   })))))
          | `obj (("kind", `str discriminator_value)::_) ->
              Error
                ((Printf.sprintf
                    "given discriminator field value '%s' is not one of [ 'anonymous', 'with-id', 'student', 'teacher' ]"
                    discriminator_value), ((`f "kind") :: path))
          | `obj (("kind", jv)::_) ->
              Error
                ((Printf.sprintf
                    "a string is expected for a variant discriminator, but the given is of type '%s'"
                    (let open Kxclib.Json in
                       string_of_jv_kind (classify_jv jv))), ((`f "kind") ::
                  path))
          | `obj _ ->
              Error ("discriminator field 'kind' does not exist", path)
          | jv ->
              Error
                ((Printf.sprintf
                    "an object is expected for a variant value, but the given is of type '%s'"
                    (let open Kxclib.Json in
                       string_of_jv_kind (classify_jv jv))), path)) in
     (of_json_impl [] x) |>
       (Result.map_error
          (fun (msg, path) ->
             let msg =
               match path with
               | [] -> Printf.sprintf "%s at root" msg
               | path ->
                   Printf.sprintf "%s at path %s" msg
                     ((path |> List.rev) |> Kxclib.Json.unparse_jvpath) in
             (msg, path,
               (`with_warning
                  ("not considering any config if exists",
                    (`named
                       ("Person",
                         (`anyone_of
                            [`object_of
                               [`mandatory_field
                                  ("kind", (`exactly (`str "anonymous")))];
                            `object_of
                              [`mandatory_field
                                 ("kind", (`exactly (`str "with-id")));
                              `mandatory_field
                                ("arg", (`tuple_of [`integral]))];
                            `object_of
                              [`mandatory_field
                                 ("kind", (`exactly (`str "student")));
                              `mandatory_field ("studentId", `integral);
                              `mandatory_field ("name", `string)];
                            `object_of
                              [`mandatory_field
                                 ("kind", (`exactly (`str "teacher")));
                              `mandatory_field ("facultyId", `integral);
                              `mandatory_field ("name", `string);
                              `mandatory_field ("department", `string)]])))))))) :
  Kxclib.Json.jv -> person Bindoj_runtime.OfJsonResult.t)[@@warning "-39"]
- : unit = ()
```
