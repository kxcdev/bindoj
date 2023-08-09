(* Copyright 2022-2023 Kotoi-Xie Consultancy, Inc. This file is a part of the

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
                                                                              *)
(* Acknowledgements  --- AnchorZ Inc. ---  The current/initial version or a
significant portion of this file is developed under the funding provided by
AnchorZ Inc. to satisfy its needs in its product development workflow.
                                                                              *)
open Bindoj_base.Type_desc
open Bindoj_gen_ts.Typescript_datatype
open Bindoj_codec.Json

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex16"

let decl : type_decl =
  record_decl "nested_record" [
    record_field_nested ~codec:(`open_ "Ex11_gen") "unit" Ex11.decl;
    record_field_nested ~codec:(`open_ "Ex01_gen") "student" Ex01.decl;
    record_field_nested ~codec:(`open_ "Ex09_gen") "int53p" Ex09.decl
      ~configs:[
        Json_config.nested_field_style `spreading;
      ];
    record_field_nested ~codec:(`open_ "Ex02_gen") "person1" Ex02.decl;
    record_field_nested ~codec:(`open_ "Ex02_gen") "person2" Ex02.decl
      ~configs:[
        Json_config.nested_field_style `spreading;
      ];
  ]

let decl_with_docstr : type_decl =
  record_decl "nested_record" [
    record_field_nested ~codec:(`open_ "Ex11_gen") "unit" Ex11.decl
      ~doc:(`docstr "unit field");
    record_field_nested ~codec:(`open_ "Ex01_gen") "student" Ex01.decl
      ~doc:(`docstr "student field");
    record_field_nested ~codec:(`open_ "Ex09_gen") "int53p" Ex09.decl
      ~configs:[
        Json_config.nested_field_style `spreading;
      ]
      ~doc:(`docstr "int53p field");
    record_field_nested ~codec:(`open_ "Ex02_gen") "person1" Ex02.decl
      ~doc:(`docstr "person1 field");
    record_field_nested ~codec:(`open_ "Ex02_gen") "person2" Ex02.decl
      ~configs:[
        Json_config.nested_field_style `spreading;
      ]
      ~doc:(`docstr "person2 field");
  ] ~doc:(`docstr "definition of nested_record type")

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  let cty_unit = Coretype.mk_prim `unit in
  let cty_int = Coretype.mk_prim `int in
  let cty_int53p = Coretype.mk_prim `int53p in
  let cty_string = Coretype.mk_prim `string in
  let person = "person" in
  "nested_record", Util.FwrtTypeEnv.(
  init
  |> bind_alias "unit" cty_unit
  |> bind_object "student"
    [ field "admission_year" cty_int;
      field "name" cty_string; ]
  |> bind_object "with_int53p"
    [ field "value" cty_int53p; ]
  |> bind_object person []
  |> bind_constructor ~parent:person "Anonymous"
  |> bind_constructor ~parent:person "With_id" ~args:[variant_argument cty_int]
  |> bind_constructor ~parent:person "Student" ~fields:[
    field "student_id" cty_int;
    field "name" cty_string]
  |> bind_constructor ~parent:person "Teacher" ~fields:[
    field "faculty_id" cty_int;
    field "name" cty_string;
    field "department" cty_string ]
  |> bind_object "nested_record" [
    field_nested ~codec:(`open_ "Ex11_gen") "unit" "unit";
    field_nested ~codec:(`open_ "Ex01_gen") "student" "student";
    field_nested ~codec:(`open_ "Ex09_gen") "int53p" "with_int53p"
      ~configs:[
        Json_config.nested_field_style `spreading;
      ];
    field_nested ~codec:(`open_ "Ex02_gen") "person1" "person";
    field_nested ~codec:(`open_ "Ex02_gen") "person2" "person"
      ~configs:[
        Json_config.nested_field_style `spreading;
      ];
  ]
)

let ts_ast : ts_ast option =
  Some [
    `type_alias_declaration
      { tsa_modifiers = [`export];
        tsa_name = "NestedRecord";
        tsa_type_parameters = [];
        tsa_type_desc = `intersection [
          `type_literal [
            { tsps_modifiers = [];
              tsps_name = "unit";
              tsps_type_desc = `type_reference "Unit"; };
            { tsps_modifiers = [];
              tsps_name = "student";
              tsps_type_desc = `type_reference "Student"; };
            { tsps_modifiers = [];
              tsps_name = "person1";
              tsps_type_desc = `type_reference "Person"; };
          ];
          `type_reference "WithInt53p";
          `type_reference "Person";
        ]; }
  ]

let expected_json_shape_explanation =
  Some (
    `with_warning
     ("not considering any config if exists",
       (`named
          ("NestedRecord",
            (`anyone_of
               [`object_of
                  [`mandatory_field
                     ("unit",
                       (`named
                          ("Unit", (`special ("unit", (`exactly `null))))));
                  `mandatory_field
                    ("student",
                      (`named
                         ("Student",
                           (`object_of
                              [`mandatory_field ("admissionYear", `integral);
                              `mandatory_field ("name", `string)]))));
                  `mandatory_field ("value", `proper_int53p);
                  `mandatory_field
                    ("person1",
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
                                `mandatory_field ("department", `string)]]))));
                  `mandatory_field ("kind", (`exactly (`str "anonymous")))];
               `object_of
                 [`mandatory_field
                    ("unit",
                      (`named ("Unit", (`special ("unit", (`exactly `null))))));
                 `mandatory_field
                   ("student",
                     (`named
                        ("Student",
                          (`object_of
                             [`mandatory_field ("admissionYear", `integral);
                             `mandatory_field ("name", `string)]))));
                 `mandatory_field ("value", `proper_int53p);
                 `mandatory_field
                   ("person1",
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
                               `mandatory_field ("department", `string)]]))));
                 `mandatory_field ("kind", (`exactly (`str "with-id")));
                 `mandatory_field ("arg", (`tuple_of [`integral]))];
               `object_of
                 [`mandatory_field
                    ("unit",
                      (`named ("Unit", (`special ("unit", (`exactly `null))))));
                 `mandatory_field
                   ("student",
                     (`named
                        ("Student",
                          (`object_of
                             [`mandatory_field ("admissionYear", `integral);
                             `mandatory_field ("name", `string)]))));
                 `mandatory_field ("value", `proper_int53p);
                 `mandatory_field
                   ("person1",
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
                               `mandatory_field ("department", `string)]]))));
                 `mandatory_field ("kind", (`exactly (`str "student")));
                 `mandatory_field ("studentId", `integral);
                 `mandatory_field ("name", `string)];
               `object_of
                 [`mandatory_field
                    ("unit",
                      (`named ("Unit", (`special ("unit", (`exactly `null))))));
                 `mandatory_field
                   ("student",
                     (`named
                        ("Student",
                          (`object_of
                             [`mandatory_field ("admissionYear", `integral);
                             `mandatory_field ("name", `string)]))));
                 `mandatory_field ("value", `proper_int53p);
                 `mandatory_field
                   ("person1",
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
                               `mandatory_field ("department", `string)]]))));
                 `mandatory_field ("kind", (`exactly (`str "teacher")));
                 `mandatory_field ("facultyId", `integral);
                 `mandatory_field ("name", `string);
                 `mandatory_field ("department", `string)]]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option =
  let open Schema_object in
  let person_ctors = [
    "anonymous", [];
    "with-id", [ "arg", integer () ];
    "student", [
      "studentId", integer ();
      "name", string ();
    ];
    "teacher", [
      "facultyId", integer ();
      "name", string ();
      "department", string ();
    ];
  ] |&> (fun (name, fields) -> (name, fields @ [ "kind", string () ~enum:[`str name]])) in
  let fields =
    ("unit", integer ()
      ~minimum:1 ~maximum:1)
    :: ("student", record [
      "admissionYear", integer ();
      "name", string ();
    ]) :: [
      "value", integer ();
      "person1", oneOf (person_ctors |&> fun (name, fields) ->
        record ~title:name fields);
    ]
  in
  let name = "NestedRecord" in
  oneOf ~schema ~title:name ~id:("#"^name) (
    person_ctors |&> (fun (_, fs) ->
      record (fields @ fs)))
  |> Option.some
