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

open struct
  module type Ex = sig val decl: type_decl val decl_with_docstr: type_decl end
  let make_decl with_doc =
    let decl, doc, open_ =
      if with_doc then
        (fun (module E: Ex) -> E.decl_with_docstr),
        (fun s -> `docstr s),
        (fun s -> `open_ (s ^ "_docstr_gen"))
      else
        (fun (module E: Ex) -> E.decl),
        constant `nodoc,
        (fun s -> `open_ (s ^ "_gen"))
    in
    record_decl "nested_record" [
      record_field_nested ~codec:(open_ "Ex11") "unit" (decl (module Ex11))
        ~doc:(doc "unit field");
      record_field_nested ~codec:(open_ "Ex01") "student" (decl (module Ex01))
        ~configs:[
          Json_config.no_mangling;
        ]
        ~doc:(doc "student field");
      record_field_nested ~codec:(open_ "Ex09") "int53p" (decl (module Ex09))
        ~configs:[
          Json_config.nested_field_style `spreading;
          Json_config.no_mangling;
        ]
        ~doc:(doc "int53p field");
      record_field_nested ~codec:(open_ "Ex02_no_mangling") "person1" (decl (module Ex02_no_mangling))
        ~doc:(doc "person1 field");
      record_field_nested ~codec:(open_ "Ex02_no_mangling") "person2" (decl (module Ex02_no_mangling))
        ~configs:[
          Json_config.nested_field_style `spreading;
        ]
        ~doc:(doc "person2 field");
  ]  ~doc:(doc "definition of nested_record type")
end

let decl : type_decl = make_decl false
let decl_with_docstr : type_decl = make_decl true

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
  |> bind_object person [] ~configs:Json_config.[ no_mangling; name "person_no_mangling" ]
  |> bind_constructor ~parent:person "Anonymous" ~configs:[ Json_config.no_mangling ]
  |> bind_constructor ~parent:person "With_id" ~args:[variant_argument cty_int] ~configs:[ Json_config.no_mangling ]
  |> bind_constructor ~parent:person "Student" ~fields:[
    field "student_id" cty_int ~configs:[ Json_config.no_mangling ];
    field "name" cty_string ~configs:[ Json_config.no_mangling ] ]
    ~configs:[ Json_config.no_mangling ]
  |> bind_constructor ~parent:person "Teacher" ~fields:[
    field "faculty_id" cty_int ~configs:[ Json_config.no_mangling ];
    field "name" cty_string ~configs:[ Json_config.no_mangling ];
    field "department" cty_string ~configs:[ Json_config.no_mangling ] ]
    ~configs:[ Json_config.no_mangling ]
  |> bind_object "nested_record" [
    field_nested ~codec:(`open_ "Ex11_gen") "unit" "unit";
    field_nested ~codec:(`open_ "Ex01_gen") "student" "student"
      ~configs:[
        Json_config.no_mangling;
      ];
    field_nested ~codec:(`open_ "Ex09_gen") "int53p" "with_int53p"
      ~configs:[
        Json_config.nested_field_style `spreading;
        Json_config.no_mangling;
      ];
    field_nested ~codec:(`open_ "Ex02_no_mangling_gen") "person1" "person";
    field_nested ~codec:(`open_ "Ex02_no_mangling_gen") "person2" "person"
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
          `type_literal Util.Ts_ast.[
            property "unit" (`type_reference "Unit");
            property "student" (`type_reference "Student");
            property "person1" (`type_reference "person_no_mangling");
          ];
          `type_reference "WithInt53p";
          `type_reference "person_no_mangling";
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
                         ("person_no_mangling",
                           (`anyone_of
                              [`object_of
                                 [`mandatory_field
                                    ("kind", (`exactly (`str "Anonymous")))];
                              `object_of
                                [`mandatory_field
                                   ("kind", (`exactly (`str "With_id")));
                                `mandatory_field
                                  ("arg", `integral)];
                              `object_of
                                [`mandatory_field
                                   ("kind", (`exactly (`str "Student")));
                                `mandatory_field ("student_id", `integral);
                                `mandatory_field ("name", `string)];
                              `object_of
                                [`mandatory_field
                                   ("kind", (`exactly (`str "Teacher")));
                                `mandatory_field ("faculty_id", `integral);
                                `mandatory_field ("name", `string);
                                `mandatory_field ("department", `string)]]))));
                  `mandatory_field ("kind", (`exactly (`str "Anonymous")))];
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
                        ("person_no_mangling",
                          (`anyone_of
                             [`object_of
                                [`mandatory_field
                                   ("kind", (`exactly (`str "Anonymous")))];
                             `object_of
                               [`mandatory_field
                                  ("kind", (`exactly (`str "With_id")));
                               `mandatory_field
                                 ("arg", `integral)];
                             `object_of
                               [`mandatory_field
                                  ("kind", (`exactly (`str "Student")));
                               `mandatory_field ("student_id", `integral);
                               `mandatory_field ("name", `string)];
                             `object_of
                               [`mandatory_field
                                  ("kind", (`exactly (`str "Teacher")));
                               `mandatory_field ("faculty_id", `integral);
                               `mandatory_field ("name", `string);
                               `mandatory_field ("department", `string)]]))));
                 `mandatory_field ("kind", (`exactly (`str "With_id")));
                 `mandatory_field ("arg", `integral)];
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
                        ("person_no_mangling",
                          (`anyone_of
                             [`object_of
                                [`mandatory_field
                                   ("kind", (`exactly (`str "Anonymous")))];
                             `object_of
                               [`mandatory_field
                                  ("kind", (`exactly (`str "With_id")));
                               `mandatory_field
                                 ("arg", `integral)];
                             `object_of
                               [`mandatory_field
                                  ("kind", (`exactly (`str "Student")));
                               `mandatory_field ("student_id", `integral);
                               `mandatory_field ("name", `string)];
                             `object_of
                               [`mandatory_field
                                  ("kind", (`exactly (`str "Teacher")));
                               `mandatory_field ("faculty_id", `integral);
                               `mandatory_field ("name", `string);
                               `mandatory_field ("department", `string)]]))));
                 `mandatory_field ("kind", (`exactly (`str "Student")));
                 `mandatory_field ("student_id", `integral);
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
                        ("person_no_mangling",
                          (`anyone_of
                             [`object_of
                                [`mandatory_field
                                   ("kind", (`exactly (`str "Anonymous")))];
                             `object_of
                               [`mandatory_field
                                  ("kind", (`exactly (`str "With_id")));
                               `mandatory_field
                                 ("arg", `integral)];
                             `object_of
                               [`mandatory_field
                                  ("kind", (`exactly (`str "Student")));
                               `mandatory_field ("student_id", `integral);
                               `mandatory_field ("name", `string)];
                             `object_of
                               [`mandatory_field
                                  ("kind", (`exactly (`str "Teacher")));
                               `mandatory_field ("faculty_id", `integral);
                               `mandatory_field ("name", `string);
                               `mandatory_field ("department", `string)]]))));
                 `mandatory_field ("kind", (`exactly (`str "Teacher")));
                 `mandatory_field ("faculty_id", `integral);
                 `mandatory_field ("name", `string);
                 `mandatory_field ("department", `string)]]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option =
  let open Schema_object in
  let person_ctors = [
    "Anonymous", [];
    "With_id", [ "arg", integer () ];
    "Student", [
      "student_id", integer ();
      "name", string ();
    ];
    "Teacher", [
      "faculty_id", integer ();
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
