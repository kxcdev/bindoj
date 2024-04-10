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
open Bindoj_openapi.V3

module Point2 : Util.Ex_desc = struct
  (* To test when types nested in the same file are (multiply) nested from different files,
     this type is defined here unlike other record types. *)

  let cty_float = Coretype.mk_prim `float

  let module_name = "Point2"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      record_decl "ex_nested_point2" [
        record_field "x" cty_float ~doc:(doc "x field");
        record_field "y" cty_float ~doc:(doc "y field");
      ] ~doc:(doc "definition of ex_nested_point2 type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_nested_point2", Util.FwrtTypeEnv.(
      init
      |> bind_object "ex_nested_point2" [
        field "x" cty_float;
        field "y" cty_float ])

  let ts_ast : ts_ast option = None
  let expected_json_shape_explanation = None
  let schema_object = None
end

module Record : Util.Ex_desc = struct
  let module_name = "Record"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      record_decl "ex_nested_record" [
        (* nested alias *)
        record_field_nested "unit" (decl (module Ex_alias.Unit))
          ~codec:(open_ "Ex_alias")
          ~doc:(doc "unit field");

        (* nested record *)
        record_field_nested "point2" (decl (module Point2))
          ~configs:[
            (* Nested type does not inherit mangling style. *)
            Json_config.no_mangling;
          ]
          ~doc:(doc "point2 field");

        (* nested spread record *)
        record_field_nested "point2_spread" (decl (module Point2))
          ~configs:[
            Json_config.nested_field_style `spreading;
            Json_config.no_mangling;
          ]
          ~doc:(doc "spread point2_spread field");

        (* nested variant *)
        record_field_nested "person" (decl (module Ex_mangling.Person_inherited))
          ~codec:(open_ "Ex_mangling")
          ~doc:(doc "person field");

        (* nested optional variant *)
        record_field_nested "optional_variant" (decl (module Ex_optional.Variant))
          ~codec:(open_ "Ex_optional")
          ~doc:(doc "optional_variant field");

        (* nested spread variant *)
        record_field_nested "person_spread" (decl (module Ex_mangling.Person_inherited))
          ~codec:(open_ "Ex_mangling")
          ~configs:[ Json_config.nested_field_style `spreading ]
          ~doc:(doc "person_spread field");
      ] ~doc:(doc "definition of ex_nested_record type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let unit, unit_env = Ex_alias.Unit.fwrt in
    let point2, point2_env = Point2.fwrt in
    let person, person_env = Ex_mangling.Person_inherited.fwrt in
    let optional_variant, optional_variant_env = Ex_optional.Variant.fwrt in
    "ex_nested_record", Util.FwrtTypeEnv.(
    init
    |> union unit_env
    |> union point2_env
    |> union person_env
    |> union optional_variant_env
    |> bind_object "ex_nested_record" [
      field_nested ~codec:(`open_ "Ex_alias_gen") "unit" unit;

      field_nested "point2" point2
        ~configs:[ Json_config.no_mangling ];

      field_nested "point2_spread" point2
        ~configs:[
          Json_config.nested_field_style `spreading;
          Json_config.no_mangling;
        ];

      field_nested ~codec:(`open_ "Ex_mangling_gen") "person" person;

      field_nested ~codec:(`open_ "Ex_optional_gen") "optional_variant" optional_variant;

      field_nested ~codec:(`open_ "Ex_mangling_gen") "person_spread" person
        ~configs:[ Json_config.nested_field_style `spreading ];
    ]
  )

  let json_name = "ExNestedRecord"

  let ts_ast : ts_ast option =
    Some [
      `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = json_name;
          tsa_type_parameters = [];
          tsa_type_desc = `intersection [
            `type_literal Util.Ts_ast.[
              property "unit" (`type_reference "ExAliasUnit");
              property "point2" (`type_reference "ExNestedPoint2");
              property "person" (`type_reference "ex_mangling_person_inherited");
              property "optionalVariant" (`type_reference "ExOptionalVariant");
            ];
            `type_reference "ExNestedPoint2";
            `type_reference "ex_mangling_person_inherited";
          ]; }
    ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
              ("ExNestedRecord",
                (`anyone_of
                  [`object_of
                      [`mandatory_field
                        ("unit",
                          (`named
                              ("ExAliasUnit",
                                (`special ("unit", (`exactly `null))))));
                      `mandatory_field
                        ("point2",
                          (`named
                            ("ExNestedPoint2",
                              (`object_of
                                  [`mandatory_field ("x", `proper_float);
                                  `mandatory_field ("y", `proper_float)]))));
                      `mandatory_field ("x", `proper_float);
                      `mandatory_field ("y", `proper_float);
                      `mandatory_field
                        ("person",
                          (`named
                            ("ex_mangling_person_inherited",
                              (`anyone_of
                                  [`object_of
                                    [`mandatory_field
                                        ("kind", (`exactly (`str "Anonymous")))];
                                  `object_of
                                    [`mandatory_field
                                      ("kind", (`exactly (`str "With_id")));
                                    `mandatory_field ("value", `integral)];
                                  `object_of
                                    [`mandatory_field
                                      ("kind", (`exactly (`str "student")));
                                    `mandatory_field ("student_id", `integral);
                                    `mandatory_field ("name", `string);
                                    `mandatory_field
                                      ("caseValue",
                                        (`string_enum ["Case_at0"; "case-at1"]))];
                                  `object_of
                                    [`mandatory_field
                                      ("kind", (`exactly (`str "Teacher")));
                                    `mandatory_field ("facultyId", `integral);
                                    `mandatory_field ("name", `string);
                                    `mandatory_field ("department", `string)]]))));
                      `mandatory_field
                        ("optionalVariant",
                          (`named
                            ("ExOptionalVariant",
                              (`anyone_of
                                  [`object_of
                                    [`mandatory_field
                                        ("tag", (`exactly (`str "tuple-like")));
                                    `optional_field ("arg", `integral)];
                                  `object_of
                                    [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "tuple-like-alias")));
                                    `optional_field ("arg", `integral)];
                                  `object_of
                                    [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "tuple-like-obj")));
                                    `optional_field ("_0", `integral);
                                    `optional_field ("_1", `integral)];
                                  `object_of
                                    [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "tuple-like-spreading")));
                                    `optional_field ("xOpt", `integral);
                                    `optional_field ("yOpt", `integral)];
                                  `object_of
                                    [`mandatory_field
                                      ("tag", (`exactly (`str "inline-record")));
                                    `optional_field ("intOpt", `integral);
                                    `optional_field ("xOpt", `integral);
                                    `optional_field ("yOpt", `integral);
                                    `mandatory_field
                                      ("objtuple",
                                        (`object_of
                                          [`optional_field ("_0", `integral);
                                          `optional_field ("_1", `integral)]))];
                                  `object_of
                                    [`mandatory_field
                                      ("tag",
                                        (`exactly
                                            (`str "inline-record-spreading")));
                                    `optional_field ("intOpt", `integral);
                                    `optional_field ("xOpt", `integral);
                                    `optional_field ("yOpt", `integral)];
                                  `object_of
                                    [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "reused-inline-record")));
                                    `optional_field ("xOpt", `integral);
                                    `optional_field ("yOpt", `integral)]]))));
                      `mandatory_field ("kind", (`exactly (`str "Anonymous")))];
                  `object_of
                    [`mandatory_field
                        ("unit",
                          (`named
                            ("ExAliasUnit",
                              (`special ("unit", (`exactly `null))))));
                    `mandatory_field
                      ("point2",
                        (`named
                            ("ExNestedPoint2",
                              (`object_of
                                [`mandatory_field ("x", `proper_float);
                                `mandatory_field ("y", `proper_float)]))));
                    `mandatory_field ("x", `proper_float);
                    `mandatory_field ("y", `proper_float);
                    `mandatory_field
                      ("person",
                        (`named
                            ("ex_mangling_person_inherited",
                              (`anyone_of
                                [`object_of
                                    [`mandatory_field
                                      ("kind", (`exactly (`str "Anonymous")))];
                                `object_of
                                  [`mandatory_field
                                      ("kind", (`exactly (`str "With_id")));
                                  `mandatory_field ("value", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("kind", (`exactly (`str "student")));
                                  `mandatory_field ("student_id", `integral);
                                  `mandatory_field ("name", `string);
                                  `mandatory_field
                                    ("caseValue",
                                      (`string_enum ["Case_at0"; "case-at1"]))];
                                `object_of
                                  [`mandatory_field
                                      ("kind", (`exactly (`str "Teacher")));
                                  `mandatory_field ("facultyId", `integral);
                                  `mandatory_field ("name", `string);
                                  `mandatory_field ("department", `string)]]))));
                    `mandatory_field
                      ("optionalVariant",
                        (`named
                            ("ExOptionalVariant",
                              (`anyone_of
                                [`object_of
                                    [`mandatory_field
                                      ("tag", (`exactly (`str "tuple-like")));
                                    `optional_field ("arg", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "tuple-like-alias")));
                                  `optional_field ("arg", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag", (`exactly (`str "tuple-like-obj")));
                                  `optional_field ("_0", `integral);
                                  `optional_field ("_1", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "tuple-like-spreading")));
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag", (`exactly (`str "inline-record")));
                                  `optional_field ("intOpt", `integral);
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral);
                                  `mandatory_field
                                    ("objtuple",
                                      (`object_of
                                          [`optional_field ("_0", `integral);
                                          `optional_field ("_1", `integral)]))];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly
                                          (`str "inline-record-spreading")));
                                  `optional_field ("intOpt", `integral);
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "reused-inline-record")));
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral)]]))));
                    `mandatory_field ("kind", (`exactly (`str "With_id")));
                    `mandatory_field ("value", `integral)];
                  `object_of
                    [`mandatory_field
                        ("unit",
                          (`named
                            ("ExAliasUnit",
                              (`special ("unit", (`exactly `null))))));
                    `mandatory_field
                      ("point2",
                        (`named
                            ("ExNestedPoint2",
                              (`object_of
                                [`mandatory_field ("x", `proper_float);
                                `mandatory_field ("y", `proper_float)]))));
                    `mandatory_field ("x", `proper_float);
                    `mandatory_field ("y", `proper_float);
                    `mandatory_field
                      ("person",
                        (`named
                            ("ex_mangling_person_inherited",
                              (`anyone_of
                                [`object_of
                                    [`mandatory_field
                                      ("kind", (`exactly (`str "Anonymous")))];
                                `object_of
                                  [`mandatory_field
                                      ("kind", (`exactly (`str "With_id")));
                                  `mandatory_field ("value", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("kind", (`exactly (`str "student")));
                                  `mandatory_field ("student_id", `integral);
                                  `mandatory_field ("name", `string);
                                  `mandatory_field
                                    ("caseValue",
                                      (`string_enum ["Case_at0"; "case-at1"]))];
                                `object_of
                                  [`mandatory_field
                                      ("kind", (`exactly (`str "Teacher")));
                                  `mandatory_field ("facultyId", `integral);
                                  `mandatory_field ("name", `string);
                                  `mandatory_field ("department", `string)]]))));
                    `mandatory_field
                      ("optionalVariant",
                        (`named
                            ("ExOptionalVariant",
                              (`anyone_of
                                [`object_of
                                    [`mandatory_field
                                      ("tag", (`exactly (`str "tuple-like")));
                                    `optional_field ("arg", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "tuple-like-alias")));
                                  `optional_field ("arg", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag", (`exactly (`str "tuple-like-obj")));
                                  `optional_field ("_0", `integral);
                                  `optional_field ("_1", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "tuple-like-spreading")));
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag", (`exactly (`str "inline-record")));
                                  `optional_field ("intOpt", `integral);
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral);
                                  `mandatory_field
                                    ("objtuple",
                                      (`object_of
                                          [`optional_field ("_0", `integral);
                                          `optional_field ("_1", `integral)]))];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly
                                          (`str "inline-record-spreading")));
                                  `optional_field ("intOpt", `integral);
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "reused-inline-record")));
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral)]]))));
                    `mandatory_field ("kind", (`exactly (`str "student")));
                    `mandatory_field ("student_id", `integral);
                    `mandatory_field ("name", `string);
                    `mandatory_field
                      ("caseValue", (`string_enum ["Case_at0"; "case-at1"]))];
                  `object_of
                    [`mandatory_field
                        ("unit",
                          (`named
                            ("ExAliasUnit",
                              (`special ("unit", (`exactly `null))))));
                    `mandatory_field
                      ("point2",
                        (`named
                            ("ExNestedPoint2",
                              (`object_of
                                [`mandatory_field ("x", `proper_float);
                                `mandatory_field ("y", `proper_float)]))));
                    `mandatory_field ("x", `proper_float);
                    `mandatory_field ("y", `proper_float);
                    `mandatory_field
                      ("person",
                        (`named
                            ("ex_mangling_person_inherited",
                              (`anyone_of
                                [`object_of
                                    [`mandatory_field
                                      ("kind", (`exactly (`str "Anonymous")))];
                                `object_of
                                  [`mandatory_field
                                      ("kind", (`exactly (`str "With_id")));
                                  `mandatory_field ("value", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("kind", (`exactly (`str "student")));
                                  `mandatory_field ("student_id", `integral);
                                  `mandatory_field ("name", `string);
                                  `mandatory_field
                                    ("caseValue",
                                      (`string_enum ["Case_at0"; "case-at1"]))];
                                `object_of
                                  [`mandatory_field
                                      ("kind", (`exactly (`str "Teacher")));
                                  `mandatory_field ("facultyId", `integral);
                                  `mandatory_field ("name", `string);
                                  `mandatory_field ("department", `string)]]))));
                    `mandatory_field
                      ("optionalVariant",
                        (`named
                            ("ExOptionalVariant",
                              (`anyone_of
                                [`object_of
                                    [`mandatory_field
                                      ("tag", (`exactly (`str "tuple-like")));
                                    `optional_field ("arg", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "tuple-like-alias")));
                                  `optional_field ("arg", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag", (`exactly (`str "tuple-like-obj")));
                                  `optional_field ("_0", `integral);
                                  `optional_field ("_1", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "tuple-like-spreading")));
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag", (`exactly (`str "inline-record")));
                                  `optional_field ("intOpt", `integral);
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral);
                                  `mandatory_field
                                    ("objtuple",
                                      (`object_of
                                          [`optional_field ("_0", `integral);
                                          `optional_field ("_1", `integral)]))];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly
                                          (`str "inline-record-spreading")));
                                  `optional_field ("intOpt", `integral);
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral)];
                                `object_of
                                  [`mandatory_field
                                      ("tag",
                                        (`exactly (`str "reused-inline-record")));
                                  `optional_field ("xOpt", `integral);
                                  `optional_field ("yOpt", `integral)]]))));
                    `mandatory_field ("kind", (`exactly (`str "Teacher")));
                    `mandatory_field ("facultyId", `integral);
                    `mandatory_field ("name", `string);
                    `mandatory_field ("department", `string)]]))))
    )

  let schema_object : Schema_object.t option =
    let open Schema_object in
    let person_ctors =
      [ "Anonymous", [];
        "With_id", [ "value", integer () ];
        "student", [
          "student_id", integer ();
          "name", string ();
          "caseValue", string () ~enum:[
            `str "Case_at0"; `str "case-at1"
          ];
        ];
        "Teacher", [
          "facultyId", integer ();
          "name", string ();
          "department", string ();
        ]
      ] |&> (fun (name, fields) ->
        (name, fields @ [ "kind", string () ~enum:[`str name]]))
    in
    let optional_variant = 
      [ "tuple-like", [
          "arg", integer () ~nullable:true;
        ];
        "tuple-like-alias", [
          "arg", integer () ~nullable:true;
        ];
        "tuple-like-obj", [
          "_0", integer () ~nullable:true;
          "_1", integer () ~nullable:true;
        ];
        "tuple-like-spreading", [
          "xOpt", integer () ~nullable:true;
          "yOpt", integer () ~nullable:true;
        ];
        "inline-record", [
          "intOpt", integer () ~nullable:true;
          "xOpt", integer () ~nullable:true;
          "yOpt", integer () ~nullable:true;
          "objtuple", record [
            "_0", integer () ~nullable:true;
            "_1", integer () ~nullable:true;
          ];
        ];
        "inline-record-spreading", [
          "intOpt", integer () ~nullable:true;
          "xOpt", integer () ~nullable:true;
          "yOpt", integer () ~nullable:true;
        ];
        "reused-inline-record", [
          "xOpt", integer () ~nullable:true;
          "yOpt", integer () ~nullable:true;
        ]
      ]
      |&> (fun (title, fields) ->
        record ~title & fields @ [ "tag", string () ~enum:[`str title]])
      |> oneOf
    in
    let fields =
      ("unit", integer () ~minimum:1 ~maximum:1)
      :: ("point2", record [
        "x", number ();
        "y", number ();
      ]) :: [
        "x", number ();
        "y", number ();
        "person", (person_ctors
          |&> (fun (title, fields) -> record ~title fields)
          |> oneOf);
        "optionalVariant", optional_variant;
      ]
    in
    oneOf ~schema ~title:json_name ~id:("#"^json_name) (
      person_ctors |&> (fun (_, fs) ->
        record (fields @ fs)))
    |> Option.some
end

module Variant : Util.Ex_desc = struct
  let discriminator = "tag"
  let arg_fname = "arg"

  let variant_configs : [`type_decl] configs = [
    Json_config.variant_discriminator discriminator;
    Json_config.name_of_variant_arg `arg;
  ]

  let module_name = "Variant"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      variant_decl "ex_nested_variant" [
        (* nested record of inline_record *)
        variant_constructor "Student1" (`inline_record [
          record_field_nested ~codec:(open_ "Ex_record") "student" (decl (module Ex_record.Student))
            ~configs:[ Json_config.no_mangling ];
        ]) ~doc:(doc "Student1 constructor");

        (* nested spread record of inline_record *)
        variant_constructor "Student2" (`inline_record [
          record_field_nested ~codec:(open_ "Ex_record") "student" (decl (module Ex_record.Student))
            ~configs:[
              Json_config.nested_field_style `spreading;
              Json_config.no_mangling;
            ];
        ]) ~doc:(doc "Student2 constructor");

        (* nested record of tuple_like *)
        variant_constructor "Student3" (`tuple_like [
          variant_argument_nested ~codec:(open_ "Ex_record") (decl (module Ex_record.Student))
            ~configs:[ Json_config.no_mangling ];
        ]) ~doc:(doc "Student3 constructor");

        (* nested spread record of tuple_like *)
        variant_constructor "Student4" (`tuple_like [
          variant_argument_nested ~codec:(open_ "Ex_record") (decl (module Ex_record.Student))
            ~configs:[
              Json_config.nested_field_style `spreading;
              Json_config.no_mangling;
            ];
        ]) ~doc:(doc "Student4 constructor");

        (* nested variant of tuple_like *)
        variant_constructor "Int_list1" (`tuple_like [
          variant_argument_nested ~codec:(open_ "Ex_variant") (decl (module Ex_variant.Int_list))
            ~configs:[ Json_config.no_mangling ];
        ]) ~doc:(doc "Int_list1 constructor");

        (* nested spread variant of tuple_like*)
        variant_constructor "Int_list2" (`tuple_like [
          variant_argument_nested ~codec:(open_ "Ex_variant") (decl (module Ex_variant.Int_list))
            ~configs:[
              Json_config.nested_field_style `spreading;
              Json_config.no_mangling;
            ];
        ]) ~doc:(doc "Int_list2 constructor");
      ] ~configs:variant_configs ~doc:(doc "definition of ex_nested_variant type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let student, student_env = Ex_record.Student.fwrt in
    let int_list, int_list_env = Ex_variant.Int_list.fwrt in
    let parent = "ex_nested_variant" in
    parent, Util.FwrtTypeEnv.(
      init
      |> union student_env
      |> union int_list_env
      |> bind_object ~configs:variant_configs parent []
      |> bind_constructor ~parent "Student1" ~fields:[
          field_nested ~codec:(`open_ "Ex_record_gen") "student" student;
        ]

      |> bind_constructor ~parent "Student2" ~fields:[
          field_nested ~codec:(`open_ "Ex_record_gen") "student" student
            ~configs:[ Json_config.nested_field_style `spreading ];
        ]

      |> bind_constructor ~parent "Student3" ~args:[
          variant_argument_nested ~codec:(`open_ "Ex_record_gen") student;
        ]

      |> bind_constructor ~parent "Student4" ~args:[
          variant_argument_nested ~codec:(`open_ "Ex_record_gen") student
            ~configs:[ Json_config.nested_field_style `spreading ];
        ]

      |> bind_constructor ~parent "Int_list1" ~args:[
          variant_argument_nested ~codec:(`open_ "Ex_variant_gen") int_list;
        ]

      |> bind_constructor ~parent "Int_list2" ~args:[
          variant_argument_nested ~codec:(`open_ "Ex_variant_gen") int_list
            ~configs:[ Json_config.nested_field_style `spreading ]
        ]
    )

  let json_name = "ExNestedVariant"

  let ts_ast : ts_ast option =
    let discriminator_value kind =
      Util.Ts_ast.property discriminator (`literal_type (`string_literal kind))
    in
    let ref_student = `type_reference "ExRecordStudent" in
    let ref_int_list = `type_reference "ExVariantIntList" in
    let student1 =
      `type_literal
        [ discriminator_value "student1";
          Util.Ts_ast.property "student" ref_student ]
    in
    let student2 =
      `intersection [
        `type_literal [ discriminator_value "student2"; ];
        ref_student;
      ]
    in
    let student3 =
      `type_literal
        [ discriminator_value "student3";
          Util.Ts_ast.property arg_fname ref_student ]
    in
    let student4 =
      `intersection [
        `type_literal [ discriminator_value "student4"; ];
        ref_student;
      ]
    in
    let int_list1 =
      `type_literal
        [ discriminator_value "int-list1";
          Util.Ts_ast.property arg_fname ref_int_list ]
    in
    let int_list2 =
      `intersection [
        `type_literal [ discriminator_value "int-list2" ];
         ref_int_list ]
    in
    let nested_variant = [
      "Int_list1", int_list1;
      "Int_list2", int_list2;
      "Student1", student1;
      "Student2", student2;
      "Student3", student3;
      "Student4", student4;
    ] in
    let options : Util.Ts_ast.options =
      { discriminator;
        var_v = "__bindoj_v";
        var_x = "__bindoj_x";
        var_fns = "__bindoj_fns";
        ret = "__bindoj_ret" } in
    Some
      [ `type_alias_declaration
          { tsa_modifiers = [`export];
            tsa_name = json_name;
            tsa_type_parameters = [];
            tsa_type_desc = `union (List.map snd nested_variant); };
        Util.Ts_ast.case_analyzer json_name ("analyze"^json_name) options nested_variant; ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
      ("not considering any config if exists",
        (`named
            ("ExNestedVariant",
              (`anyone_of
                [`object_of
                    [`mandatory_field ("tag", (`exactly (`str "student1")));
                    `mandatory_field
                      ("student",
                        (`named
                          ("ExRecordStudent",
                            (`object_of
                                [`mandatory_field ("admissionYear", `integral);
                                `mandatory_field ("name", `string)]))))];
                `object_of
                  [`mandatory_field ("tag", (`exactly (`str "student2")));
                  `mandatory_field ("admissionYear", `integral);
                  `mandatory_field ("name", `string)];
                `object_of
                  [`mandatory_field ("tag", (`exactly (`str "student3")));
                  `mandatory_field
                    ("arg",
                      (`named
                          ("ExRecordStudent",
                            (`object_of
                              [`mandatory_field ("admissionYear", `integral);
                              `mandatory_field ("name", `string)]))))];
                `object_of
                  [`mandatory_field ("tag", (`exactly (`str "student4")));
                  `mandatory_field ("admissionYear", `integral);
                  `mandatory_field ("name", `string)];
                `object_of
                  [`mandatory_field ("tag", (`exactly (`str "int-list1")));
                  `mandatory_field
                    ("arg",
                      (`named
                          ("ExVariantIntList",
                            (`anyone_of
                              [`object_of
                                  [`mandatory_field
                                    ("kind", (`exactly (`str "intnil")))];
                              `object_of
                                [`mandatory_field
                                    ("kind", (`exactly (`str "intcons")));
                                `mandatory_field
                                  ("value", (`tuple_of [`integral; `self]))]]))))];
                `object_of
                  [`mandatory_field ("tag", (`exactly (`str "int-list2")));
                  `mandatory_field ("kind", (`exactly (`str "intnil")))];
                `object_of
                  [`mandatory_field ("tag", (`exactly (`str "int-list2")));
                  `mandatory_field ("kind", (`exactly (`str "intcons")));
                  `mandatory_field ("value", (`tuple_of [`integral; `self]))]]))))
    )

  let schema_object : Schema_object.t option =
    let student_fields = Schema_object.[
      "admissionYear", integer ();
      "name", string ()
    ] in
    let int_list_fields = Schema_object.[
        "intnil", [];
        "intcons", [ "value", tuple [ integer(); ref "#ExVariantIntList"; ] ]
      ]
    in
    let constructor_fields name fields = fields @ [
      discriminator, Schema_object.string () ~enum:[`str name]
    ] in
    Schema_object.(
      oneOf ~schema ~title:json_name ~id:("#"^json_name) [
        record ~title:"student1" @@ constructor_fields "student1" [
          "student", record student_fields;
        ];
        record ~title:"student2" @@ constructor_fields "student2" student_fields;
        record ~title:"student3" @@ constructor_fields "student3" [
          arg_fname, record student_fields;
        ];
        record ~title:"student4" @@ constructor_fields "student4" student_fields;
        record ~title:"int-list1" @@ constructor_fields "int-list1" [
          arg_fname, oneOf (int_list_fields |&> fun (name, fields) ->
            record ~title:name (fields @ [
              "kind", string () ~enum:[`str name]]));
        ];
        record ~title:"int-list2_intnil" @@ constructor_fields "int-list2" [
          "kind", string () ~enum:[`str "intnil" ];
        ];
        record ~title:"int-list2_intcons" @@ constructor_fields "int-list2" [
          "value", tuple [ integer(); ref "#ExVariantIntList" ];
          "kind", string () ~enum:[`str "intcons" ];
        ];
      ]
    )
    |> Option.some
end

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex_nested"

let example_descs : (module Util.Ex_desc) list = [
  (module Point2);
  (module Record);
  (module Variant);
]
