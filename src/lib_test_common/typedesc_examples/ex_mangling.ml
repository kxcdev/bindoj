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
open Bindoj_codec_config
open Bindoj_gen_ts.Typescript_datatype
open Bindoj_openapi.V3

open struct
  let cty_int = Coretype.mk_prim `int
  let cty_string = Coretype.mk_prim `string
end

module Student_inherited : Util.Ex_desc = struct
  let cty_enum = Coretype.(mk_string_enum [
    string_enum_case "Case_at0";
    string_enum_case "case_at1"
      ~configs:[ Json_config.no_mangling ];
  ])

  let module_name = "Student_inherited"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      record_decl "ex_mangling_student_inherited" [
        record_field "admission_year" cty_int ~doc:(doc "addmission_year field");
        record_field "name" cty_string
          ~configs:[ Json_config.default_mangling ]
          ~doc:(doc "name field");
        record_field "case_value" cty_enum
          ~configs:[ Json_config.default_mangling ]
          ~doc:(doc "case_value field");
      ] ~configs:[ Json_config.no_mangling ]
        ~doc:(doc "definition of ex_mangling_student_inherited type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_mangling_student_inherited", Util.FwrtTypeEnv.(
      init
      |> bind_object "ex_mangling_student_inherited"
        [ field "admission_year" cty_int;
        field "name" cty_string
          ~configs:[ Json_config.default_mangling ];
        field "case_value" cty_enum
          ~configs:[ Json_config.default_mangling ]; ]
        ~configs:[ Json_config.no_mangling ])

  let json_name = "ex_mangling_student_inherited"

  let ts_ast : ts_ast option =
    Some
      [ `type_alias_declaration
          { tsa_modifiers = [`export];
            tsa_name = json_name;
            tsa_type_parameters = [];
            tsa_type_desc =
            `type_literal
              Util.Ts_ast.[
                property "admission_year" (`type_reference "number");
                property "name" (`type_reference "string");
                property "caseValue" (`union [
                    `literal_type (`string_literal "Case-at0");
                    `literal_type (`string_literal "case_at1");
                  ]);
              ]; } ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
      ("not considering any config if exists",
        (`named
            (json_name,
              (`object_of
                [`mandatory_field ("admission_year", `integral);
                `mandatory_field ("name", `string);
                `mandatory_field
                  ("caseValue", (`string_enum ["Case-at0"; "case_at1"]))]))))
    )

  let schema_object : Schema_object.t option =
    Some Schema_object.(
    record ~schema
      ~title:json_name
      ~id:("#"^json_name)
      [ "admission_year", integer ();
        "name", string ();
        "caseValue", string () ~enum:[
          `str "Case-at0"; `str "case_at1"
        ] ]
  )
end

module Person_no_mangling : Util.Ex_desc = struct
  let module_name = "Person_no_mangling"

  include Util.Make_ex_decls(struct
    let make_decl (module D : Util.With_docstr) : type_decl =
      let open D in
      variant_decl "ex_mangling_person_no_mangling" [
        variant_constructor "Anonymous" `no_param
          ~configs:[ Json_config.no_mangling ]
          ~doc:(doc "Anonymous constructor");

        variant_constructor "With_id" (`tuple_like [variant_argument cty_int])
          ~configs:[ Json_config.no_mangling ]
          ~doc:(doc "With_id constructor");

        variant_constructor "Student" (`inline_record [
          record_field "student_id" cty_int
            ~configs:[ Json_config.no_mangling ]
            ~doc:(doc "student_id field in Student constructor");
          record_field "name" cty_string
            ~configs:[ Json_config.no_mangling ]
            ~doc:(doc "name field in Student constructor");
        ]) ~configs:[ Json_config.no_mangling ] ~doc:(doc "Student constructor");

        variant_constructor "Teacher" (`inline_record [
          record_field "faculty_id" cty_int
            ~configs:[ Json_config.no_mangling ]
            ~doc:(doc "faculty_id field in Teacher constructor");
          record_field "name" cty_string
            ~configs:[ Json_config.no_mangling ]
            ~doc:(doc "name field in Teacher constructor");
          record_field "department" cty_string
            ~configs:[ Json_config.no_mangling ]
            ~doc:(doc "dapartment field in Teacher constructor");
        ]) ~configs:[ Json_config.no_mangling ] ~doc:(doc "Teacher constructor")

      ] ~configs:[ Json_config.no_mangling ] ~doc:(doc "definition of ex_mangling_person_no_mangling type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let parent = "ex_mangling_person_no_mangling" in
    "ex_mangling_person_no_mangling", Util.FwrtTypeEnv.(
      init
      |> bind_object "ex_mangling_person_no_mangling" [] ~configs:[ Json_config.no_mangling ]
      |> bind_constructor ~parent "Anonymous" ~configs:[ Json_config.no_mangling ]
      |> bind_constructor ~parent "With_id" ~args:[variant_argument cty_int] ~configs:[ Json_config.no_mangling ]
      |> bind_constructor ~parent "Student" ~fields:[
        field "student_id" cty_int ~configs:[ Json_config.no_mangling ];
        field "name" cty_string ~configs:[ Json_config.no_mangling ] ]
        ~configs:[ Json_config.no_mangling ]
      |> bind_constructor ~parent "Teacher" ~fields:[
        field "faculty_id" cty_int ~configs:[ Json_config.no_mangling ];
        field "name" cty_string ~configs:[ Json_config.no_mangling ];
        field "department" cty_string ~configs:[ Json_config.no_mangling ] ]
        ~configs:[ Json_config.no_mangling ]
    )

  let json_name = "ex_mangling_person_no_mangling"

  let ts_ast : ts_ast option =
    let discriminator = "kind" in
    let arg_fname = "value" in
    let discriminator_value kind =
      Util.Ts_ast.property discriminator (`literal_type (`string_literal kind))
    in
    let anonymous =
      `type_literal
        [ discriminator_value "Anonymous" ] in
    let with_id =
      `type_literal
        [ discriminator_value "With_id";
          Util.Ts_ast.property arg_fname (`type_reference "number") ] in
    let student =
      `type_literal
        Util.Ts_ast.[
          discriminator_value "Student";
          property "student_id" (`type_reference "number");
          property "name" (`type_reference "string") ] in
    let teacher =
      `type_literal
        Util.Ts_ast.[
          discriminator_value "Teacher";
          property "faculty_id" (`type_reference "number");
          property"name" (`type_reference "string");
          property"department" (`type_reference "string") ] in
    let person = [
      "With_id", with_id;
      "Teacher", teacher;
      "Student", student;
      "Anonymous", anonymous;
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
            tsa_type_desc = `union (List.map snd person); };
        Util.Ts_ast.case_analyzer json_name ("analyze_"^json_name) options person; ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            (json_name,
              (`anyone_of
                  [`object_of
                    [`mandatory_field ("kind", (`exactly (`str "Anonymous")))];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "With_id")));
                    `mandatory_field ("value", `integral)];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "Student")));
                    `mandatory_field ("student_id", `integral);
                    `mandatory_field ("name", `string)];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "Teacher")));
                    `mandatory_field ("faculty_id", `integral);
                    `mandatory_field ("name", `string);
                    `mandatory_field ("department", `string)]]))))
    )

  let schema_object : Schema_object.t option =
    Util.Schema_object.variant json_name
      Schema_object.[
        "Anonymous", [];
        "With_id", [ "value", integer () ];
        "Student", [
          "student_id", integer ();
          "name", string ();
        ];
        "Teacher", [
          "faculty_id", integer ();
          "name", string ();
          "department", string ();
        ]; ]
    |> Option.some
end

module Person_inherited : Util.Ex_desc = struct
  let cty_enum = Coretype.(mk_string_enum [
    string_enum_case "Case_at0";
    string_enum_case "case_at1"
      ~configs:[ Json_config.default_mangling ];
  ] ~configs:[ Json_config.no_mangling ])

  let module_name = "Person_inherited"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      variant_decl "ex_mangling_person_inherited" [
        variant_constructor "Anonymous" `no_param
          ~doc:(doc "Anonymous constructor");

        variant_constructor "With_id" (`tuple_like [variant_argument cty_int])
          ~doc:(doc "With_id constructor");

        variant_constructor "Student" (`inline_record [
          record_field "student_id" cty_int  ~configs:[ Json_config.no_mangling ]
            ~doc:(doc "student_id field in Student constructor");
          record_field "name" cty_string
            ~doc:(doc "name field in Student constructor");
          record_field "case_value" cty_enum
            ~doc:(doc "case_value field in Student constructor")
        ]) ~configs:[ Json_config.default_mangling ]
          ~doc:(doc "Student constructor");

        variant_constructor "Teacher" (`reused_inline_record (decl (module Ex_record.Teacher)))
          ~doc:(doc "Teacher constructor")
          ~configs: [
            Json_config.no_mangling;
            Ts_config.reused_variant_inline_record_style `inline_fields;
          ]
      ] ~configs:[ Json_config.no_mangling ]
        ~doc:(doc "definition of ex_mangling_person_inherited type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let parent = "ex_mangling_person_inherited" in
    parent, Util.FwrtTypeEnv.(
      init
      |> bind_object parent [] ~configs:[ Json_config.no_mangling ]
      |> bind_constructor ~parent "Anonymous"
      |> bind_constructor ~parent "With_id" ~args:[variant_argument cty_int]
      |> bind_constructor ~parent "Student" ~fields:[
        field "student_id" cty_int ~configs:[ Json_config.no_mangling ];
        field "name" cty_string;
        field "case_value" cty_enum;
      ] ~configs:[ Json_config.default_mangling ]
      |> bind_constructor ~parent
        ~annot_kc:(Some (Tfcki_reused_variant_inline_record Ex_record.Teacher.decl))
        "Teacher" ~fields:[
        field "faculty_id" cty_int;
        field "name" cty_string;
        field "department" cty_string]
        ~configs: [
          Json_config.no_mangling;
          Ts_config.reused_variant_inline_record_style `inline_fields;
        ]
    )

  let json_name = "ex_mangling_person_inherited"

  let ts_ast : ts_ast option =
    let discriminator = "kind" in
    let arg_fname = "value" in
    let discriminator_value kind =
      Util.Ts_ast.property discriminator (`literal_type (`string_literal kind))
    in
    let anonymous =
      `type_literal
        [ discriminator_value "Anonymous" ] in
    let with_id =
      `type_literal
        [ discriminator_value "With_id";
          Util.Ts_ast.property arg_fname (`type_reference "number") ] in
    let student =
      `type_literal
        Util.Ts_ast.[
          discriminator_value "student";
          property "caseValue" (`union [
              `literal_type (`string_literal "Case_at0");
              `literal_type (`string_literal "case-at1");
            ]);
          property "name" (`type_reference "string");
          property "student_id" (`type_reference "number");

        ] in
    let teacher =
      `type_literal
        Util.Ts_ast.[
          discriminator_value "Teacher";
          property "department" (`type_reference "string");
          property "facultyId" (`type_reference "number");
          property "name" (`type_reference "string");
        ] in
    let person = [
      "With_id", with_id;
      "Teacher", teacher;
      "Student", student;
      "Anonymous", anonymous;
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
            tsa_type_desc = `union (List.map snd person); };
        Util.Ts_ast.case_analyzer json_name ("analyze_"^json_name) options person; ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
              (json_name,
                (`anyone_of
                  [`object_of
                      [`mandatory_field ("kind", (`exactly (`str "Anonymous")))];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "With_id")));
                    `mandatory_field ("value", `integral)];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "student")));
                    `mandatory_field ("student_id", `integral);
                    `mandatory_field ("name", `string);
                    `mandatory_field
                      ("caseValue", (`string_enum ["Case_at0"; "case-at1"]))];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "Teacher")));
                    `mandatory_field ("facultyId", `integral);
                    `mandatory_field ("name", `string);
                    `mandatory_field ("department", `string)]]))))
    )

  let schema_object : Schema_object.t option =
    Util.Schema_object.variant json_name
      Schema_object.[
        "Anonymous", [];
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
        ]; ]
    |> Option.some
end

module Enum : Util.Ex_desc = struct
  let cty_enum doc = Coretype.(mk_string_enum [
    string_enum_case "Case_at0" ~doc:(doc "zeroth case");
    string_enum_case "case_at1"
      ~configs:[ Json_config.default_mangling ]
      ~doc:(doc "first case");
    string_enum_case "Case_at2"
      ~configs:[ Json_config.default_mangling ]
      ~doc:(doc "second case");
    string_enum_case "Case_at3"
      ~configs:[ Json_config.name "Case-third"; Json_config.default_mangling ]
      ~doc:(doc "third case");
  ] ~configs:[ Json_config.no_mangling ])

  let module_name = "Enum"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      alias_decl "ex_mangling_enum" (cty_enum doc) ~doc:(doc "alias of string cases")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    "ex_mangling_enum", Util.FwrtTypeEnv.(
      init
      |> bind_alias "ex_mangling_enum" (cty_enum (constant `nodoc))
    )

  let json_name = "ExManglingEnum"

  let ts_ast : ts_ast option = Some [
    `type_alias_declaration {
      tsa_modifiers = [`export];
      tsa_name = json_name;
      tsa_type_parameters = [];
      tsa_type_desc =
        `union [
          `literal_type (`string_literal "Case_at0");
          `literal_type (`string_literal "case-at1");
          `literal_type (`string_literal "Case-at2");
          `literal_type (`string_literal "Case-third");
        ];
    }
  ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
      ("not considering any config if exists",
        (`named (json_name, (`string_enum ["Case_at0"; "case-at1"; "Case-at2"; "Case-third"]))))
    )

  let schema_object : Schema_object.t option = None

end

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex_mangling"

let example_descs : (module Util.Ex_desc) list = [
  (module Student_inherited);
  (module Person_no_mangling);
  (module Person_inherited);
  (module Enum);
]
