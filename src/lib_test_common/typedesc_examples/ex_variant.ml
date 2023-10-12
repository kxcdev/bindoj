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

module Person : Util.Ex_desc = struct
  let module_name = "Person"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      variant_decl "ex_variant_person" [
        variant_constructor "Anonymous" `no_param
          ~doc:(doc "Anonymous constructor");

        variant_constructor "With_id" (`tuple_like [variant_argument cty_int])
          ~doc:(doc "With_id constructor");

        variant_constructor "Student" (`inline_record [
          record_field "student_id" cty_int
            ~doc:(doc "student_id field in Student constructor");
          record_field "name" cty_string
            ~doc:(doc "name field in Student constructor");
        ]) ~doc:(doc "Student constructor");

        variant_constructor "Teacher" (`inline_record [
          record_field "faculty_id" cty_int
            ~doc:(doc "faculty_id field in Teacher constructor");
          record_field "name" cty_string
            ~doc:(doc "name field in Teacher constructor");
          record_field "department" cty_string
            ~doc:(doc "dapartment field in Teacher constructor");
        ]) ~doc:(doc "Teacher constructor")

      ] ~doc:(doc "definition of ex_variant_person type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let parent = "ex_variant_person" in
    parent, Util.FwrtTypeEnv.(
      init
      |> bind_object parent []
      |> bind_constructor ~parent "Anonymous"
      |> bind_constructor ~parent "With_id" ~args:[variant_argument cty_int]
      |> bind_constructor ~parent "Student" ~fields:[
        field "student_id" cty_int;
        field "name" cty_string]
      |> bind_constructor ~parent "Teacher" ~fields:[
        field "faculty_id" cty_int;
        field "name" cty_string;
        field "department" cty_string]
    )

  let json_name = "ExVariantPerson"

  let ts_ast : ts_ast option =
    let discriminator = "kind" in
    let arg_fname = "value" in
    let discriminator_value kind =
      Util.Ts_ast.property discriminator (`literal_type (`string_literal kind))
    in
    let anonymous =
      `type_literal
        [ discriminator_value "anonymous"; ] in
    let with_id =
      `type_literal
        [ discriminator_value "with-id";
          Util.Ts_ast.property arg_fname (`type_reference "number") ] in
    let student =
      `type_literal
        Util.Ts_ast.[
          discriminator_value "student";
          property "name" (`type_reference "string");
          property "studentId" (`type_reference "number");
        ] in
    let teacher =
      `type_literal
        Util.Ts_ast.[
          discriminator_value "teacher";
          property "facultyId" (`type_reference "number");
          property "name" (`type_reference "string");
          property "department" (`type_reference "string") ] in
    let person = [
      "Anonymous", anonymous;
      "Student", student;
      "Teacher", teacher;
      "With_id", with_id;
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
        Util.Ts_ast.case_analyzer json_name ("analyze"^json_name) options person; ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            (json_name,
              (`anyone_of
                  [`object_of
                    [`mandatory_field ("kind", (`exactly (`str "anonymous")))];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "with-id")));
                    `mandatory_field ("value", `integral)];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "student")));
                    `mandatory_field ("studentId", `integral);
                    `mandatory_field ("name", `string)];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "teacher")));
                    `mandatory_field ("facultyId", `integral);
                    `mandatory_field ("name", `string);
                    `mandatory_field ("department", `string)]]))))
    )

  let schema_object : Schema_object.t option =
    Util.Schema_object.variant json_name
      Schema_object.[
        "anonymous", [];
        "with-id", [ "value", integer () ];
        "student", [
          "studentId", integer ();
          "name", string ();
        ];
        "teacher", [
          "facultyId", integer ();
          "name", string ();
          "department", string ();
        ]; ]
    |> Option.some
end

module Person_reused : Util.Ex_desc = struct
  let module_name = "Person_reused"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      variant_decl "ex_variant_person_reused" [
        variant_constructor "Anonymous" `no_param
          ~doc:(doc "Anonymous constructor");

        variant_constructor "With_id" (`tuple_like [variant_argument cty_int])
          ~doc:(doc "With_id constructor");

        variant_constructor "Student" (`inline_record [
          record_field "student_id" cty_int
            ~doc:(doc "student_id field in Student constructor");
          record_field "name" cty_string
            ~doc:(doc "name field in Student constructor");
        ]) ~doc:(doc "Student constructor");

        variant_constructor "Teacher" (`reused_inline_record (decl (module Ex_record.Teacher)))
          ~doc:(doc "Teacher constructor")
          ~configs: [ Ts_config.reused_variant_inline_record_style `intersection_type ]

      ] ~doc:(doc "definition of ex_variant_person_reused type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let parent = "ex_variant_person_reused" in
    parent, Util.FwrtTypeEnv.(
      init
      |> bind_object parent []
      |> bind_constructor ~parent "Anonymous"
      |> bind_constructor ~parent "With_id" ~args:[variant_argument cty_int]
      |> bind_constructor ~parent "Student" ~fields:[
        field "student_id" cty_int;
        field "name" cty_string]
      |> bind_constructor ~parent
        ~annot_kc:(Some (Tfcki_reused_variant_inline_record Ex_record.Teacher.decl))
        "Teacher" ~fields:[
          field "faculty_id" cty_int;
          field "name" cty_string;
          field "department" cty_string ]
        ~configs: [ Ts_config.reused_variant_inline_record_style `intersection_type ]
    )

  let json_name = "ExVariantPersonReused"

  let ts_ast : ts_ast option =
    let discriminator = "kind" in
    let arg_fname = "value" in
    let discriminator_value kind =
      Util.Ts_ast.property discriminator (`literal_type (`string_literal kind))
    in
    let anonymous =
      `type_literal
        [ discriminator_value "anonymous"; ] in
    let with_id =
      `type_literal
        [ discriminator_value "with-id";
          Util.Ts_ast.property arg_fname (`type_reference "number") ] in
    let student =
      `type_literal
        Util.Ts_ast.[
          discriminator_value "student";
          property "studentId" (`type_reference "number");
          property "name" (`type_reference "string") ] in
    let teacher =
      `intersection [
        `type_literal [ discriminator_value "teacher" ];
        `type_reference "ExRecordTeacher";
      ] in
    let person = [
      "Anonymous", anonymous;
      "Student", student;
      "Teacher", teacher;
      "With_id", with_id;
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
        Util.Ts_ast.case_analyzer json_name ("analyze"^json_name) options person; ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            (json_name,
              (`anyone_of
                  [`object_of
                    [`mandatory_field ("kind", (`exactly (`str "anonymous")))];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "with-id")));
                    `mandatory_field ("value", `integral)];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "student")));
                    `mandatory_field ("studentId", `integral);
                    `mandatory_field ("name", `string)];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "teacher")));
                    `mandatory_field ("facultyId", `integral);
                    `mandatory_field ("name", `string);
                    `mandatory_field ("department", `string)]]))))
    )

  let schema_object : Schema_object.t option =
    Util.Schema_object.variant json_name
      Schema_object.[
        "anonymous", [];
        "with-id", [ "value", integer () ];
        "student", [
          "studentId", integer ();
          "name", string ();
        ];
        "teacher", [
          "facultyId", integer ();
          "name", string ();
          "department", string ();
        ]; ]
    |> Option.some
end

module Int_list : Util.Ex_desc = struct
  let module_name = "Int_list"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      variant_decl "ex_variant_int_list" [
        variant_constructor "IntNil" `no_param
          ~doc:(doc "nil for ex_variant_int_list");

        variant_constructor "IntCons" (`tuple_like [
          variant_argument @@ Coretype.mk_prim `int;
          variant_argument @@ Coretype.mk_self ();
        ]) ~doc:(doc "cons for ex_variant_int_list");
      ] ~doc:(doc "definition of ex_variant_int_list type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let parent = "ex_variant_int_list" in
    parent, Util.FwrtTypeEnv.(
      init
      |> bind_object parent []
      |> bind_constructor ~parent "IntNil"
      |> bind_constructor ~parent "IntCons" ~args:[
        variant_argument @@ Coretype.mk_prim `int;
        variant_argument @@ Coretype.mk_self ();
      ]
    )

  let json_name = "ExVariantIntList"

  let ts_ast : ts_ast option =
    let discriminator = "kind" in
    let arg_fname = "value" in
    let int_nil =
      `type_literal
        Util.Ts_ast.[
          property discriminator (`literal_type (`string_literal "intnil")) ] in
    let int_cons =
      `type_literal
        Util.Ts_ast.[
          property discriminator (`literal_type (`string_literal "intcons"));
          property arg_fname (`tuple [ `type_reference "number"; `type_reference json_name; ]) ] in
    let cstrs = ["IntNil", int_nil; "IntCons", int_cons] in
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
            tsa_type_desc = `union (List.map snd cstrs); };
        Util.Ts_ast.case_analyzer json_name ("analyze"^json_name) options cstrs; ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            (json_name,
              (`anyone_of
                  [`object_of
                    [`mandatory_field ("kind", (`exactly (`str "intnil")))];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "intcons")));
                    `mandatory_field ("value", (`tuple_of [`integral; `self]))]]))))
    )

  let schema_object : Schema_object.t option =
    Util.Schema_object.variant json_name
      Schema_object.[
        "intnil", [];
        "intcons", [
          "value", tuple [ integer(); ref ("#"^json_name); ]
        ];
      ]
    |> Option.some
end

module Int_list_objtuple : Util.Ex_desc = struct
  let vc_configs : [`variant_constructor] configs = Json_config.[ tuple_style (`obj `default) ]

  let module_name = "Int_list_objtuple"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      variant_decl "ex_variant_int_list_objtuple" [
        variant_constructor "IntNil" `no_param
          ~doc:(doc "nil for ex_variant_int_list_objtuple");

        variant_constructor "IntCons" (`tuple_like [
          variant_argument cty_int;
          variant_argument @@ Coretype.mk_self ();
        ]) ~configs:vc_configs
          ~doc:(doc "cons for ex_variant_int_list_objtuple");

      ] ~doc:(doc "definition of ex_variant_int_list_objtuple type")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let parent = "ex_variant_int_list_objtuple" in
    "ex_variant_int_list_objtuple", Util.FwrtTypeEnv.(
      init
      |> bind_object "ex_variant_int_list_objtuple" []
      |> bind_constructor ~parent "IntNil"
      |> bind_constructor ~parent "IntCons" ~configs:vc_configs ~args:[
        variant_argument cty_int;
        variant_argument @@ Coretype.mk_self ()]
    )

  let json_name = "ExVariantIntListObjtuple"
  let ts_ast : ts_ast option =
    let discriminator = "kind" in
    let int_nil =
      `type_literal
        Util.Ts_ast.[
          property discriminator (`literal_type (`string_literal "intnil"))] in
    let int_cons =
      `type_literal
        Util.Ts_ast.[
          property discriminator (`literal_type (`string_literal "intcons"));
          property "_0" (`type_reference "number");
          property "_1" (`type_reference json_name) ] in
    let cstrs = ["IntNil", int_nil; "IntCons", int_cons] in
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
            tsa_type_desc = `union (List.map snd cstrs); };
        Util.Ts_ast.case_analyzer json_name ("analyze"^json_name) options cstrs; ]

  let expected_json_shape_explanation = None

  let schema_object : Schema_object.t option =
    Util.Schema_object.variant json_name
      Schema_object.[
        "intnil", [];
        "intcons", [
          "_0", integer ();
          "_1", ref ("#"^json_name)
        ];
      ]
    |> Option.some
end

module Polymorphic : Util.Ex_desc = struct
  let module_name = "Polymorphic"

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      variant_decl "ex_variant_foo" [
        variant_constructor "Foo0" `no_param
          ~doc:(doc "polyvariant case (length=0)");
        variant_constructor "Foo1" (`tuple_like [variant_argument cty_int])
          ~doc:(doc "polyvariant case (length=1)");
        variant_constructor "Foo2" (`tuple_like [variant_argument cty_int; variant_argument cty_int])
          ~doc:(doc "polyvariant case (length=2)");
      ] ~configs:[
        Caml_config.variant_type `polymorphic
      ] ~doc:(doc "polyvariant")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let parent = "ex_variant_foo" in
    parent, Util.FwrtTypeEnv.(
      let va_int = variant_argument cty_int in
      init
      |> bind_object ~configs:[Caml_config.variant_type `polymorphic] parent []
      |> bind_constructor ~parent "Foo0"
      |> bind_constructor ~parent "Foo1" ~args:[va_int]
      |> bind_constructor ~parent "Foo2" ~args:[va_int; va_int]
    )

  let json_name = "ExVariantFoo"
  let ts_ast : ts_ast option =
    let discriminator = "kind" in
    let discriminator_value kind =
      Util.Ts_ast.property discriminator (`literal_type (`string_literal kind))
    in
    let foo0 =
      `type_literal
        [ discriminator_value "foo0" ] in
    let foo1 =
      `type_literal
        [ discriminator_value "foo1";
          Util.Ts_ast.property "value" (`type_reference "number") ] in
    let foo2 =
      `type_literal
        [ discriminator_value "foo2";
          Util.Ts_ast.property "value" (`tuple [`type_reference "number"; `type_reference "number"]) ] in
    let foos = ["Foo0", foo0; "Foo1", foo1; "Foo2", foo2] in
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
            tsa_type_desc = `union (List.map snd foos); };
        Util.Ts_ast.case_analyzer json_name ("analyze"^json_name) options foos ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            (json_name,
              (`anyone_of
                  [`object_of
                    [`mandatory_field ("kind", (`exactly (`str "foo0")))];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "foo1")));
                    `mandatory_field ("value", `integral)];
                  `object_of
                    [`mandatory_field ("kind", (`exactly (`str "foo2")));
                    `mandatory_field ("value", (`tuple_of [`integral; `integral]))]]))))
    )

  open Bindoj_openapi.V3

  let schema_object : Schema_object.t option = None
end

module Customized_union : Util.Ex_desc = struct
  let module_name = "Customized_union"

  let variant_configs : [`type_decl] configs = [
    Json_config.variant_discriminator "tag";
    Json_config.name_of_variant_arg (`kind_name None);
  ]

  include Util.Make_ex_decls(struct
    let make_decl (module D: Util.With_docstr) : type_decl =
      let open D in
      variant_decl "ex_variant_customized_union" [
      variant_constructor "Case_tuple_like_arg" (`tuple_like [variant_argument cty_int])
        ~configs:[
          Json_config.name "case-tuple-like-arg'";
          Json_config.name_of_variant_arg `arg;
        ]
        ~doc:(doc "customized arg name (arg)");
      variant_constructor "Case_tuple_like_exactly" (`tuple_like [variant_argument cty_int])
        ~configs:[
          Json_config.name "case-tuple-like-exactly'";
          Json_config.name_of_variant_arg (`exactly "Argument");
        ]
        ~doc:(doc "customized arg name (exactly Argument)");
      variant_constructor "Case_tuple_like_kind_name" (`tuple_like [variant_argument cty_int])
        ~configs:[
          Json_config.name "case-tuple-like-kind-name'";
        ]
        ~doc:(doc "inherited customized arg name (kind_name)");
      variant_constructor "Case_tuple_like_kind_name_no_mangling" (`tuple_like [variant_argument cty_int])
        ~configs:[
          Json_config.name_of_variant_arg (`kind_name (Some `no_mangling));
        ]
        ~doc:(doc "customized arg name (kind_name of no_mangling)");
      variant_constructor "Case_tuple_like_kind_name_no_mangling_with_ctor_name" (`tuple_like [variant_argument cty_int])
        ~configs:[
          Json_config.name "case-tuple-like-kind-name-no-mangling-with-ctor-name";
          Json_config.name_of_variant_arg (`kind_name (Some `no_mangling));
        ]
        ~doc:(doc "customized arg name (kind_name of no_mangling) with customized constructor name");
      variant_constructor "Case_inline_record" (`inline_record [
        record_field "x" cty_int
          ~configs:[ Json_config.name "x'" ];
        record_field "y" cty_int
          ~configs:[ Json_config.name "y'" ];
      ])
        ~configs:[
          Json_config.name "case-inline-record'";
        ]
        ~doc:(doc "customized name of fields");
    ] ~configs:variant_configs
      ~doc:(doc "variant with customized discriminator and argument names")
  end)

  let fwrt : (unit, unit, unit) ts_fwrt_decl =
    let parent = "ex_variant_customized_union" in
    parent, Util.FwrtTypeEnv.(
      init
      |> bind_object ~configs:variant_configs parent []
      |> bind_constructor ~parent "Case_tuple_like_arg"
          ~configs:[
            Json_config.name "case-tuple-like-arg'";
            Json_config.name_of_variant_arg `arg;
          ]
          ~args:[variant_argument cty_int]
      |> bind_constructor ~parent "Case_tuple_like_exactly"
          ~configs:[
            Json_config.name "case-tuple-like-exactly'";
            Json_config.name_of_variant_arg (`exactly "Argument");
          ]
          ~args:[variant_argument cty_int]
      |> bind_constructor ~parent "Case_tuple_like_kind_name"
          ~args:[variant_argument cty_int]
          ~configs:[
            Json_config.name "case-tuple-like-kind-name'";
          ]
      |> bind_constructor ~parent "Case_tuple_like_kind_name_no_mangling"
          ~args:[variant_argument cty_int]
          ~configs:[
            Json_config.name_of_variant_arg (`kind_name (Some `no_mangling));
          ]
      |> bind_constructor ~parent "Case_tuple_like_kind_name_no_mangling_with_ctor_name"
          ~args:[variant_argument cty_int]
          ~configs:[
            Json_config.name "case-tuple-like-kind-name-no-mangling-with-ctor-name";
            Json_config.name_of_variant_arg (`kind_name (Some `no_mangling));
          ]
      |> bind_constructor ~parent "Case_inline_record"
          ~configs:[
            Json_config.name "case-inline-record'";
          ]
          ~fields:[
            field "x" cty_int
              ~configs:[ Json_config.name "x'" ];
            field "y" cty_int
              ~configs:[ Json_config.name "y'" ];
          ]
    )

  let json_name = "ExVariantCustomizedUnion"
  let ts_ast : ts_ast option =
    let discriminator = "tag" in
    let case_tuple_like_arg =
      `type_literal
        Util.Ts_ast.[
          property discriminator (`literal_type (`string_literal "case-tuple-like-arg'"));
          property "arg" (`type_reference "number"); ] in
    let case_tuple_like_exactly =
      `type_literal
        Util.Ts_ast.[
          property discriminator (`literal_type (`string_literal "case-tuple-like-exactly'"));
          property "Argument" (`type_reference "number"); ] in
    let case_tuple_like_kind_name =
      `type_literal
        Util.Ts_ast.[
          property discriminator (`literal_type (`string_literal "case-tuple-like-kind-name'"));
          property "case-tuple-like-kind-name'" (`type_reference "number"); ] in
    let case_tuple_like_kind_name_no_mangling =
      `type_literal
        Util.Ts_ast.[
          property discriminator (`literal_type (`string_literal "case-tuple-like-kind-name-no-mangling"));
          property "Case_tuple_like_kind_name_no_mangling" (`type_reference "number"); ] in
    let case_tuple_like_kind_name_no_mangling_with_ctor_name =
      `type_literal
        Util.Ts_ast.[
          property discriminator (`literal_type (`string_literal "case-tuple-like-kind-name-no-mangling-with-ctor-name"));
          property "case-tuple-like-kind-name-no-mangling-with-ctor-name" (`type_reference "number"); ] in
    let case_inline_record =
      `type_literal
        Util.Ts_ast.[
          property discriminator (`literal_type (`string_literal "case-inline-record'"));
          property "x'" (`type_reference "number");
          property "y'" (`type_reference "number"); ] in
    let customized_union = [
      "Case_tuple_like_arg", case_tuple_like_arg;
      "Case_tuple_like_exactly", case_tuple_like_exactly;
      "Case_tuple_like_kind_name", case_tuple_like_kind_name;
      "Case_tuple_like_kind_name_no_mangling", case_tuple_like_kind_name_no_mangling;
      "Case_tuple_like_kind_name_no_mangling_with_ctor_name", case_tuple_like_kind_name_no_mangling_with_ctor_name;
      "Case_inline_record", case_inline_record;
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
            tsa_type_desc = `union (List.map snd customized_union); };
        Util.Ts_ast.case_analyzer json_name ("analyze"^json_name) options customized_union; ]

  let expected_json_shape_explanation =
    Some (
      `with_warning
        ("not considering any config if exists",
          (`named
            (json_name,
              (`anyone_of
                  [`object_of
                    [`mandatory_field ("tag", (`exactly (`str "case-tuple-like-arg'")));
                    `mandatory_field ("arg", `integral)];
                  `object_of
                    [`mandatory_field ("tag", (`exactly (`str "case-tuple-like-exactly'")));
                    `mandatory_field ("Argument", `integral)];
                  `object_of
                    [`mandatory_field ("tag", (`exactly (`str "case-tuple-like-kind-name'")));
                    `mandatory_field ("case-tuple-like-kind-name'", `integral)];
                  `object_of
                    [`mandatory_field ("tag", (`exactly (`str "case-tuple-like-kind-name-no-mangling")));
                    `mandatory_field ("Case_tuple_like_kind_name_no_mangling", `integral)];
                  `object_of
                    [`mandatory_field ("tag", (`exactly (`str "case-tuple-like-kind-name-no-mangling-with-ctor-name")));
                    `mandatory_field ("case-tuple-like-kind-name-no-mangling-with-ctor-name", `integral)];
                  `object_of
                    [`mandatory_field ("tag", (`exactly (`str "case-inline-record'")));
                    `mandatory_field ("x'", `integral);
                    `mandatory_field ("y'", `integral)]]))))
    )

  let schema_object : Schema_object.t option = None
end

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex_variant"

let example_descs : (module Util.Ex_desc) list = [
  (module Person);
  (module Person_reused);
  (module Int_list);
  (module Int_list_objtuple);
  (module Polymorphic);
  (module Customized_union);
]
