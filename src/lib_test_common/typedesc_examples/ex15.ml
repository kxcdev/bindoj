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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex15"

let discriminator = "tag"
let arg_fname = "value"

let variant_configs : [`type_decl] configs = [
  Json_config.variant_discriminator discriminator;
]

let constructor_configs: [`variant_constructor] configs = [
  Json_config.name_of_variant_arg arg_fname
]

let cty_int = Coretype.mk_prim `int
let cty_string = Coretype.mk_prim `string

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
    variant_decl "nested_variant" [
      variant_constructor "Student1" (`inline_record [
        record_field_nested ~codec:(open_ "Ex01") "student" (decl (module Ex01))
          ~configs:[
            Json_config.no_mangling;
          ];
      ]) ~configs:constructor_configs ~doc:(doc "Student1 constructor");
      variant_constructor "Student2" (`inline_record [
        record_field_nested ~codec:(open_ "Ex01") "student" (decl (module Ex01))
          ~configs:[
            Json_config.nested_field_style `spreading;
            Json_config.no_mangling;
          ];
      ]) ~configs:constructor_configs ~doc:(doc "Student2 constructor");
      variant_constructor "Student3" (`tuple_like [
        variant_argument_nested ~codec:(open_ "Ex01") (decl (module Ex01))
          ~configs:[
            Json_config.no_mangling;
          ];
      ]) ~configs:constructor_configs ~doc:(doc "Student3 constructor");
      variant_constructor "Student4" (`tuple_like [
        variant_argument_nested ~codec:(open_ "Ex01") (decl (module Ex01))
          ~configs:[
            Json_config.nested_field_style `spreading;
            Json_config.no_mangling;
          ];
      ]) ~configs:constructor_configs ~doc:(doc "Student4 constructor");
      variant_constructor "Int_list1" (`tuple_like [
        variant_argument_nested ~codec:(open_ "Ex03") (decl (module Ex03))
          ~configs:[
            Json_config.no_mangling;
          ];
      ]) ~configs:constructor_configs ~doc:(doc "IntList constructor");
      variant_constructor "Int_list2" (`tuple_like [
        variant_argument_nested ~codec:(open_ "Ex03") (decl (module Ex03))
          ~configs:[
            Json_config.nested_field_style `spreading;
            Json_config.no_mangling;
          ];
      ]) ~configs:constructor_configs ~doc:(doc "IntList constructor");
    ] ~configs:variant_configs ~doc:(doc "definition of nested_variant type")
end

let decl : type_decl = make_decl false

let decl_with_docstr : type_decl = make_decl true

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  let int_list = "int_list" in
  let nested_variant = "nested_variant" in
  "nested_variant", Util.FwrtTypeEnv.(
    init
    |> bind_object "student"
      [ field "admission_year" cty_int;
        field "name" cty_string; ]
    |> bind_object int_list []
    |> bind_constructor ~parent:int_list "IntNil"
    |> bind_constructor ~parent:int_list "IntCons" ~args:[
      variant_argument @@ cty_int;
      variant_argument @@ Coretype.mk_self ();
    ]
    |> bind_object ~configs:variant_configs nested_variant []
    |> bind_constructor ~parent:nested_variant "Student1" ~fields:[
      field_nested ~codec:(`open_ "Ex01") "student" "student";
    ] ~configs:constructor_configs
    |> bind_constructor ~parent:nested_variant "Student2" ~fields:[
      field_nested ~codec:(`open_ "Ex01") "student" "student"
        ~configs:[
          Json_config.nested_field_style `spreading;
        ];
    ] ~configs:constructor_configs
    |> bind_constructor ~parent:nested_variant "Student3" ~args:[
      variant_argument_nested ~codec:(`open_ "Ex01") "student";
    ] ~configs:constructor_configs
    |> bind_constructor ~parent:nested_variant "Student4" ~args:[
      variant_argument_nested ~codec:(`open_ "Ex01") "student"
        ~configs:[
          Json_config.nested_field_style `spreading;
        ];
    ] ~configs:constructor_configs
    |> bind_constructor ~parent:nested_variant "Int_list1" ~args:[
      variant_argument_nested ~codec:(`open_ "Ex03") "int_list";
    ] ~configs:constructor_configs
    |> bind_constructor ~parent:nested_variant "Int_list2" ~args:[
      variant_argument_nested ~codec:(`open_ "Ex03") "int_list"
        ~configs:[
          Json_config.nested_field_style `spreading;
        ]
    ] ~configs:constructor_configs
  )

let ts_ast : ts_ast option =
  let discriminator_value kind =
    Util.Ts_ast.property discriminator (`literal_type (`string_literal kind))
  in
  let student1 =
    `type_literal
      [ discriminator_value "student1";
        Util.Ts_ast.property "student" (`type_reference "Student"); ]
  in
  let student2 =
    `intersection [
      `type_literal
        [ discriminator_value "student2"; ];
      `type_reference "Student";
    ]
  in
  let student3 =
    `type_literal
      [ discriminator_value "student3";
        Util.Ts_ast.property arg_fname (`type_reference "Student"); ]
  in
  let student4 =
    `intersection [
      `type_literal
        [ discriminator_value "student4"; ];
      `type_reference "Student";
    ]
  in
  let int_list1 =
    `type_literal
      [ discriminator_value "int-list1";
        Util.Ts_ast.property arg_fname (`type_reference "IntList") ]
  in
  let int_list2 =
    `intersection [
      `type_literal
        [ discriminator_value "int-list2" ];
      `type_reference "IntList";
      ]
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
          tsa_name = "NestedVariant";
          tsa_type_parameters = [];
          tsa_type_desc = `union (List.map snd nested_variant); };
      Util.Ts_ast.case_analyzer "NestedVariant" "analyzeNestedVariant" options nested_variant; ]

let expected_json_shape_explanation =
  Some (
    `with_warning
     ("not considering any config if exists",
       (`named
          ("NestedVariant",
            (`anyone_of
               [`object_of
                  [`mandatory_field ("tag", (`exactly (`str "student1")));
                  `mandatory_field
                    ("student",
                      (`named
                         ("Student",
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
                   ("value",
                     (`named
                        ("Student",
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
                   ("value",
                     (`named
                        ("IntList",
                          (`anyone_of
                             [`object_of
                                [`mandatory_field
                                   ("kind", (`exactly (`str "intnil")))];
                             `object_of
                               [`mandatory_field
                                  ("kind", (`exactly (`str "intcons")));
                               `mandatory_field
                                 ("arg", (`tuple_of [`integral; `self]))]]))))];
               `object_of
                 [`mandatory_field ("tag", (`exactly (`str "int-list2")));
                 `mandatory_field ("kind", (`exactly (`str "intnil")))];
               `object_of
                 [`mandatory_field ("tag", (`exactly (`str "int-list2")));
                 `mandatory_field ("kind", (`exactly (`str "intcons")));
                 `mandatory_field ("arg", (`tuple_of [`integral; `self]))]]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option =
  let student_fields = Schema_object.[
    "admissionYear", integer ();
    "name", string ();
  ] in
  let int_list_fields = Schema_object.[
      "intnil", [];
      "intcons", [
        "arg", tuple [ integer(); ref "#IntList"; ]
      ];
  ] in
  let name = "NestedVariant" in
  let constructor_fields name fields = fields @ [
    discriminator, Schema_object.string () ~enum:[`str name]
  ] in
  Schema_object.(
    oneOf ~schema ~title:name ~id:("#"^name) [
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
        "arg", tuple [ integer(); ref "#IntList" ];
        "kind", string () ~enum:[`str "intcons" ];
      ];
    ]
  )
  |> Option.some
