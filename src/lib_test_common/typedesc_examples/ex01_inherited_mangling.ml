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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex01_inherited_mangling"

let cty_int = Coretype.mk_prim `int
let cty_string = Coretype.mk_prim `string

let cty_enum = Coretype.(mk_string_enum [
  string_enum_case "Case_at0";
  string_enum_case "case_at1"
    ~configs:[ Json_config.no_mangling ];
])

let json_name = "student_inherited_mangling"
let configs : [`type_decl] configs = Json_config.[ no_mangling; name json_name ]

let decl : type_decl =
  record_decl "student" [
    record_field "admission_year" cty_int;
    record_field "name" cty_string
      ~configs:[ Json_config.default_mangling ];
    record_field "case_value" cty_enum
      ~configs:[ Json_config.default_mangling ];
  ]  ~configs

let decl_with_docstr : type_decl =
  record_decl "student" [
    record_field "admission_year" cty_int ~doc:(`docstr "addmission_year field");
    record_field "name" cty_string
      ~configs:[ Json_config.default_mangling ]
      ~doc:( `docstr "name field");
    record_field "case_value" cty_enum
      ~configs:[ Json_config.default_mangling ]
      ~doc:( `docstr "case_value field");
  ] ~configs
  ~doc:(`docstr "definition of student type")

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  "student", Util.FwrtTypeEnv.(
    init
    |> bind_object "student"
      [ field "admission_year" cty_int;
        field "name" cty_string
          ~configs:[ Json_config.default_mangling ];
        field "case_value" cty_enum
          ~configs:[ Json_config.default_mangling ]; ]
      ~configs
  )

let ts_ast : ts_ast option =
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = json_name;
          tsa_type_parameters = [];
          tsa_type_desc =
            `type_literal
              [ { tsps_modifiers = [];
                  tsps_name = "admission_year";
                  tsps_type_desc = `type_reference "number"; };
                { tsps_modifiers = [];
                  tsps_name = "name";
                  tsps_type_desc = `type_reference "string"; };
                { tsps_modifiers = [];
                  tsps_name = "caseValue";
                  tsps_type_desc = `union [
                    `literal_type (`string_literal "Case-at0");
                    `literal_type (`string_literal "case_at1");
                  ]; };
              ]; } ]

let expected_json_shape_explanation =
  Some (
    `with_warning
     ("not considering any config if exists",
       (`named
          ("student_inherited_mangling",
            (`object_of
               [`mandatory_field ("admission_year", `integral);
               `mandatory_field ("name", `string);
               `mandatory_field
                 ("caseValue", (`string_enum ["Case-at0"; "case_at1"]))]))))
  )

open Bindoj_openapi.V3

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
