(* Copyright 2022 Kotoi-Xie Consultancy, Inc. This file is a part of the

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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex12"

let cty_enum = Coretype.(mk_string_enum [
  string_enum_case "Case_at0";
  string_enum_case "case_at1"
    ~configs:[ Json_config.default_mangling ];
  string_enum_case "Case_at2"
    ~configs:[ Json_config.default_mangling ];
  string_enum_case "Case_at3"
    ~configs:[ Json_config.name "Case_third"; Json_config.default_mangling ];
] ~configs:[ Json_config.no_mangling ])

let decl : type_decl =
  alias_decl "cases" cty_enum

let decl_with_docstr : type_decl =
  alias_decl "cases" (
    Coretype.(mk_string_enum [
      string_enum_case "Case_at0" ~doc:(`docstr "zeroth case");
      string_enum_case "case_at1"
        ~configs:[ Json_config.default_mangling ]
        ~doc:(`docstr "first case");
      string_enum_case "Case_at2"
        ~configs:[ Json_config.default_mangling ]
        ~doc:(`docstr "second case");
      string_enum_case "Case_at3"
        ~configs:[ Json_config.name "Case_third"; Json_config.default_mangling ]
        ~doc:(`docstr "third case");
    ] ~configs:[ Json_config.no_mangling ])
  ) ~doc:(`docstr "alias of string cases")

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  "cases", Util.FwrtTypeEnv.(
    init
    |> bind_alias "cases" cty_enum
  )

let ts_ast : ts_ast option = Some [
  `type_alias_declaration {
    tsa_modifiers = [`export];
    tsa_name = "Cases";
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
       (`named ("Cases", (`string_enum ["Case_at0"; "case-at1"; "Case-at2"; "Case-third"]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option = None
