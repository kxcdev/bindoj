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
open Bindoj_gen_config

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex14"

let cty_typle = Coretype.mk_tuple [
  Coretype.prim `float;
  Coretype.prim `string;
] ~configs: [
  Json_config.tuple_style (`obj `default)
]

let decl : type_decl =
  alias_decl "objtuple" cty_typle

let decl_with_docstr : type_decl =
  alias_decl "objtuple" cty_typle
    ~doc:(`docstr "alias of objtuple type")

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  "objtuple", Util.FwrtTypeEnv.(
    init
    |> bind_alias "objtuple" cty_typle
  )

let ts_ast : ts_ast option = Some [
  `type_alias_declaration {
    tsa_modifiers = [`export];
    tsa_name = "Objtuple";
    tsa_type_parameters = [];
    tsa_type_desc =
      `type_literal [
        { tsps_modifiers = [];
          tsps_name = "_0";
          tsps_type_desc = `type_reference "number" };
        { tsps_modifiers = [];
          tsps_name = "_1";
          tsps_type_desc =
          `type_reference "string" }; ]
  }
]

let expected_json_shape_explanation =
  Some (
    `with_warning
     ("not considering any config if exists",
       (`named ("Objtuple", (`tuple_of [`proper_float; `string]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option =
  Schema_object.(record ~schema
    ~title:"Objtuple"
    ~id:"#Objtuple"
    [
      "_0", number ();
      "_1", string ();
    ])
  |> Option.some
