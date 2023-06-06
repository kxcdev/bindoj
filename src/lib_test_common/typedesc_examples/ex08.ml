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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex08"

let cty_string = Coretype.mk_prim `string
let cty_json = Bindoj_std.Coretypes.json

let decl : type_decl =
  record_decl "named_json" [
    record_field "name" cty_string;
    record_field "json" cty_json;
  ]

let decl_with_docstr : type_decl =
  record_decl "named_json" [
    record_field "name" cty_string ~doc:(`docstr "a name of datum");
    record_field "json" cty_json ~doc:(`docstr "a json datum");
  ] ~doc:(`docstr "record of name and json datum")

let fwrt : (unit, unit) ts_fwrt_decl =
  "named_json", Util.FwrtTypeEnv.(
      init
      |> bind_object "named_json"
        [ field "name" cty_string;
          field "json" cty_json; ]
    )

let ts_ast : ts_ast option = Some [
    `type_alias_declaration {
        tsa_modifiers = [`export];
        tsa_name = "NamedJson";
        tsa_type_parameters = [];
        tsa_type_desc =
          `type_literal [
              { tsps_modifiers = [];
                tsps_name = "json";
                tsps_type_desc = `type_reference "json_value"; };
              { tsps_modifiers = [];
                tsps_name = "name";
                tsps_type_desc = `type_reference "string"; };
            ];};
  ]
