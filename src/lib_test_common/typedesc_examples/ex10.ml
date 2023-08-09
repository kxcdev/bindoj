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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex10"

let cty_int_opt = Coretype.(mk_option % prim) `int

let decl : type_decl =
  record_decl "xy_opt" [
    record_field "x_opt" cty_int_opt;
    record_field "y_opt" cty_int_opt;
  ]

let decl_with_docstr : type_decl =
  record_decl "xy_opt" [
    record_field "x_opt" cty_int_opt ~doc:(`docstr "an optional int x");
    record_field "y_opt" cty_int_opt ~doc:(`docstr "an optional int y");
  ] ~doc:(`docstr "record of optional int x and optional int y")

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  "xy_opt", Util.FwrtTypeEnv.(
      init
      |> bind_object "xy_opt"
        [ field "x_opt" cty_int_opt;
          field "y_opt" cty_int_opt; ]
    )

let ts_ast : ts_ast option = None

let expected_json_shape_explanation =
  Some (
    `with_warning
      ("not considering any config if exists",
        (`named
          ("XyOpt",
            (`object_of
                [`optional_field ("xOpt", `integral);
                `optional_field ("yOpt", `integral)]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option = None
