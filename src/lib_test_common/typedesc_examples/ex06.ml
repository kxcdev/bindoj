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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex06"

let prims =
  [`unit; `bool; `int; `float; `string; `uchar; `byte; `bytes]
  |> List.map (fun p -> Coretype.string_of_prim p, Coretype.mk_prim p)

let decl : type_decl =
  record_decl "various_prim_types" (prims |> List.map (fun (name, ct) ->
    record_field name ct
  ))

let decl_with_docstr : type_decl =
  record_decl "various_prim_types" (prims |> List.map (fun (name, ct) ->
    record_field name ct ~doc:(`docstr name)
  )) ~doc:(`docstr "various primitive types")

let fwrt : (unit, unit) ts_fwrt_decl =
  "various_prim_types", Util.FwrtTypeEnv.(
    init
    |> bind_object "various_prim_types" (
      prims |> List.map (fun (name, ct) ->
        field name ct
      ))
  )

let ts_ast : ts_ast option =
  let make_case tsps_name tsps_type_desc =
    { tsps_modifiers = [];
        tsps_name;
        tsps_type_desc; } in
  let lit =
    `type_literal [
        make_case "unit" (`literal_type (`numeric_literal 1.));
        make_case "bool" (`type_reference "boolean");
        make_case "int" (`type_reference "number");
        make_case "float" (`type_reference "number");
        make_case "string" (`type_reference "string");
        make_case "uchar" (`type_reference "string");
        make_case "byte" (`type_reference "number");
        make_case "bytes" (`type_reference "string");
      ] in
  [ `type_alias_declaration {
        tsa_modifiers = [`export];
        tsa_name = "VariousPrimTypes";
        tsa_type_parameters = [];
        tsa_type_desc = lit
      };
  ] |> some
