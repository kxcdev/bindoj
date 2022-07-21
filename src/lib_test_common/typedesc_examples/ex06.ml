(* Copyright 2022 Kotoi-Xie Consultancy, Inc.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

(* Acknowledgements - AnchorZ Inc.
The initial version or a significant portion of this file is developed
under the funding of AnchorZ Inc. to satisfy its needs in
product development. *)

open Bindoj_base.Type_desc
open Bindoj_gen_foreign.Foreign_datatype
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

let fwrt : (unit, unit) fwrt_decl =
  let annot = () in
  "various_prim_types", FwrtTypeEnv.(
    init
    |> bind_object ~annot "various_prim_types" (
      prims |> List.map (fun (name, ct) ->
        field ~annot name ct
      ))
  )

let ts_ast : ts_ast option = None
