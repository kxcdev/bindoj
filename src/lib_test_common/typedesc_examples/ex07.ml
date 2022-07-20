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
open Bindoj_gen.Json_codec
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_gen_ts.Typescript_datatype

let cty_int = Coretype.mk_prim `int

let variant_configs : [`type_decl] configs = [
  Json_config.variant_discriminator "tag";
]

let decl : type_decl =
  variant_decl "customized_union" [
    variant_constructor "Case1" (`tuple_like [cty_int])
      ~configs:[Json_config.name "value"];
    variant_constructor "Case2" (`inline_record [
      record_field "x" cty_int;
      record_field "y" cty_int;
    ]);
  ] ~configs:variant_configs

let decl_with_docstr : type_decl =
  variant_decl "customized_union" [
    variant_constructor "Case1" (`tuple_like [cty_int])
      ~configs:[Json_config.name "value"]
      ~doc:(`docstr "custom arg name (value)");
    variant_constructor "Case2" (`inline_record [
      record_field "x" cty_int;
      record_field "y" cty_int;
    ]);
  ] ~configs:variant_configs
    ~doc:(`docstr "variant with customized discriminator and argument names")

let fwrt : (unit, unit) fwrt_decl =
  let parent = "customized_union" in
  let annot = () in
  parent, FwrtTypeEnv.(
    init
    |> bind_object ~annot ~configs:variant_configs parent []
    |> bind_constructor ~parent ~annot ~configs:[Json_config.name "value"]  "Case1" ~args:[cty_int]
    |> bind_constructor ~parent ~annot ~configs:[Json_config.name "values"] "Case2"
        ~fields:[
          field ~annot "x" cty_int;
          field ~annot "y" cty_int;
        ]
  )

let ts_ast : ts_ast option = None
