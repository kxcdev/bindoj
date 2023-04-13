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
open Bindoj_gen.Json_codec
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_gen_ts.Typescript_datatype

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex07"

let cty_int = Coretype.mk_prim `int

let variant_configs : [`type_decl] configs = [
  Json_config.variant_discriminator "tag";
]

let decl : type_decl =
  variant_decl "customized_union" [
    variant_constructor "Case1" (`tuple_like [cty_int])
      ~configs:[
        Json_config.name "Case1_";
        Json_config.name_of_variant_arg "value";
      ];
    variant_constructor "Case2" (`inline_record [
      record_field "x" cty_int
        ~configs:[ Json_config.name "x_" ];
      record_field "y" cty_int
        ~configs:[ Json_config.name "y_" ];
    ])
      ~configs:[
        Json_config.name "Case2_";
      ];
  ] ~configs:variant_configs

let decl_with_docstr : type_decl =
  variant_decl "customized_union" [
    variant_constructor "Case1" (`tuple_like [cty_int])
      ~configs:[
        Json_config.name "Case1_";
        Json_config.name_of_variant_arg "value";
      ]
      ~doc:(`docstr "custom arg name (value)");
    variant_constructor "Case2" (`inline_record [
      record_field "x" cty_int
        ~configs:[ Json_config.name "x_" ];
      record_field "y" cty_int
        ~configs:[ Json_config.name "y_" ];
    ])
      ~configs:[
        Json_config.name "Case2_";
      ];
  ] ~configs:variant_configs
    ~doc:(`docstr "variant with customized discriminator and argument names")

let fwrt : (unit, unit) fwrt_decl =
  let parent = "customized_union" in
  let annot = () in
  parent, FwrtTypeEnv.(
    init
    |> bind_object ~annot ~configs:variant_configs parent []
    |> bind_constructor ~parent ~annot "Case1"
        ~configs:[
          Json_config.name "Case1_";
          Json_config.name_of_variant_arg "value";
        ]
        ~args:[cty_int]
    |> bind_constructor ~parent ~annot "Case2"
        ~configs:[
          Json_config.name "Case2_";
          Json_config.name_of_variant_arg "values";
        ]
        ~fields:[
          field ~annot "x" cty_int
            ~configs:[ Json_config.name "x_" ];
          field ~annot "y" cty_int
            ~configs:[ Json_config.name "y_" ];
        ]
  )

let ts_ast : ts_ast option =
  let discriminator = "tag" in
  let var_v = "__bindoj_v" in
  let var_x = "__bindoj_x" in
  let var_fns = "__bindoj_fns" in
  let ret = "__bindoj_ret" in
  let case1 =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "Case1_"); };
        { tsps_modifiers = [];
          tsps_name = "value";
          tsps_type_desc = `type_reference "number"; }; ] in
  let case2 =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "Case2_"); };
        { tsps_modifiers = [];
          tsps_name = "x_";
          tsps_type_desc = `type_reference "number"; };
        { tsps_modifiers = [];
          tsps_name = "y_";
          tsps_type_desc = `type_reference "number"; }; ] in
  let customized_union = [
    "Case1_", case1;
    "Case2_", case2;
  ] in
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "customized_union";
          tsa_type_parameters = [];
          tsa_type_desc = `union (List.map snd customized_union); };
      `function_declaration
        { tsf_modifiers = [`export];
          tsf_name = "analyze_customized_union";
          tsf_type_parameters = [ret];
          tsf_parameters =
            Util.Ts_ast.(case_analyzer_parameters { discriminator; var_x; var_v; var_fns; ret } customized_union);
          tsf_type_desc =
            `func_type
              { tsft_parameters =
                  [ { tsp_name = var_x;
                      tsp_type_desc = `type_reference "customized_union"; } ];
                tsft_type_desc = `type_reference ret; };
          tsf_body =
            Util.Ts_ast.(case_analyzer_body "customized_union" { discriminator; var_x; var_v; var_fns; ret; } customized_union); } ]
