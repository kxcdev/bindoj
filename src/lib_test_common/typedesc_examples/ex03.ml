(* Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

open Bindoj_base.Type_desc
open Bindoj_gen.Json_codec
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_gen_ts.Typescript_datatype

let kind_fname = "kind"

let decl : type_decl =
  { td_name = "int_list";
    td_kind =
      Variant_kind
        [Cstr_tuple { ct_name = "IntNil";
                      ct_args = [];
                      ct_codec = `default_codec;
                      ct_flvconfigs = [Flvconfig_flat_kind
                                          { kind_fname=Some kind_fname; arg_fname=Some "arg"; }];
            }, `nodoc;
          Cstr_tuple { ct_name = "IntCons";
                      ct_args = ["int"; "int_list"];
                      ct_codec = `default_codec;
                      ct_flvconfigs = [Flvconfig_flat_kind
                                          { kind_fname=Some kind_fname; arg_fname=Some "arg"; }]
            }, `nodoc],
      `nodoc;
    td_flvconfigs = [];
  }

let decl_with_docstr : type_decl =
  { td_name = "int_list";
    td_kind =
      Variant_kind
        [Cstr_tuple { ct_name = "IntNil";
                      ct_args = [];
                      ct_codec = `default_codec;
                      ct_flvconfigs = [Flvconfig_flat_kind
                                          { kind_fname=Some kind_fname; arg_fname=Some "arg"; }]
            }, `docstr "nil for int_list";
          Cstr_tuple { ct_name = "IntCons";
                       ct_args = ["int"; "int_list"];
                       ct_codec = `default_codec;
                       ct_flvconfigs = [Flvconfig_flat_kind
                                          { kind_fname=Some "kind"; arg_fname=Some "arg"; }]
            }, `docstr "cons for int_list"],
      `docstr "int list";
    td_flvconfigs = [];
  }

let fwrt : (unit, unit) fwrt_decl =
  "int_list", FwrtTypeEnv.(
    init
    |> bind ~annot:() "int_list" []
    |> bind ~parent:"int_list" ~annot:() ~kind_fname "IntCons" [ item ~annot:() "arg" ["int"; "int_list"] ]
    |> bind ~parent:"int_list" ~annot:() ~kind_fname "IntNil" []
  )

let ts_ast : ts_ast option =
  let ret = "__bindoj_ret" in
  let kind_fname = "kind" in
  let arg_fname = "arg" in
  let var_v = "__bindoj_v" in
  let var_x = "__bindoj_x" in
  let var_fns = "__bindoj_fns" in
  let int_nil =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = kind_fname;
          tsps_type_desc = `literal_type (`string_literal "IntNil"); } ] in
  let int_cons =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = kind_fname;
          tsps_type_desc = `literal_type (`string_literal "IntCons"); };
        { tsps_modifiers = [];
          tsps_name = arg_fname;
          tsps_type_desc = `tuple [ `type_reference "number"; `type_reference "int_list"; ]; } ] in
  let cstrs = [int_nil; int_cons] in
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "int_list";
          tsa_type_parameters = [];
          tsa_type_desc = `union cstrs; };
      `function_declaration
        { tsf_modifiers = [`export];
          tsf_name = "analyze_int_list";
          tsf_type_parameters = [ret];
          tsf_parameters =
            Util.Ts_ast.(case_analyzer_parameters { kind_fname; var_x; var_v; var_fns; ret; } cstrs);
          tsf_type_desc =
            `func_type
              { tsft_parameters =
                  [ { tsp_name = var_x;
                      tsp_type_desc = `type_reference "int_list"; } ];
                tsft_type_desc = `type_reference ret; };
          tsf_body =
            Util.Ts_ast.(case_analyzer_body "int_list" { kind_fname; var_x; var_v; var_fns; ret; } cstrs); } ]
