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
open Bindoj_gen.Caml_datatype
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_gen_ts.Typescript_datatype

let decl : type_decl = {
  td_name = "foo";
  td_kind =
    Variant_kind [
      Cstr_tuple {
        ct_name = "Foo0";
        ct_args = [];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `nodoc;
      Cstr_tuple {
        ct_name = "Foo1";
        ct_args = ["int"];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `nodoc;
      Cstr_tuple {
        ct_name = "Foo2";
        ct_args = ["int"; "int"];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `nodoc;
    ], `nodoc;
  td_flvconfigs = [
    Flvconfig_variant_flavor `polymorphic_variant
  ]
}

let decl_with_docstr : type_decl = {
  td_name = "foo";
  td_kind =
    Variant_kind [
      Cstr_tuple {
        ct_name = "Foo0";
        ct_args = [];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `docstr "polyvariant case (length=0)";
      Cstr_tuple {
        ct_name = "Foo1";
        ct_args = ["int"];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `docstr "polyvariant case (length=1)";
      Cstr_tuple {
        ct_name = "Foo2";
        ct_args = ["int"; "int"];
        ct_codec = `default_codec;
        ct_flvconfigs = []
      }, `docstr "polyvariant case (length=2)";
    ], `docstr "polyvariant";
  td_flvconfigs = [
    Flvconfig_variant_flavor `polymorphic_variant
  ]
}

let fwrt : (unit, unit) fwrt_decl =
  "foo", FwrtTypeEnv.(
    init
    |> bind ~annot:() "foo" []
    |> bind ~parent:"foo" ~annot:() "Foo2" [ item ~annot:() "arg" ["int"; "int"] ]
    |> bind ~parent:"foo" ~annot:() "Foo1" [ item ~annot:() "arg" ["int"] ]
    |> bind ~parent:"foo" ~annot:() "Foo0" []
  )

let ts_ast : ts_ast option =
  let ret = "__bindoj_ret" in
  let kind_fname = "kind" in
  let arg_fname = "arg" in
  let var_v = "__bindoj_v" in
  let var_x = "__bindoj_x" in
  let var_fns = "__bindoj_fns" in
  let foo0 =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = kind_fname;
          tsps_type_desc = `literal_type (`string_literal "Foo0"); } ] in
  let foo1 =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = kind_fname;
          tsps_type_desc = `literal_type (`string_literal "Foo1"); };
        { tsps_modifiers = [];
          tsps_name = arg_fname;
          tsps_type_desc = `type_reference "number"; } ] in
  let foo2 =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = kind_fname;
          tsps_type_desc = `literal_type (`string_literal "Foo2"); };
        { tsps_modifiers = [];
          tsps_name = arg_fname;
          tsps_type_desc = `tuple [`type_reference "number"; `type_reference "number"]; } ] in
  let foos = [foo0; foo1; foo2] in
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "foo";
          tsa_type_parameters = [];
          tsa_type_desc = `union foos; };
      `function_declaration
        { tsf_modifiers = [`export];
          tsf_name = "analyze_foo";
          tsf_type_parameters = [ret];
          tsf_parameters =
            Util.Ts_ast.(case_analyzer_parameters { kind_fname; var_x; var_v; var_fns; ret; } foos);
          tsf_type_desc =
            `func_type
              { tsft_parameters =
                  [ { tsp_name = var_x;
                      tsp_type_desc = `type_reference "foo"; } ];
                tsft_type_desc = `type_reference ret; };
          tsf_body =
            Util.Ts_ast.(case_analyzer_body "foo" { kind_fname; var_x; var_v; var_fns; ret; } foos); } ]
