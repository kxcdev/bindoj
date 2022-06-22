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
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_gen_ts.Typescript_datatype

let cty_int = Coretype.mk_prim `int

let decl : type_decl =
  variant_decl "foo" [
    variant_constructor "Foo0" `no_param;
    variant_constructor "Foo1" (`tuple_like [cty_int]);
    variant_constructor "Foo2" (`tuple_like [cty_int; cty_int]);
  ] ~configs:[
    Caml_config.variant_type `polymorphic
  ]

let decl_with_docstr : type_decl =
  variant_decl "foo" [
    variant_constructor "Foo0" `no_param
      ~doc:(`docstr "polyvariant case (length=0)");
    variant_constructor "Foo1" (`tuple_like [cty_int])
      ~doc:(`docstr "polyvariant case (length=1)");
    variant_constructor "Foo2" (`tuple_like [cty_int; cty_int])
      ~doc:(`docstr "polyvariant case (length=2)");
  ] ~configs:[
    Caml_config.variant_type `polymorphic
  ] ~doc:(`docstr "polyvariant")

let fwrt : (unit, unit) fwrt_decl =
  let parent = "foo" in
  let annot = () in
  "foo", FwrtTypeEnv.(
    init
    |> bind_object ~annot ~configs:[Caml_config.variant_type `polymorphic] "foo" []
    |> bind_constructor ~parent ~annot "Foo0"
    |> bind_constructor ~parent ~annot "Foo1" ~args:[cty_int]
    |> bind_constructor ~parent ~annot "Foo2" ~args:[cty_int; cty_int]
  )

let ts_ast : ts_ast option =
  let ret = "__bindoj_ret" in
  let discriminator = "kind" in
  let arg_fname = "arg" in
  let var_v = "__bindoj_v" in
  let var_x = "__bindoj_x" in
  let var_fns = "__bindoj_fns" in
  let foo0 =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "Foo0"); } ] in
  let foo1 =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "Foo1"); };
        { tsps_modifiers = [];
          tsps_name = arg_fname;
          tsps_type_desc = `type_reference "number"; } ] in
  let foo2 =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "Foo2"); };
        { tsps_modifiers = [];
          tsps_name = arg_fname;
          tsps_type_desc = `tuple [`type_reference "number"; `type_reference "number"]; } ] in
  let foos = ["Foo0", foo0; "Foo1", foo1; "Foo2", foo2] in
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "foo";
          tsa_type_parameters = [];
          tsa_type_desc = `union (List.map snd foos); };
      `function_declaration
        { tsf_modifiers = [`export];
          tsf_name = "analyze_foo";
          tsf_type_parameters = [ret];
          tsf_parameters =
            Util.Ts_ast.(case_analyzer_parameters { discriminator; var_x; var_v; var_fns; ret; } foos);
          tsf_type_desc =
            `func_type
              { tsft_parameters =
                  [ { tsp_name = var_x;
                      tsp_type_desc = `type_reference "foo"; } ];
                tsft_type_desc = `type_reference ret; };
          tsf_body =
            Util.Ts_ast.(case_analyzer_body "foo" { discriminator; var_x; var_v; var_fns; ret; } foos); } ]
