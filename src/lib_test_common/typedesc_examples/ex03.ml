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
open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_gen_ts.Typescript_datatype

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex03"

let decl : type_decl =
  variant_decl "int_list" [
    variant_constructor "IntNil" `no_param;
    variant_constructor "IntCons" (`tuple_like [
      Coretype.mk_prim `int;
      Coretype.mk_self ();
    ]);
  ]

let decl_with_docstr : type_decl =
  variant_decl "int_list" [
    variant_constructor "IntNil" `no_param
      ~doc:(`docstr "nil for int_list");

    variant_constructor "IntCons" (`tuple_like [
      Coretype.mk_prim `int;
      Coretype.mk_self ();
    ]) ~doc:(`docstr "cons for int_list");

  ] ~doc:(`docstr "int list")

let fwrt : (unit, unit) fwrt_decl =
  let parent = "int_list" in
  let annot = () in
  "int_list", FwrtTypeEnv.(
    init
    |> bind_object ~annot "int_list" []
    |> bind_constructor ~parent ~annot "IntNil"
    |> bind_constructor ~parent ~annot "IntCons" ~args:[Coretype.mk_prim `int; Coretype.mk_self ()]
  )

let ts_ast : ts_ast option =
  let ret = "__bindoj_ret" in
  let discriminator = "kind" in
  let arg_fname = "arg" in
  let var_v = "__bindoj_v" in
  let var_x = "__bindoj_x" in
  let var_fns = "__bindoj_fns" in
  let int_nil =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "IntNil"); } ] in
  let int_cons =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "IntCons"); };
        { tsps_modifiers = [];
          tsps_name = arg_fname;
          tsps_type_desc = `tuple [ `type_reference "number"; `type_reference "int_list"; ]; } ] in
  let cstrs = ["IntNil", int_nil; "IntCons", int_cons] in
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "int_list";
          tsa_type_parameters = [];
          tsa_type_desc = `union (List.map snd cstrs); };
      `function_declaration
        { tsf_modifiers = [`export];
          tsf_name = "analyze_int_list";
          tsf_type_parameters = [ret];
          tsf_parameters =
            Util.Ts_ast.(case_analyzer_parameters { discriminator; var_x; var_v; var_fns; ret; } cstrs);
          tsf_type_desc =
            `func_type
              { tsft_parameters =
                  [ { tsp_name = var_x;
                      tsp_type_desc = `type_reference "int_list"; } ];
                tsft_type_desc = `type_reference ret; };
          tsf_body =
            Util.Ts_ast.(case_analyzer_body "int_list" { discriminator; var_x; var_v; var_fns; ret; } cstrs); } ]
