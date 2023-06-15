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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex03_objtuple"

let configs : [`variant_constructor] configs =
  let open Bindoj_codec.Json in
  [Json_config.tuple_style (`obj `default)]

let decl : type_decl =
  variant_decl "int_list" [
    variant_constructor "IntNil" `no_param;
    variant_constructor "IntCons" (`tuple_like [
      Coretype.mk_prim `int;
      Coretype.mk_self ();
    ]) ~configs;
  ]

let decl_with_docstr : type_decl =
  variant_decl "int_list" [
    variant_constructor "IntNil" `no_param
      ~doc:(`docstr "nil for int_list");

    variant_constructor "IntCons" (`tuple_like [
      Coretype.mk_prim `int;
      Coretype.mk_self ();
    ]) ~configs
       ~doc:(`docstr "cons for int_list");

  ] ~doc:(`docstr "int list")

let fwrt : (unit, unit) ts_fwrt_decl =
  let parent = "int_list" in
  "int_list", Util.FwrtTypeEnv.(
    init
    |> bind_object "int_list" []
    |> bind_constructor ~parent "IntNil"
    |> bind_constructor ~parent "IntCons" ~configs ~args:[Coretype.mk_prim `int; Coretype.mk_self ()]
  )

let ts_ast : ts_ast option =
  let discriminator = "kind" in
  let int_nil =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "intnil"); } ] in
  let int_cons =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "intcons"); };
        { tsps_modifiers = [];
          tsps_name = "_0";
          tsps_type_desc = `type_reference "number" };
        { tsps_modifiers = [];
          tsps_name = "_1";
          tsps_type_desc = `type_reference "IntList" } ] in
  let cstrs = ["IntNil", int_nil; "IntCons", int_cons] in
  let options : Util.Ts_ast.options =
    { discriminator;
      var_v = "__bindoj_v";
      var_x = "__bindoj_x";
      var_fns = "__bindoj_fns";
      ret = "__bindoj_ret" } in
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "IntList";
          tsa_type_parameters = [];
          tsa_type_desc = `union (List.map snd cstrs); };
      Util.Ts_ast.case_analyzer "IntList" "analyzeIntList" options cstrs; ]

open Bindoj_openapi.V3

let schema_object : Schema_object.t option =
  Util.Schema_object.variant "IntList"
    Schema_object.[
      "intnil", [];
      "intcons", [
        "_0", integer ();
        "_1", ref "#IntList"
      ];
    ]
  |> Option.some
