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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex03"

let decl : type_decl =
  variant_decl "int_list" [
    variant_constructor "IntNil" `no_param;
    variant_constructor "IntCons" (`tuple_like [
      variant_argument @@ Coretype.mk_prim `int;
      variant_argument @@ Coretype.mk_self ();
    ]);
  ]

let decl_with_docstr : type_decl =
  variant_decl "int_list" [
    variant_constructor "IntNil" `no_param
      ~doc:(`docstr "nil for int_list");

    variant_constructor "IntCons" (`tuple_like [
      variant_argument @@ Coretype.mk_prim `int;
      variant_argument @@ Coretype.mk_self ();
    ]) ~doc:(`docstr "cons for int_list");

  ] ~doc:(`docstr "int list")

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  let parent = "int_list" in
  "int_list", Util.FwrtTypeEnv.(
    init
    |> bind_object "int_list" []
    |> bind_constructor ~parent "IntNil"
    |> bind_constructor ~parent "IntCons" ~args:[
      variant_argument @@ Coretype.mk_prim `int;
      variant_argument @@ Coretype.mk_self ();
    ]
  )

let ts_ast : ts_ast option =
  let discriminator = "kind" in
  let arg_fname = "arg" in
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
          tsps_name = arg_fname;
          tsps_type_desc = `tuple [ `type_reference "number"; `type_reference "IntList"; ]; } ] in
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

let expected_json_shape_explanation =
  Some (
    `with_warning
      ("not considering any config if exists",
        (`named
          ("IntList",
            (`anyone_of
                [`object_of
                  [`mandatory_field ("kind", (`exactly (`str "intnil")))];
                `object_of
                  [`mandatory_field ("kind", (`exactly (`str "intcons")));
                  `mandatory_field ("arg", (`tuple_of [`integral; `self]))]]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option =
  Util.Schema_object.variant "IntList"
    Schema_object.[
      "intnil", [];
      "intcons", [
        "arg", tuple [ integer(); ref "#IntList"; ]
      ];
    ]
  |> Option.some
