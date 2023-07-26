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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex04"

let cty_int = Coretype.mk_prim `int

let decl : type_decl =
  variant_decl "foo" [
    variant_constructor "Foo0" `no_param;
    variant_constructor "Foo1" (`tuple_like [variant_argument cty_int]);
    variant_constructor "Foo2" (`tuple_like [variant_argument cty_int; variant_argument cty_int]);
  ] ~configs:[
    Caml_config.variant_type `polymorphic
  ]

let decl_with_docstr : type_decl =
  variant_decl "foo" [
    variant_constructor "Foo0" `no_param
      ~doc:(`docstr "polyvariant case (length=0)");
    variant_constructor "Foo1" (`tuple_like [variant_argument cty_int])
      ~doc:(`docstr "polyvariant case (length=1)");
    variant_constructor "Foo2" (`tuple_like [variant_argument cty_int; variant_argument cty_int])
      ~doc:(`docstr "polyvariant case (length=2)");
  ] ~configs:[
    Caml_config.variant_type `polymorphic
  ] ~doc:(`docstr "polyvariant")

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  let parent = "foo" in
  "foo", Util.FwrtTypeEnv.(
    let va_int = variant_argument cty_int in
    init
    |> bind_object ~configs:[Caml_config.variant_type `polymorphic] "foo" []
    |> bind_constructor ~parent "Foo0"
    |> bind_constructor ~parent "Foo1" ~args:[va_int]
    |> bind_constructor ~parent "Foo2" ~args:[va_int; va_int]
  )

let ts_ast : ts_ast option =
  let discriminator = "kind" in
  let arg_fname = "arg" in
  let foo0 =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "foo0"); } ] in
  let foo1 =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "foo1"); };
        { tsps_modifiers = [];
          tsps_name = arg_fname;
          tsps_type_desc = `type_reference "number"; } ] in
  let foo2 =
    `type_literal
      [ { tsps_modifiers = [];
          tsps_name = discriminator;
          tsps_type_desc = `literal_type (`string_literal "foo2"); };
        { tsps_modifiers = [];
          tsps_name = arg_fname;
          tsps_type_desc = `tuple [`type_reference "number"; `type_reference "number"]; } ] in
  let foos = ["Foo0", foo0; "Foo1", foo1; "Foo2", foo2] in
  let options : Util.Ts_ast.options =
    { discriminator;
      var_v = "__bindoj_v";
      var_x = "__bindoj_x";
      var_fns = "__bindoj_fns";
      ret = "__bindoj_ret" } in
  Some
    [ `type_alias_declaration
        { tsa_modifiers = [`export];
          tsa_name = "Foo";
          tsa_type_parameters = [];
          tsa_type_desc = `union (List.map snd foos); };
      Util.Ts_ast.case_analyzer "Foo" "analyzeFoo" options foos ]

let expected_json_shape_explanation =
  Some (
    `with_warning
      ("not considering any config if exists",
        (`named
          ("Foo",
            (`anyone_of
                [`object_of
                  [`mandatory_field ("kind", (`exactly (`str "foo0")))];
                `object_of
                  [`mandatory_field ("kind", (`exactly (`str "foo1")));
                  `mandatory_field ("arg", (`tuple_of [`integral]))];
                `object_of
                  [`mandatory_field ("kind", (`exactly (`str "foo2")));
                  `mandatory_field ("arg", (`tuple_of [`integral; `integral]))]]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option = None
