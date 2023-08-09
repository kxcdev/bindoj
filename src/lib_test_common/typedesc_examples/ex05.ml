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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex05"

let tuple_configs : [`coretype] configs =
  let open Bindoj_codec.Json in
  [Json_config.tuple_style (`obj `default)]

let cty_int_opt = Coretype.(mk_option % prim) `int
let cty_int_lst = Coretype.(mk_list % prim) `int
let cty_int_tpl = Coretype.(mk_tuple % List.map prim) [ `int; `int ]
let cty_int_obt = Coretype.(mk_tuple ~configs:tuple_configs % List.map prim) [ `int; `int ]
let cty_int_map = Coretype.(mk_map `string % prim) `int

let decl : type_decl =
  record_decl "complex_types" [
    record_field "option" cty_int_opt;

    record_field "list" cty_int_lst;

    record_field "tuple" cty_int_tpl;

    record_field "objtuple" cty_int_obt;

    record_field "nested" (Coretype.(
      mk_tuple [
        cty_int_opt.ct_desc;
        cty_int_lst.ct_desc;
        cty_int_tpl.ct_desc;
      ]
    ));

    record_field "map" cty_int_map;
  ]

let decl_with_docstr : type_decl =
  record_decl "complex_types" [
    record_field "option" (Coretype.(mk_option (prim `int)))
      ~doc:(`docstr "int option");

    record_field "list" (Coretype.(mk_list (prim `int)))
      ~doc:(`docstr "int list");

    record_field "tuple" (Coretype.(mk_tuple [prim `int; prim `int]))
      ~doc:(`docstr "(int * int)");

    record_field "objtuple" (Coretype.(mk_tuple ~configs:tuple_configs [prim `int; prim `int]))
      ~doc:(`docstr "(int * int) (as object)");

    record_field "nested" (Coretype.(
      mk_tuple [
        option (prim `int);
        list (prim `int);
        tuple [prim `int; prim `int];
      ]
    )) ~doc:(`docstr "(int option * int list * (int * int))");

    record_field "map" (Coretype.(mk_map `string (prim `int)))
      ~doc:(`docstr "map<string, int>");
  ] ~doc:(`docstr "collection of complex types")

let fwrt : (unit, unit, unit) ts_fwrt_decl =
  "complex_types", Util.FwrtTypeEnv.(
    init
    |> bind_object "complex_types" [
      field "option" (Coretype.(mk_option (prim `int)));
      field "list"   (Coretype.(mk_list (prim `int)));
      field "tuple"  (Coretype.(mk_tuple [prim `int; prim `int]));
      field "objtuple" (Coretype.(mk_tuple ~configs:tuple_configs [prim `int; prim `int]));
      field "nested" (
        Coretype.(
          mk_tuple [
            option (prim `int);
            list (prim `int);
            tuple [prim `int; prim `int];
          ]
        )
      );
      field "map" (Coretype.(mk_map `string (prim `int)));
    ]
  )

let ts_ast : ts_ast option = None

let expected_json_shape_explanation =
  Some (
    `with_warning
      ("not considering any config if exists",
        (`named
          ("ComplexTypes",
            (`object_of
                [`optional_field ("option", `integral);
                `mandatory_field ("list", (`array_of `integral));
                `mandatory_field ("tuple", (`tuple_of [`integral; `integral]));
                `mandatory_field
                  ("objtuple", (`tuple_of [`integral; `integral]));
                `mandatory_field
                  ("nested",
                    (`tuple_of
                      [`nullable `integral;
                      `array_of `integral;
                      `tuple_of [`integral; `integral]]));
                `mandatory_field ("map", (`record_of `integral))]))))
  )

open Bindoj_openapi.V3

let schema_object : Schema_object.t option = None
