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

let example_module_path = "Bindoj_test_common_typedesc_examples.Ex05"

let decl : type_decl =
  record_decl "complex_types" [
    record_field "option" (Coretype.(mk_option (prim `int)));

    record_field "list" (Coretype.(mk_list (prim `int)));

    record_field "tuple" (Coretype.(mk_tuple [prim `int; prim `int]));

    record_field "nested" (Coretype.(
      mk_tuple [
        option (prim `int);
        list (prim `int);
        tuple [prim `int; prim `int];
      ]
    ));

    record_field "map" (Coretype.(mk_map `string (prim `int)));
  ]

let decl_with_docstr : type_decl =
  record_decl "complex_types" [
    record_field "option" (Coretype.(mk_option (prim `int)))
      ~doc:(`docstr "int option");

    record_field "list" (Coretype.(mk_list (prim `int)))
      ~doc:(`docstr "int list");

    record_field "tuple" (Coretype.(mk_tuple [prim `int; prim `int]))
      ~doc:(`docstr "(int * int)");

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

let fwrt : (unit, unit) fwrt_decl =
  let annot = () in
  "complex_types", FwrtTypeEnv.(
    init
    |> bind_object ~annot "complex_types" [
      field ~annot "option" (Coretype.(mk_option (prim `int)));
      field ~annot "list"   (Coretype.(mk_list (prim `int)));
      field ~annot "tuple"  (Coretype.(mk_tuple [prim `int; prim `int]));
      field ~annot "nested" (
        Coretype.(
          mk_tuple [
            option (prim `int);
            list (prim `int);
            tuple [prim `int; prim `int];
          ]
        )
      );
      field ~annot "map" (Coretype.(mk_map `string (prim `int)));
    ]
  )

let ts_ast : ts_ast option = None
