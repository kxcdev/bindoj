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
open Bindoj_typedesc
open Bindoj_typedesc.Typed_type_desc
open Bindoj_objintf_shared
open Bindoj_test_common_typedesc_generated_examples.Ex_record

open struct
  let cty_unit = Coretype.mk_prim `unit
  let cty_string = Coretype.mk_prim `string

  let ttd_my_string = Coretypes.(
    Prims.string |> to_typed_type_decl "my_string"
  )

  let ttd_student = Student.(Typed.mk decl reflect)
end

let caml_resolution_strategy td _ =
  match td with
  | { td_name = "my_string"; _ } -> `inline_type_definition
  | _ -> `infile_type_definition None

let ts_type_decl_resolution_strategy _ _ = `infile_type_definition

let ts_ident_resolution_strategy = function
  | { Coretype.id_name = "non_json_values"; _ } -> `import_location "./utils"
  | _ -> failwith "unexpected ident!"

type bridgeable_ident = [
  | `hello
  | `sole_var
  | `unit_sole
  | `unit_obj
  | `unit_mod
  | `rec_obj
  | `with_default_value
]

let bridgeables : (bridgeable_ident * (_, bridgeable_ident) bridgeable_decl) list =
  let simple_method_with_trailing_units ?(pargs=[]) ?largs n =
    simple_method
      ~pargs:(pargs @ (Functionals.ntimes n (List.cons & positional_argument_regular (`direct cty_unit)) []))
      ?largs
  in
  let make_unit_bundle name style =
    simple_bridgeable & method_bundle_bridgeable name [
      simple_method "name"
        (method_return_type_regular & `direct cty_string);
      simple_method_with_trailing_units 1 "unit_01"
        ~pargs:[
          positional_argument_regular (`direct cty_string);
        ]
        (method_return_type_regular & `direct cty_string);
      simple_method_with_trailing_units 2 "unit_02"
        ~pargs:[
          positional_argument_regular ~name:"parg_name" (`direct cty_string);
        ]
        (method_return_type_regular & `direct cty_string);
      simple_method_with_trailing_units 3 "unit_03"
        ~pargs:[
          positional_argument_regular (`direct cty_string);
        ]
        (method_return_type_regular & `direct cty_string);
    ] ~configs:[ Objintf_config.caml_style style ]
  in
  let make_rec_bundle name style =
    complex_bridgeable & method_bundle_bridgeable name [
      simple_method "name"
        (method_return_type_regular & `direct cty_string);
      complex_method "get_self"
        (method_return_type_regular (`self_bridgeable `peer));
    ] ~configs:[ Objintf_config.caml_style style ]
  in
  [
    `hello,
      simple_bridgeable & sole_method_bridgeable &
        simple_method "hello"
          ~pargs:[
            positional_argument_regular ~name:"parg_name" (`direct cty_string);
          ]
          (method_return_type_regular & `direct cty_unit);

    `sole_var,
      complex_bridgeable & sole_method_bridgeable &
        simple_method "sole_var"
          (method_return_type_regular & `direct cty_string);

    `unit_sole,
      simple_bridgeable & sole_method_bridgeable &
      simple_method_with_trailing_units 1 "unit_sole"
        ~pargs:[
          positional_argument_regular (`direct cty_string);
          positional_argument_regular (`direct cty_unit);
        ]
        (method_return_type_regular & `direct cty_string);

    `unit_obj, make_unit_bundle "unit_obj" `object_;
    `unit_mod, make_unit_bundle "unit_mod" `module_;

    `rec_obj, make_rec_bundle "rec_obj" `object_;

    `with_default_value,
      simple_bridgeable & method_bundle_bridgeable "with_default_value" [
        simple_method "get_default_string"
          ~largs:[
            labeled_argument_with_default "larg_str" ttd_my_string ~default:"Hello"
          ]
          (method_return_type_regular & `direct cty_string);
        simple_method "get_default_student"
          ~largs:[
            labeled_argument_with_default "larg_student"
              ttd_student
              ~default:{
                admission_year = 1984;
                name = "William Gibson";
              }
          ]
          (method_return_type_regular & `direct cty_string);
      ] ~configs:[ Objintf_config.caml_style `module_ ];
  ]

let bridgeable_ident_resolver = Fn.flip List.assoc bridgeables

let module_name = "Ex_objintf_one_directional"

let objintf_decl polarity: bridgeable_ident sameworld_objintf =
  sameworld_objintf ~name:module_name ~polarity (bridgeables |&> snd) ~named_objects:[
    named_object_decl "my_string"
      ~party:`peer
      ~typ:(`direct cty_string);
    named_object_decl "my_hello"
      ~party:`peer
      ~typ:(`bridgeable (`peer, `hello));
    named_object_decl "my_sole_var"
      ~party:`peer
      ~typ:(`bridgeable (`peer, `sole_var));
    named_object_decl "my_unit_sole"
      ~party:`peer
      ~typ:(`bridgeable (`peer, `unit_sole));
    named_object_decl "my_unit_obj"
      ~party:`peer
      ~typ:(`bridgeable (`peer, `unit_obj));
    named_object_decl "my_unit_mod"
      ~party:`peer
      ~typ:(`bridgeable (`peer, `unit_mod));
    named_object_decl "my_rec_obj"
      ~party:`peer
      ~typ:(`bridgeable (`peer, `rec_obj));
    named_object_decl "my_non_json_values"
      ~party:`peer
      ~typ:(`direct (Coretype.mk_ident ~codec:(`open_ "Utils") "non_json_values"));
    named_object_decl "with_default_value"
      ~party:`peer
      ~typ: (`bridgeable (`peer, `with_default_value));
  ] ~object_registries:[
    object_registry_decl "string"
      ~coordinate_desc:[
        "cdn_id0", `prim `string;
        "cdn_id1", `prim `int53p;
      ]
      ~party:`peer
      ~typ:(`direct cty_string);
    object_registry_decl "hello"
      ~coordinate_desc:[
        "cdn_id", `prim `string;
      ]
      ~party:`peer
      ~typ:(`bridgeable (`peer, `hello));
  ]
