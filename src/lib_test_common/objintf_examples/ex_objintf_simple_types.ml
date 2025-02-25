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
open Bindoj_objintf_shared
open Bindoj_typedesc.Type_desc

let caml_resolution_strategy : type_decl -> Coretype.codec -> _ =
  let tds =
    Bindoj_test_common_typedesc_examples.All.all
    |&>> (fun (_, (module Ex)) -> Ex.example_descs |&> (fun (module D) -> D.decl))
  in
  fun td _ ->
    if List.mem td tds then
      `no_resolution
    else
      `infile_type_definition None

let ts_type_decl_resolution_strategy : type_decl -> Coretype.codec -> _ =
  let import_location_map =
    Bindoj_test_common_typedesc_examples.All.all
      |&>> (fun (module_name, (module Ex)) ->
        Ex.example_descs |&> fun (module D) ->
          (D.decl, `import_location (sprintf "../../compile-tests/%s_gen" module_name)))
  in
  fun td _ ->
  import_location_map |> List.assoc td

let ts_ident_resolution_strategy _ = failwith "unexpected ident"

type bridgeable_ident = [ `never ]

let bridgeable_ident_resolver `never = failwith "never called"

let module_name = "Ex_objintf_simple_types"

let objintf_decl polarity: bridgeable_ident sameworld_objintf =
  let coretyep_named_objects =
    let prims = Coretype.[
      "unit", prim `unit;
      "bool", prim `bool;
      "int", prim `int;
      "int53p", prim `int53p;
      "float", prim `float;
      "string", prim `string;
      "uchar", prim `uchar;
      "byte", prim `byte;
      "bytes", prim `bytes;
    ]
  in
  Coretype.(
      (prims |&>> fun (name, ct) ->
        [
          name, ct;
          name^"_option", option ct;
          name^"_list", list ct;
          name^"_map", map `string ct;
        ] @ (prims |&> fun (n, c) ->
          sprintf "%s_tuple_with_%s" name n, tuple [ ct; c])
      ) @ [ "string_enum", string_enum [
              string_enum_case "a";
              string_enum_case "b";
              string_enum_case "c";
            ]
        ]
    ) |&> (fun (name, ct) ->
      named_object_decl ("cty_"^name) ~party:`endemic ~typ:(`direct (Coretype.mk ct)))
  in
  let type_decl_named_objects =
    Bindoj_test_common_typedesc_examples.All.all
    |&>> (fun (module_name, (module Ex)) ->
       let codec = `open_ ("Bindoj_test_common_typedesc_generated_examples."^(String.capitalize_ascii module_name)) in
        Ex.example_descs |&> fun (module D) ->
          named_object_decl ("td_"^D.decl.td_name)
            ~party:`endemic
            ~typ:(`nested(D.decl, codec)))
  in

  let named_objects = coretyep_named_objects @ type_decl_named_objects in

  sameworld_objintf ~name:module_name ~polarity ~named_objects ~object_registries:[] []
