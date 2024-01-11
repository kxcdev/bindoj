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
open Bindoj_gen
open Bindoj_objintf_gen
open Bindoj_objintf_gen_jsoo
open Bindoj_example_objintf_jsoo_sameworld_desc.Desc

let () =
  let formatter = Format.std_formatter in
  match Array.to_list Sys.argv |> List.tl with
  | "ml" :: _ ->
    Caml_bridge.gen_structure
      ~resolution_strategy:caml_resolution_strategy
      ~bridgeable_ident_resolver
      ~generators:[
        Jsoo_full_bridge.gen_full_bridge_impl
      ]
      (objintf_decl Cis_party)
    |> Emitter.structure formatter
  | "mli" :: _ ->
    Caml_bridge.gen_signature
      ~resolution_strategy:caml_resolution_strategy
      ~bridgeable_ident_resolver
      ~generators:[
        Jsoo_full_bridge.gen_full_bridge_impl_signature
      ]
      (objintf_decl Cis_party)
    |> Emitter.signature formatter
  | _ -> failwith "usage: gen_jsoo <ml|mli>"