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
open Bindoj_objintf_gen_ts
open Bindoj_test_common_objintf_examples

let mapping =
  All.all |&>> (fun ((module M: Utils.Ex) as m) ->
    let name = String.lowercase_ascii M.module_name in
    [ sprintf "%s_gen.ts" name, (m, Cis_party);
      sprintf "%s_trans_gen.ts" name, (m, Trans_party); ])

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [] ->
    failwith "usage: gen <filename> [-trans-party]"
  | name :: _ ->
    match List.assoc_opt name mapping with
    | None -> failwith' "unknown example %s" name
    | Some ((module M: Utils.Ex), party) ->
      let formatter = Format.std_formatter in

    Ts_bridge.gen_ts_bridge
      ~type_decl_resolution_strategy:M.ts_type_decl_resolution_strategy
      ~ident_resolution_strategy:M.ts_ident_resolution_strategy
      ~bridgeable_ident_resolver:M.bridgeable_ident_resolver
      ~bindoj_runtime_import_location:"../../public-packages/runtime"
      (M.objintf_decl party)
    |> Format.pp_print_string formatter
