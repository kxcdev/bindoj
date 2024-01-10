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
open Ppxlib
open Ast_helper
open Bindoj_ppxlib_utils
open Bindoj_objintf_shared
open Bindoj_objintf_gen
open Bindoj_objintf_gen_jsoo
open Bindoj_gen
open Bindoj_test_common_objintf_examples

let gen_structure =
  fun ~formatter ~trans_party (module M: Utils.Ex) ->
    let open M in
    let structure: Ppxlib.structure =
      Caml_bridge.gen_structure
        ~resolution_strategy:caml_resolution_strategy
        ~bridgeable_ident_resolver
        ~generators:[
          Jsoo_full_bridge.gen_full_bridge_impl
        ]
        (objintf_decl trans_party)
    in
    ([ "Bindoj_objintf_gen_jsoo_test_gen_utils",
        warning_attribute "-33" (* suppress 'unused open' warning *)
    ] |&> fun (s, attrs) -> Str.open_ & Opn.mk ~attrs & Mod.ident & lidloc s)
    @ structure
    |> Emitter.structure formatter

let gen_signature =
  fun ~formatter ~trans_party (module M: Utils.Ex) ->
    let open M in
    let signature: Ppxlib.signature =
      Caml_bridge.gen_signature
        ~resolution_strategy:caml_resolution_strategy
        ~bridgeable_ident_resolver
        ~generators:[
          Jsoo_full_bridge.gen_full_bridge_impl_signature
        ]
        (objintf_decl trans_party)
    in
    ([ "Bindoj_objintf_gen_jsoo_test_gen_utils",
        warning_attribute "-33" (* suppress 'unused open' warning *)
    ] |&> fun (s, attrs) -> Sig.open_ & Opn.mk ~attrs & lidloc s)
    @ signature
    |> Emitter.signature formatter

let gen_full_bridge_only_structure =
  fun ~formatter ~trans_party (module M: Utils.Ex) ->
    let open M in
    let structure: Ppxlib.structure =
      Jsoo_full_bridge.gen_full_bridge_impl
        ~resolution_strategy:caml_resolution_strategy
        ~bridgeable_ident_resolver
        (objintf_decl trans_party)
    in
    ([ sprintf "Bindoj_objintf_gen_test_gen_output.%s%s_gen"
        M.module_name
        (match trans_party with | Cis_party -> "" | Trans_party -> "_trans"),
       [];
       "Bindoj_objintf_gen_jsoo_test_gen_utils",
        warning_attribute "-33" (* suppress 'unused open' warning *);
       Bridge_labels.(concrete_bridge_interfaces^"."^interfaces),
        warning_attribute "-33" (* suppress 'unused open' warning *); ]
      |&> fun (s, attrs) -> Str.open_ & Opn.mk ~attrs & Mod.ident & lidloc s)
    @ structure
    |> Emitter.structure formatter

let gen_full_bridge_only_signature =
  fun ~formatter ~trans_party (module M: Utils.Ex) ->
    let open M in
    let signature: Ppxlib.signature =
      Jsoo_full_bridge.gen_full_bridge_impl_signature
        ~resolution_strategy:caml_resolution_strategy
        ~bridgeable_ident_resolver
        (objintf_decl trans_party)
    in
    ([ sprintf "Bindoj_objintf_gen_test_gen_output.%s%s_gen"
        M.module_name
        (match trans_party with | Cis_party -> "" | Trans_party -> "_trans"),
        [];
        "Bindoj_objintf_gen_jsoo_test_gen_utils",
        warning_attribute "-33" (* suppress 'unused open' warning *);
        Bridge_labels.(concrete_bridge_interfaces^"."^interfaces),
        warning_attribute "-33" (* suppress 'unused open' warning *); ]
      |&> fun (s, attrs) -> Sig.open_ & Opn.mk ~attrs & lidloc s)
    @ signature
    |> Emitter.signature formatter

let mapping =
  All.all |&>> (fun ((module M: Utils.Ex) as m) ->
    let name = String.lowercase_ascii M.module_name in
    [ sprintf "%s_jsoo_gen.ml" name, (m, Cis_party, `structure);
      sprintf "%s_jsoo_gen.mli" name, (m, Cis_party, `signature);
      sprintf "%s_jsoo_trans_gen.ml" name, (m, Trans_party, `structure);
      sprintf "%s_jsoo_trans_gen.mli" name, (m, Trans_party, `signature);])

let () =
    let full_bridge_only = ArgOptions.has_flag "-full-bridge-only" in
    match Array.to_list Sys.argv |> List.tl with
    | [] ->
      failwith "usage: gen <filename>"
    | name :: _ ->
      match List.assoc_opt name mapping with
      | None -> failwith' "unknown example %s" name
      | Some (m, trans_party, `structure) ->
        let formatter = Format.std_formatter in
        if full_bridge_only then
          gen_full_bridge_only_structure ~formatter ~trans_party m
        else
          gen_structure ~formatter ~trans_party m
      | Some (m, trans_party, `signature) ->
        let formatter = Format.std_formatter in
        if full_bridge_only then
          gen_full_bridge_only_signature ~formatter ~trans_party m
        else
          gen_signature ~formatter ~trans_party m
