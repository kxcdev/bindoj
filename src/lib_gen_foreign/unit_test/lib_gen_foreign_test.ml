(* Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

open Bindoj_gen_foreign.Foreign_datatype
open Bindoj_test_common

module Ex01 = Typedesc_examples.Ex01
module Ex02 = Typedesc_examples.Ex02
module Ex03 = Typedesc_examples.Ex03


let testable_fwrt =
  let pp : (unit, unit) fwrt_decl Fmt.t = fun ppf (name, env) ->
    let ({ fd_name; fd_kind_fname; fd_parent; fd_children; _; }, _) =
      FwrtTypeEnv.lookup name env in
    Format.fprintf ppf
      "{ fd_name = %s; fd_kind_fname = %s; fd_parent = %s; fd_children = %s; }"
      fd_name
      fd_kind_fname
      (match fd_parent with
       | None -> "None"
       | Some x -> "Some "^x)
      (List.fold_right (fun x acc -> x^"; "^acc) fd_children ""
       |> fun content -> "["^content^"]")
  in
  let equal : (unit, unit) fwrt_decl -> (unit, unit) fwrt_decl -> bool = fun (name1, env1) (name2, env2) ->
    let open Kxclib in
    let ({ fd_children = children1; fd_fields = fields1; _; }, _) =
      FwrtTypeEnv.lookup name1 env1 in
    let children1 = List.sort compare children1 in
    let fields1 =
      (fields1 |&> fun ({ ff_name; ff_type; _; }, _) ->
          (ff_name, ff_type))
      |> List.sort (fun x y -> compare (fst x) (fst y)) in
    let ({ fd_children = children2; fd_fields = fields2; _; }, _) =
      FwrtTypeEnv.lookup name2 env2 in
    let children2 = List.sort compare children2 in
    let fields2 =
      (fields2 |&> fun ({ ff_name; ff_type; _; }, _) ->
          (ff_name, ff_type))
      |> List.sort (fun x y -> compare (fst x) (fst y)) in
    fields1 = fields2 && children1 = children2 in
  Alcotest.testable pp equal

let create_cases doc decl fwrt =
  let open Alcotest in
  let create_test () =
    Alcotest.check testable_fwrt doc
      (fwrt_decl_of_type_decl `flat_kind decl) fwrt in
  (doc, [test_case "fwrt_decl_of_type_decl works" `Quick create_test])

let () =
  let open Alcotest in
  run "lib_gen_foreign" [
    create_cases "ex01" Ex01.decl Ex01.fwrt;
    create_cases "ex02" Ex02.decl Ex02.fwrt;
    create_cases "ex03" Ex03.decl Ex03.fwrt;
  ]
