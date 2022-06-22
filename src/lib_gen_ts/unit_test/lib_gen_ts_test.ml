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

open Bindoj_gen_ts.Typescript_datatype
open Bindoj_test_common

let testable_ts_ast =
  Alcotest.testable pp_ts_ast equal_ts_ast

let create_cases doc (module Ex : Typedesc_examples.T) =
  let open Alcotest in
  match Ex.ts_ast with
  | Some ts_ast ->
    let create_test () =
      Alcotest.check testable_ts_ast doc
        (ts_ast_of_fwrt_decl (annotate_fwrt_decl true false Ex.fwrt)) ts_ast in
    (doc, [test_case "ts_ast_of_fwrt_decl and annotate_fwrt_decl work" `Quick create_test])
  | None ->
    (doc, [test_case "(skipped)" `Quick (fun () -> ())])

let () =
  let open Alcotest in
  let open Kxclib in
  Typedesc_examples.all
  |&> (fun (name, m) -> create_cases name m)
  |> run "lib_gen_ts: ts_ast"
