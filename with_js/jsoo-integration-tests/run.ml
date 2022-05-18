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

open Bindoj_test_common

let create_test_cases name (module Ex : Typedesc_generated_examples.T) =
  let open Alcotest in

  let gen  = Js.require (sprintf "../compile-tests/%s_ts_gen" name) in
  let json = Js.require (sprintf "../compile-tests/%s_examples.json" name) in

  let test_require () =
    check pass "should not fail" (gen, json) (gen, json)
  in

  let test_decode_json () =
    let json_samples =
      json
      |> Js.to_array
      |> Array.to_list
      |> List.filter_map (fun x ->
        x
        |> Js.Json.stringify
        |> Yojson.Safe.from_string
        |> Json.of_yojson
        |> Ex.decode_json)
    in
    check (list Ex.t) "same value(s)" Ex.sample_values json_samples
  in

  name, [
    test_case "can require test files (.ts, .json)" `Quick test_require;
    test_case "Ex.decode_json works" `Quick test_decode_json
  ]

let () =
  Typedesc_generated_examples.all
  |> List.map (fun (name, m) -> create_test_cases name m)
  |> Alcotest.run "jsoo_integration_tests" ~compact:true
