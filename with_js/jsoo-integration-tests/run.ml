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

module Import = Bindoj_withjs_import

module Ts = Bindoj_withjs_import.Ts2ocaml
module Faker = Bindoj_withjs_import.Json_schema_faker

open Bindoj_test_common.Typedesc_generated_examples

let notNone (_: 'a Alcotest.testable) : 'a option Alcotest.testable =
  Alcotest.testable
    (fun ppf -> function
      | None -> Format.pp_print_string ppf "None"
      | Some _ -> Format.pp_print_string ppf "Some _")
    (fun x y -> match x, y with | (Some _, Some _) -> true | _ -> false)

let create_test_cases name (module Ex : T) =
  let open Alcotest in

  (* let gen  = Js.require (sprintf "../compile-tests/%s_gen" name) in *)
  let json = Js.require (sprintf "../compile-tests/%s_examples.json" name) in
  let schema = Js.require (sprintf "../compile-tests/%s_schema.json" name) in

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
    let values = Ex.sample_values |> List.map Sample_value.orig in
    check (list Ex.t) "same value(s)" values json_samples
  in

  let test_schema_generate () =
    let module F = Faker.JSONSchemaFaker in
    let setOption = F.option () |> Ts.Intersection.get_1 in
    setOption (
      Faker.JSONSchemaFakerOptions.create ~fillProperties:false ()
    );
    for i = 1 to 10 do
      let value = F.generate ~schema ()  in
      let json =
        value
        |> Js.Json.stringify
        |> Yojson.Safe.from_string
      in
      let json_str = Yojson.Safe.pretty_to_string json in
      check (notNone Ex.t)
        (sprintf "random example #%d can be parsed: %s" i json_str)
        (Some (Obj.magic ()))
        (json |> Json.of_yojson |> Ex.decode_json)
    done
  in

  name, [
    test_case "can decode example JSONs" `Quick test_decode_json;
    test_case "can decode randomly generated JSONs" `Quick test_schema_generate;
  ]

let () =
  all
  |> List.map (fun (name, m) -> create_test_cases name m)
  |> Alcotest.run "with_js.jsoo_integration"
