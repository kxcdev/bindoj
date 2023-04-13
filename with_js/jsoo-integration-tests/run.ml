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
open Bindoj_withjs_import

module Ts = Ts2ocaml
module Faker = Json_schema_faker

open Bindoj_base
open Bindoj_test_common.Typedesc_generated_examples

let notNone (_: 'a Alcotest.testable) : 'a option Alcotest.testable =
  Alcotest.testable
    (fun ppf -> function
      | None -> Format.pp_print_string ppf "None"
      | Some _ -> Format.pp_print_string ppf "Some _")
    (fun x y -> match x, y with | (Some _, Some _) -> true | _ -> false)

let create_test_cases name (module Ex : T) filter =
  let open Alcotest in

  (* let gen  = Js.require (sprintf "../compile-tests/%s_gen" name) in *)
  let json = Js.require (sprintf "../compile-tests/%s_examples.json" name) in
  let schema = Js.require (sprintf "../compile-tests/%s_schema.json" name) in

  let test_decode_json of_json () =
    let json_samples =
      json
      |> Js.to_array
      |> Array.to_list
      |> List.filter_map (fun x ->
        x
        |> Js.Json.stringify
        |> Yojson.Safe.from_string
        |> Json.of_yojson
        |> of_json)
    in
    let values = Ex.sample_values |> List.map Sample_value.orig in
    check (list Ex.t) "same value(s)" values json_samples
  in

  let test_schema_generate of_json () =
    let module F = Faker.JSONSchemaFaker in
    let open Bindoj_withjs_import.Jsonschema in

    let validator = Validator.create () in

    let setOption = F.option () |> Ts.Intersection.get_1 in
    setOption (
      Faker.JSONSchemaFakerOptions.create
        ~fillProperties:false
        ~failOnInvalidTypes:true
        ~failOnInvalidFormat:true
        ~replaceEmptyByRandomValue:true
        ()
    );

    for i = 1 to 100 do
      let instance =
        (* clone because faker pollutes the schema object *)
        let schema = Js.clone schema in
        F.generate ~schema ()
      in
      let result =
        (* validate because faker sometimes generates an invalid example *)
        Validator.validate validator ~schema ~instance ()
      in
      if ValidatorResult.get_valid result && ValidatorResult.get_errors result = [] then begin
        let json =
          instance
          |> Js.Json.stringify
          |> Yojson.Safe.from_string
        in
        let json_str = Yojson.Safe.pretty_to_string json in
        let result =
          json |> Json.of_yojson |> of_json
        in
        check (notNone Ex.t)
          (sprintf "random example #%d can be parsed: %s" i json_str)
          (Some (Obj.magic ())) result
      end
    done
  in

  let interpreted jv =
    let open Typed_type_desc in
    let env = Ex.env in
    let typed_decl = Typed.mk Ex.decl Ex.reflect in
    Bindoj_codec.Json.of_json' ~env typed_decl jv
    |> function
    | Ok x -> Some x
    | Error (msg, _, _) -> eprintf "%s\n" msg; None
  in
  let compiled = Ex.of_json in

  name, [
    (`interpreted, `examples),
    test_case "[interpreted] can decode example JSONs" `Quick (test_decode_json interpreted);

    (`interpreted, `random),
    test_case "[interpreted] can decode random JSONs" `Quick (test_schema_generate interpreted);

    (`compiled, `examples),
    test_case "[compiled] can decode example JSONs" `Quick (test_decode_json compiled);

    (`compiled, `random),
    test_case "[compiled] can decode random JSONs" `Quick (test_schema_generate compiled);
  ] |> List.filter (fun (variant, _) -> filter (name, variant))
    |&> snd

let () =
  all
  |> List.map (fun (name, m) ->
    create_test_cases name m
      (function | _ -> true))
  |> Alcotest.run "with_js.jsoo_integration"
