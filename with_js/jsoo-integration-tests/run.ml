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
open Bindoj_withjs_import

module Ts = Ts2ocaml
module Faker = Json_schema_faker

open Bindoj_base
open Bindoj_base.Runtime
open Bindoj_codec_config
open Bindoj_test_common.Typedesc_generated_examples

let list_find_remove_opt f =
  let rec go others = function
    | h :: _ when f h -> Some (h, List.rev others)
    | h :: t -> go (h :: others) t
    | [] -> None
  in
  go []

let notNone (_: 'a Alcotest.testable) : 'a option Alcotest.testable =
  Alcotest.testable
    (fun ppf -> function
      | None -> Format.pp_print_string ppf "None"
      | Some _ -> Format.pp_print_string ppf "Some _")
    (fun x y -> match x, y with | (Some _, Some _) -> true | _ -> false)

let all_schema : Ojs.t list =
  all |&>> (fun (name, _) ->
    Js.require (sprintf "../compile-tests/%s_schema.json" name) |> Ojs.list_of_js identity)

let create_test_cases name env filter (module D : Ex_generated_desc) =
  let open Alcotest in

  let self_type_name = Json_config.get_mangled_name_of_type D.decl |> fst in
  let self_schema_id = "#"^self_type_name in

  (* let gen  = Js.require (sprintf "../compile-tests/%s_gen" name) in *)
  let json =
    Js.require (sprintf "../compile-tests/%s_examples.json" name)
    |> Fn.flip Ojs.get_prop_ascii self_type_name
    |> Obj.magic
  in

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
    let values = D.sample_values |> List.map Util.Sample_value.orig in
    check (list D.t) "same value(s)" values json_samples
  in


  let get_id o = Ojs.(get_prop_ascii o "id" |> string_of_js) in
  let schema, all_schema =
      list_find_remove_opt (fun sc -> get_id sc = self_schema_id) all_schema
      |?! (fun () -> failwith "schema not found")
      |> Obj.magic
    in
  let test_schema_generate of_json () =
    let module F = Faker.JSONSchemaFaker in
    let open Bindoj_withjs_import.Jsonschema in

    let validator = Validator.create () in
    let () =
      all_schema
      |&> (fun sc -> let id = get_id sc in ("/" ^ id, sc))
      |> Array.of_list
      |> Ojs.obj
      |> Validator.AnonymousInterface2.t_of_js
      |> Validator.set_schemas validator
    in
    eprintf "%s" (Validator.get_schemas validator |> Js.Json.stringify);

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
        let refs = all_schema |> List.map Js.clone |> Ts.Union.inject_1 in
        F.generate ~schema ~refs ()
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
        check (notNone D.t)
          (sprintf "random example #%d can be parsed: %s" i json_str)
          (Some (Obj.magic ())) result
      end
    done
  in

  let handle_error = function
    | Ok x -> Some x
    | Error e ->
      eprintf "%s\n" (OfJsonResult.Err.to_string e); None
  in

  let interpreted jv =
    let open Typed_type_desc in
    let typed_decl = Typed.mk D.decl D.reflect in
    Bindoj_codec.Json.of_json' ~env typed_decl jv
    |> handle_error
  in
  let compiled = D.of_json' &> handle_error in

  sprintf "%s.%s" name D.decl.td_name, [
    (`interpreted, `examples),
    test_case "[interpreted] can decode example JSONs" `Quick (test_decode_json interpreted);

    (`interpreted, `random),
    test_case "[interpreted] can decode random JSONs" `Quick (test_schema_generate interpreted);

    (`compiled, `examples),
    test_case "[compiled] can decode example JSONs" `Quick (test_decode_json compiled);

    (`compiled, `random),
    test_case "[compiled] can decode random JSONs" `Quick (test_schema_generate compiled);
  ] |> List.filter (fun (variant, _) -> filter (name, D.decl.td_name, variant))
    |&> snd

let () =
  Bisect.Runtime.reset_counters();
  all
  |&>> (fun (name, (module E : Ex_generated)) ->
    E.example_generated_descs |&> (create_test_cases name E.env (function | _ -> true)))
  |> Alcotest.run "with_js.jsoo_integration";
  Bisect.Runtime.write_coverage_data();
