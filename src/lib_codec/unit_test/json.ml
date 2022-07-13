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

open Alcotest
open Kxclib
open Bindoj_base
open Bindoj_test_common.Typedesc_generated_examples

module Testables : sig
  val jv : Json.jv testable
end = struct
  type jv = [
    | `null
    | `bool of bool
    | `num of float
    | `str of string
    | `arr of jv list
    | `obj of (string * jv) list
  ] [@@ deriving show]
  let jv = testable pp_jv ( = )
end

let create_test_cases (name: string) (module Ex : T) =
  let open Ex in
  let open Bindoj_codec in
  let msg msg = sprintf "%s %s" name msg in
  let env = StringMap.empty in
  let typed_decl = Typed_type_desc.Typed.mk Ex.decl Ex.reflect in
  let to_json = Json.to_json ~env typed_decl in
  let of_json = Json.of_json ~env typed_decl in

  (* encoding *)
  let check_encoder (value: t Sample_value.t) =
    check Testables.jv (msg "encoding")
      value.jv
      (to_json value.orig)
  in

  (* decoding *)
  let check_decoder (value: t Sample_value.t) =
    check (option t) (msg "decoding")
      (Some value.orig)
      (of_json value.jv)
  in

  (* encoding & decoding *)
  let check_combined (value: t Sample_value.t) =
    check (option t) (msg "encoding -> decoding")
      (Some value.orig)
      (of_json (to_json value.orig));
    check (option Testables.jv) (msg "decoding -> encoding")
      (Some value.jv)
      (Option.map to_json (of_json value.jv))
  in

  let forall check = fun () -> sample_values |> List.iter check in
  name, [
    test_case "[interpreted] encoder works" `Quick (forall check_encoder);
    test_case "[interpreted] decoder works" `Quick (forall check_decoder);
    test_case "[interpreted] works in both direction" `Quick (forall check_combined);
  ]

let () =
  all
  |> List.map (fun (name, m) -> create_test_cases name m)
  |> Alcotest.run "lib_codec.json"
