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
open Alcotest
open Kxclib
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
  let msg msg = sprintf "%s %s" name msg in

  let of_json jv = of_json' jv |> function
    | Ok x -> Some x
    | Error (msg, _, _) -> eprintf "%s\n" msg; None
  in


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
    test_case "[compiled] encoder works" `Quick (forall check_encoder);
    test_case "[compiled] decoder works" `Quick (forall check_decoder);
    test_case "[compiled] works in both direction" `Quick (forall check_combined);
  ]

let () =
  all
  |> List.map (fun (name, m) -> create_test_cases name m)
  |> Alcotest.run "lib_gen.json_codec"
