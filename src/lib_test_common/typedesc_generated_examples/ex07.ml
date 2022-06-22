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

include Bindoj_gen_test_gen_output.Ex07_gen

type t = customized_union =
  | Case1 of int
  | Case2 of { x: int; y: int }
[@@deriving show]

let to_json = customized_union_to_json
let of_json = customized_union_of_json
let t : t Alcotest.testable = Alcotest.of_pp pp

type sample = t Sample_value.t
open Sample_value
open Sample_value.JvHelper

let discriminator = "tag"

let sample_value01 : sample = {
  orig = Case1 42;
  jv = ctor1 ~discriminator ~arg:"value" "Case1" (`num 42.);
}

let sample_value02 : sample = {
  orig = Case2 { x = 4; y = 2 };
  jv = ctor_record ~discriminator "Case2" [
    "x", `num 4.;
    "y", `num 2.;
  ]
}

let sample_values = [
  sample_value01;
  sample_value02;
]
