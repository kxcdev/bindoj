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
include Bindoj_gen_test_gen_output.Ex05_notuple_gen
open Bindoj_base

type t = complex_types = {
  option: int option;
  list:   int list;
  map: (string * int) list;
} [@@deriving show]
let decl = Bindoj_test_common_typedesc_examples.Ex05_notuple.decl
let reflect = complex_types_reflect

let json_shape_explanation = complex_types_json_shape_explanation
let to_json = complex_types_to_json
let of_json' = complex_types_of_json'
let env = empty_tdenv
let t : t Alcotest.testable = Alcotest.of_pp pp

type sample = t Sample_value.t
open Sample_value

let sample_value01 : sample = {
  orig = {
    option = Some 42;
    list = [1;2;3;4];
    map = ["foo", 4; "bar", 2]
  };
  jv = `obj [
    "option", `num 42.;
    "list", `arr [`num 1.; `num 2.; `num 3.; `num 4.];
    "map", `obj ["foo", `num 4.; "bar", `num 2.];
  ]
}

let sample_value02 : sample = {
  orig = {
    option = None;
    list = [];
    map = [];
  };
  jv = `obj [
    "list", `arr [];
    "map", `obj [];
  ]
}

let sample_values = [
  sample_value01;
  sample_value02;
]
