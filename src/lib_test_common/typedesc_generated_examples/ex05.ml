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
include Bindoj_gen_test_gen_output.Ex05_gen

type t = complex_types = {
  option: int option;
  list:   int list;
  tuple:  (int * int);
  objtuple: (int * int);
  nested: (int option * int list * (int * int));
  map: (string * int) list;
} [@@deriving show]
let decl = Bindoj_test_common_typedesc_examples.Ex05.decl
let reflect = complex_types_reflect

let to_json = complex_types_to_json
let of_json = complex_types_of_json
let t : t Alcotest.testable = Alcotest.of_pp pp

type sample = t Sample_value.t
open Sample_value

let sample_value01 : sample = {
  orig = {
    option = Some 42;
    list = [1;2;3;4];
    tuple = (4, 2);
    objtuple = (4, 2);
    nested = (Some 42, [4;2], (4,2));
    map = ["foo", 4; "bar", 2]
  };
  jv = `obj [
    "option", `num 42.;
    "list", `arr [`num 1.; `num 2.; `num 3.; `num 4.];
    "tuple", `arr [`num 4.; `num 2.];
    "objtuple", `obj ["_0", `num 4.; "_1", `num 2.];
    "nested", `arr [
      `num 42.;
      `arr [`num 4.; `num 2.];
      `arr [`num 4.; `num 2.];
    ];
    "map", `obj ["foo", `num 4.; "bar", `num 2.];
  ]
}

let sample_value02 : sample = {
  orig = {
    option = None;
    list = [];
    tuple = (0, 0);
    objtuple = (0, 0);
    nested = (None, [], (0,0));
    map = [];
  };
  jv = `obj [
    "option", `null;
    "list", `arr [];
    "tuple", `arr [`num 0.; `num 0.];
    "objtuple", `obj ["_0", `num 0.; "_1", `num 0.];
    "nested", `arr [
      `null;
      `arr [];
      `arr [`num 0.; `num 0.];
    ];
    "map", `obj [];
  ]
}

let sample_values = [
  sample_value01;
  sample_value02;
]
