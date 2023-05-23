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
include Bindoj_gen_test_gen_output.Ex03_objtuple_gen
open Bindoj_base

type t = int_list =
  | IntNil
  | IntCons of int * t
  [@@deriving show]
let decl = Bindoj_test_common_typedesc_examples.Ex03_objtuple.decl
let reflect = int_list_reflect

let json_shape_explanation = int_list_json_shape_explanation
let to_json = int_list_to_json
let of_json' = int_list_of_json'
let env = empty_tdenv
let t : t Alcotest.testable = Alcotest.of_pp pp

open Sample_value
open Sample_value.JvHelper

let intCons a b =
  ctor_record "IntCons" [
    "_0", `num (float_of_int a);
    "_1", b
  ]
let intNil = ctor0 "IntNil"

let sample_value01 = { orig = IntNil; jv = intNil }

let sample_value02 = {
  orig = IntCons (1, IntCons (2, IntNil));
  jv = intCons 1 (intCons 2 intNil);
}

let sample_value03 = {
  orig = IntCons (1, IntCons (2, IntCons (3, IntCons (4, IntNil))));
  jv = intCons 1 (intCons 2 (intCons 3 (intCons 4 intNil)));
}

let sample_value04 = {
    orig = IntCons (26335605, IntCons (35460072, IntNil));
    jv = intCons 26335605 (intCons 35460072 intNil)
  }

let sample_values = [
  sample_value01;
  sample_value02;
  sample_value03;
  sample_value04;
]
