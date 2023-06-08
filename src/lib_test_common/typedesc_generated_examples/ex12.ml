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
include Bindoj_gen_test_gen_output.Ex12_gen
open Bindoj_base

type t = [ `case1 | `case2 | `case3 ] [@@deriving show]

let decl = Bindoj_test_common_typedesc_examples.Ex12.decl
let reflect = cases_reflect

let json_shape_explanation = cases_json_shape_explanation
let to_json = cases_to_json
let of_json' = cases_of_json'
let env = empty_tdenv
let t : t Alcotest.testable = Alcotest.of_pp pp

let sample_value01 : t Sample_value.t = {
  orig = `case1; jv = `str "case1"
}

let sample_value02 : t Sample_value.t = {
  orig = `case2; jv = `str "case2"
}

let sample_value03 : t Sample_value.t = {
  orig = `case3; jv = `str "case3"
}

let sample_values = [
  sample_value01;
  sample_value02;
  sample_value03;
]
