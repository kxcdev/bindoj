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
include Bindoj_gen_test_gen_output.Ex04_gen
open Bindoj_base

type t = [ `Foo0  | `Foo1 of int  | `Foo2 of (int * int) ] [@@deriving show]
let decl = Bindoj_test_common_typedesc_examples.Ex04.decl
let reflect = foo_reflect

let json_discriminator_value = foo_json_discriminator_value
let json_shape_explanation = foo_json_shape_explanation
let to_json = foo_to_json
let of_json' = foo_of_json'
let env = empty_tdenv
let t : t Alcotest.testable = Alcotest.of_pp pp

type sample = t Sample_value.t
open Sample_value
open Sample_value.JvHelper

let sample_value01 : sample = {
  orig = `Foo0;
  jv = ctor0 "foo0";
}

let sample_value02 : sample = {
  orig = `Foo1 1;
  jv = ctor1 "foo1" (`num 1.);
}

let sample_value03 : sample = {
  orig = `Foo2 (1, 2);
  jv = ctor2 "foo2" (`num 1.) (`num 2.);
}

let sample_values = [
  sample_value01;
  sample_value02;
  sample_value03;
]
