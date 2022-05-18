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

include Bindoj_gen_test.Ex03_gen

type t = int_list =
  | IntNil
  | IntCons of int * t
  [@@deriving show, eq]

let encode_json = encode_int_list_json
let decode_json = decode_int_list_json
let t : t Alcotest.testable = Alcotest.testable pp equal

let sample_value01 = IntNil

let sample_value02 = IntCons (1, IntCons (2, IntNil))

let sample_value03 = IntCons (1, IntCons (2, IntCons (3, IntCons (4, IntNil))))

let sample_values = [
  sample_value01;
  sample_value02;
  sample_value03;
]
