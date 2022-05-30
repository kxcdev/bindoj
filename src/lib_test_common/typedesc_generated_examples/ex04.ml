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

include Bindoj_gen_test_gen_output.Ex04_gen

type t = foo [@@deriving show]

let encode_json = encode_foo_json
let decode_json = decode_foo_json
let t : t Alcotest.testable = Alcotest.of_pp pp

type sample = t Sample_value.t
open Sample_value
open Sample_value.JvHelper

let sample_value01 : sample = {
  orig = `Foo0;
  jv = ctor0 "Foo0";
}

let sample_value02 : sample = {
  orig = `Foo1 1;
  jv = ctor1 "Foo1" (`num 1.);
}

let sample_value03 : sample = {
  orig = `Foo2 (1, 2);
  jv = ctor2 "Foo2" (`num 1.) (`num 2.);
}

let sample_values = [
  sample_value01;
  sample_value02;
  sample_value03;
]
