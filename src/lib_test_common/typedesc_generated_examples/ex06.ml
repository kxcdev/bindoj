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

include Bindoj_gen_test_gen_output.Ex06_gen

type uchar = Uchar.t
let pp_uchar ppf x = Format.pp_print_char ppf (Uchar.to_char x)

type t = various_prim_types = {
  unit:   unit;
  bool:   bool;
  int:    int;
  float:  float;
  string: string;
  uchar:  uchar;
  byte:   char;
  bytes:  Bytes.t;
} [@@deriving show]

let to_json = various_prim_types_to_json
let of_json = various_prim_types_of_json

let t : t Alcotest.testable = Alcotest.of_pp pp

type sample = t Sample_value.t
open Sample_value

let sample_value01 : sample = {
  orig = {
    unit = ();
    bool = true;
    int = 42;
    float = 4.2;
    string = "foo";
    uchar = Uchar.of_char 'a';
    byte = 'b';
    bytes = Bytes.of_string "Hello, world!";
  };
  jv = `obj [
    "unit", `null;
    "bool", `bool true;
    "int", `num 42.;
    "float", `num 4.2;
    "string", `str "foo";
    "uchar", `str (String.of_seq (List.to_seq ['a']));
    "byte", `num (float_of_int (int_of_char 'b'));
    "bytes", `str "SGVsbG8sIHdvcmxkIQ==";
  ]
}

let sample_values = [sample_value01];
