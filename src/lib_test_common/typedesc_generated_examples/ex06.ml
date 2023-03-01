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
include Bindoj_gen_test_gen_output.Ex06_gen
open Bindoj_base

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
let decl = Bindoj_test_common_typedesc_examples.Ex06.decl
let reflect = various_prim_types_reflect

let to_json = various_prim_types_to_json
let of_json = various_prim_types_of_json
let env = empty_tdenv

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
    "unit", `num 1.;
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
