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
include Bindoj_gen_test_gen_output.Ex08_gen
open Bindoj_base

open struct
  type jv = [
    | `null
    | `bool of bool
    | `num of float
    | `str of string
    | `arr of jv list
    | `obj of (string * jv) list
  ] [@@deriving show]
end

type t = named_json = { name : string; json : jv; } [@@deriving show]
let decl = Bindoj_test_common_typedesc_examples.Ex08.decl
let reflect = named_json_reflect

let json_shape_explanation = named_json_json_shape_explanation
let to_json = named_json_to_json
let of_json' = named_json_of_json'
let env =
  empty_tdenv
  |> Bindoj_std.Tdenv_wrappers.json

let t : t Alcotest.testable = Alcotest.of_pp pp

let sample_value01 : t Sample_value.t =
  let jv_pos = `obj [ ("x", `num 2.); ("y", `num (-5.)); ] in
  {
    orig = {
      name = "position";
      json = jv_pos;
    };
    jv = `obj [
        ("name", `str "position");
        ("json", jv_pos);
      ]
  }

let sample_value02 : t Sample_value.t =
  let jv_hello = `str "hello?" in
  {
    orig = {
      name = "greeting";
      json = jv_hello;
    };
    jv = `obj [
        ("name", `str "greeting");
        ("json", jv_hello);
      ]
  }

let sample_value03 : t Sample_value.t =
  let jv_tup = `arr [`str "x"; `num 2.; `str "y"; `num (-5.)] in
  {
    orig = {
      name = "tup_pos";
      json = jv_tup;
    };
    jv = `obj [
        ("name", `str "tup_pos");
        ("json", jv_tup);
      ]
  }

let sample_value04 : t Sample_value.t =
  let jv_bool = `bool true in
  {
    orig = {
      name = "flag";
      json = jv_bool;
    };
    jv = `obj [
        ("name", `str "flag");
        ("json", jv_bool);
      ]
  }

let sample_value05 : t Sample_value.t =
  let jv_null = `null in
  {
    orig = {
      name = "null_val";
      json = jv_null;
    };
    jv = `obj [
        ("name", `str "null_val");
        ("json", jv_null);
      ]
  }

let sample_values = [
  sample_value01;
  sample_value02;
  sample_value03;
  sample_value04;
  sample_value05;
]
