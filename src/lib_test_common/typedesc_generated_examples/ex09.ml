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
include Bindoj_gen_test_gen_output.Ex09_gen
open Bindoj_base

open Kxclib

let pp_int53p ppf x = fprintf ppf "%s" (Int53p.to_string x)
type t = with_int53p = { value : int53p; } [@@deriving show]
let decl = Bindoj_test_common_typedesc_examples.Ex09.decl
let reflect = with_int53p_reflect

let json_shape_explanation = with_int53p_json_shape_explanation
let to_json = with_int53p_to_json
let of_json' = with_int53p_of_json'
let env =
  empty_tdenv
  |> Bindoj_std.Tdenv_wrappers.json

let t : t Alcotest.testable = Alcotest.of_pp pp

let sample_value01 : t Sample_value.t =
  {
    orig = {
      value = (Int53p.of_int 0);
    };
    jv = `obj [ "value", `num 0.; ]
  }

let sample_value02 : t Sample_value.t =
  {
    orig = {
      value = (Int53p.of_int 1);
    };
    jv = `obj [ "value", `num 1.; ]
  }

let sample_value03 : t Sample_value.t =
  {
    orig = {
      value = (Int53p.of_int (-1));
    };
    jv = `obj [ "value", `num (-1.); ]
  }

let sample_value04 : t Sample_value.t =
  {
    orig = {
      value = (Int53p.of_int 102);
    };
    jv = `obj [ "value", `num 102.; ]
  }

let sample_value05 : t Sample_value.t =
  {
    orig = {
      value = (Int53p.of_int64 (1_099_511_627_776L));
    };
    jv = `obj [ "value", `num 1_099_511_627_776.; ]
  }

let sample_value06 : t Sample_value.t =
  {
    orig = {
      value = (Int53p.of_int64 (-2_199_023_255_552L));
    };
    jv = `obj [ "value", `num (-2_199_023_255_552.); ]
  }

let sample_values = [
  sample_value01;
  sample_value02;
  sample_value03;
  sample_value04;
  sample_value05;
  sample_value06;
]
