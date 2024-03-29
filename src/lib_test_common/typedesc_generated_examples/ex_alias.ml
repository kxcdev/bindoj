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
open Bindoj_base
open struct
  module Td_ex_alias = Bindoj_test_common_typedesc_examples.Ex_alias
end

include Bindoj_gen_test_gen_output.Ex_alias_gen

module Unit = struct
  type t = unit [@@deriving show]

  let decl = Td_ex_alias.Unit.decl
  let reflect = ex_alias_unit_reflect

  let json_shape_explanation = ex_alias_unit_json_shape_explanation
  let to_json = ex_alias_unit_to_json
  let of_json' = ex_alias_unit_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  let sample_value01 : t Util.Sample_value.t = {
    orig = (); jv = `num 1.
  }

  let sample_values = [
    sample_value01
  ]
end

module Int_opt = struct
  type t = int option [@@deriving show]

  let decl = Td_ex_alias.Int_opt.decl
  let reflect = ex_alias_int_opt_reflect

  let json_shape_explanation = ex_alias_int_opt_json_shape_explanation
  let to_json = ex_alias_int_opt_to_json
  let of_json' = ex_alias_int_opt_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  let sample_value01 : t Util.Sample_value.t = {
    orig = None; jv = `null
  }

  let sample_value02 : t Util.Sample_value.t = {
    orig = Some 42; jv = `num 42.
  }

  let sample_values = [
    sample_value01;
    sample_value02;
  ]
end

module Objtuple = struct
  type t = float*string [@@deriving show]

  let decl = Td_ex_alias.Objtuple.decl
  let reflect = ex_alias_objtuple_reflect

  let json_shape_explanation = ex_alias_objtuple_json_shape_explanation
  let to_json = ex_alias_objtuple_to_json
  let of_json' = ex_alias_objtuple_of_json'
  let t : t Alcotest.testable = Alcotest.of_pp pp

  let sample_value01 : t Util.Sample_value.t = {
    orig = (12.3, "test"); jv = `obj [ ("_0", `num 12.3); ("_1", `str "test"); ]
  }

  let sample_values = [
    sample_value01
  ]
end

let env = empty_tdenv

let example_generated_descs : (module Util.Ex_generated_desc) list = [
  (module Unit);
  (module Int_opt);
  (module Objtuple);
]
