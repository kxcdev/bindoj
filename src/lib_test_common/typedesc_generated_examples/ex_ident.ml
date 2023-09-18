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
open struct
  module Td_ex_ident = Bindoj_test_common_typedesc_examples.Ex_ident
end

include Bindoj_gen_test_gen_output.Ex_ident_gen

module Student_pair = struct
  type t = ex_ident_student_pair = {
    student1: Ex_record.Student.t;
    student2: Ex_record.Student.t
  } [@@deriving show]

  let decl = Td_ex_ident.Student_pair.decl
  let reflect = ex_ident_student_pair_reflect

  let json_shape_explanation = ex_ident_student_pair_json_shape_explanation
  let to_json = ex_ident_student_pair_to_json
  let of_json' = ex_ident_student_pair_of_json'

  let t : t Alcotest.testable = Alcotest.of_pp pp

  let sample_value01 : t Util.Sample_value.t =
    let s : Ex_record.Student.t Util.Sample_value.t = {
      orig = {
        admission_year = 1984;
        name = "William Gibson";
      };
      jv = `obj [
        ("admissionYear", `num 1984.);
        ("name", `str "William Gibson")
      ] }
    in
    { orig = {
        student1 = s.orig;
        student2 = s.orig
      };
      jv = `obj [
        ("student1", s.jv);
        ("student2", s.jv);
      ];
    }

  let sample_values = [
    sample_value01;
  ]
end

let env =
  let open Bindoj_typedesc.Typed_type_desc in
  { Type_decl_environment.empty with
    alias_ident_typemap =
      Util.Env.to_alias_ident_typemap [
        (module Ex_record.Student);
      ] }

let example_generated_descs : (module Util.Ex_generated_desc) list = [
  (module Student_pair)
]
