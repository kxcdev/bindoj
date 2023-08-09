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
include Bindoj_gen_test_gen_output.Ex02_gen
open Bindoj_base

type t = person =
  | Anonymous
  | With_id of int
  | Student of { student_id: int; name: string }
  | Teacher of { faculty_id: int; name: string; department: string }
  [@@deriving show]
let decl = Bindoj_test_common_typedesc_examples.Ex02.decl
let reflect = person_reflect

let json_discriminator_value = person_json_discriminator_value
let json_shape_explanation = person_json_shape_explanation
let to_json = person_to_json
let of_json' = person_of_json'
let env = empty_tdenv
let t : t Alcotest.testable = Alcotest.of_pp pp

open Sample_value
open Sample_value.JvHelper

let sample_value01 = { orig = Anonymous; jv = ctor0 "anonymous" }

let sample_value02 = {
  orig = With_id 1619;
  jv = ctor1 "with-id" (`num 1619.);
}

let sample_value03 = {
  orig =
    Student {
      student_id = 451;
      name = "Ray Bradbury";
    };
  jv =
    ctor_record "student" [
      ("studentId", `num 451.);
      ("name", `str "Ray Bradbury");
    ];
}

let sample_value04 = {
  orig =
    Teacher {
      faculty_id = 2001;
      name = "Arthur C. Clark";
      department = "Space";
    };
  jv =
    ctor_record "teacher" [
      ("facultyId", `num 2001.);
      ("name", `str "Arthur C. Clark");
      ("department", `str "Space");
    ];
}

let sample_values = [
  sample_value01;
  sample_value02;
  sample_value03;
  sample_value04;
]
