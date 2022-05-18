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

include Bindoj_gen_test.Ex02_gen

type t = person =
  | Anonymous
  | With_id of int
  | Student of { student_id: int; name: string }
  | Teacher of { faculty_id: int; name: string; department: string }
  [@@deriving show, eq]

let encode_json = encode_person_json
let decode_json = decode_person_json
let t : t Alcotest.testable = Alcotest.testable pp equal

let sample_value01 = Anonymous

let sample_value02 = With_id 1619

let sample_value03 =
  Student {
    student_id = 451;
    name = "Ray Bradbury";
  }

let sample_value04 =
  Teacher {
    faculty_id = 2001;
    name = "Arthur C. Clark";
    department = "Space";
  }

let sample_values = [
  sample_value01;
  sample_value02;
  sample_value03;
  sample_value04;
]
