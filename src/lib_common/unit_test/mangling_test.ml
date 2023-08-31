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
open Alcotest
open Bindoj_common

let cases ~label mangler samples =
  test_case label `Quick (fun () ->
      samples |> List.iteri (fun idx (pre, after) ->
                     check string (sprintf "%s_%d" label idx)
                       after (mangler pre)))

let () =
  Printexc.record_backtrace true;
  run __FILE__ ([
    "version_mangling", [
      cases ~label:"snake_to_upper_camel" (Mangling.snake_to_upper_camel) [
          "student_info", "StudentInfo";
          "student_info_v1", "StudentInfoV1";
          "student_info_v1_0", "StudentInfoV1_0";
          "student_v1_0_info", "StudentV1_0Info";
          "student_v1_info", "StudentV1Info";
          "v1_0_student_info", "V1_0StudentInfo";
          "v1_student_info", "V1StudentInfo";
        ];
      cases ~label:"snake_to_lower_camel" (Mangling.snake_to_lower_camel) [
          "student_info", "studentInfo";
          "student_info_v1", "studentInfoV1";
          "student_info_v1_0", "studentInfoV1_0";
          "student_v1_0_info", "studentV1_0Info";
          "student_v1_info", "studentV1Info";
          "v1_0_student_info", "v1_0StudentInfo";
          "v1_student_info", "v1StudentInfo";
        ];
      cases ~label:"snake_to_kebab" (Mangling.snake_to_kebab) [
          "student_info", "student-info";
          "student_info_v1", "student-info-v1";
          "student_info_v1_0", "student-info-v1_0";
          "student_v1_0_info", "student-v1_0-info";
          "student_v1_0_1_info", "student-v1_0_1-info";
          "student_v1_info", "student-v1-info";
          "v1_0_student_info", "v1_0-student-info";
          "v1_student_info", "v1-student-info";
        ];
      cases ~label:"cap_snake_to_kebab" (Mangling.cap_snake_to_kebab) [
          "Student_info", "student-info";
          "Student_info_v1", "student-info-v1";
          "Student_info_v1_0", "student-info-v1_0";
          "Student_v1_0_info", "student-v1_0-info";
          "Student_v1_0_1_info", "student-v1_0_1-info";
          "Student_v1_info", "student-v1-info";
          "V1_0_student_info", "v1_0-student-info";
          "V1_student_info", "v1-student-info";
        ];
    ];

    "no_version_mangling",
    let preserve_version_substring = false in
    [ cases ~label:"snake_to_upper_camel" (Mangling.snake_to_upper_camel ~preserve_version_substring) [
          "student_info", "StudentInfo";
          "student_info_v1", "StudentInfoV1";
          "student_info_v1_0", "StudentInfoV10";
          "student_v1_0_info", "StudentV10Info";
          "student_v1_info", "StudentV1Info";
          "v1_0_student_info", "V10StudentInfo";
          "v1_student_info", "V1StudentInfo";
        ];
      cases ~label:"snake_to_lower_camel" (Mangling.snake_to_lower_camel ~preserve_version_substring) [
          "student_info", "studentInfo";
          "student_info_v1", "studentInfoV1";
          "student_info_v1_0", "studentInfoV10";
          "student_v1_0_info", "studentV10Info";
          "student_v1_info", "studentV1Info";
          "v1_0_student_info", "v10StudentInfo";
          "v1_student_info", "v1StudentInfo";
        ];
      cases ~label:"snake_to_kebab" (Mangling.snake_to_kebab ~preserve_version_substring) [
          "student_info", "student-info";
          "student_info_v1", "student-info-v1";
          "student_info_v1_0", "student-info-v1-0";
          "student_v1_0_info", "student-v1-0-info";
          "student_v1_0_1_info", "student-v1-0-1-info";
          "student_v1_info", "student-v1-info";
          "v1_0_student_info", "v1-0-student-info";
          "v1_student_info", "v1-student-info";
        ];
      cases ~label:"cap_snake_to_kebab" (Mangling.cap_snake_to_kebab ~preserve_version_substring) [
          "Student_info", "student-info";
          "Student_info_v1", "student-info-v1";
          "Student_info_v1_0", "student-info-v1-0";
          "Student_v1_0_info", "student-v1-0-info";
          "Student_v1_0_1_info", "student-v1-0-1-info";
          "Student_v1_info", "student-v1-info";
          "V1_0_student_info", "v1-0-student-info";
          "V1_student_info", "v1-student-info";
        ];
    ];
  ])
