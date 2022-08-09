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
open Bindoj_apidir_shared
open Bindoj_test_common.Apidir_examples.Utils
module SampleApiDir = Bindoj_test_common.Apidir_examples.Sample_apidir_01

open Alcotest

let test_individual_invocation_points() = begin
    let module Dir = SampleApiDir in

    (* get_any_student *)
    check string "get_any_student.ip_name"
      "get-any-student"
      Dir.get_any_student.ip_name;
    check string "get_any_student.ip_urlpath"
      "/student/any-one"
      Dir.get_any_student.ip_urlpath;
    check http_method "get_any_student.ip_method"
      `get
      Dir.get_any_student.ip_method;

    (* get_student_from_person *)
    check string "get_any_student.ip_name"
      "get-student-from-person"
      Dir.get_student_from_person.ip_name;
    check string "get_student_from_person.ip_urlpath"
      "/student/from-person"
      Dir.get_student_from_person.ip_urlpath;
    check http_method "get_student_from_person.ip_method"
      `post
      Dir.get_student_from_person.ip_method;
  end

let test_invocation_point_collection() = begin
    let module Dir = SampleApiDir in
    let invps, _ =  Dir.registry_info() in
    check (list string) "registry_info has all invp listed"
      (List.sort compare
         ["get-any-student";
          "get-student-from-person"])
      (List.sort compare (invps |&> (fun (Invp invp) -> invp.ip_name)))
  end

let () =
  Printexc.record_backtrace true;
  run (__FILE__) [
      "invocation_points", [
        test_case "individual_invocation_points" `Quick
          test_individual_invocation_points;
        test_case "" `Quick
          test_invocation_point_collection;
      ]
    ]
