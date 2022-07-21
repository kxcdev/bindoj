(* Copyright 2022 Kotoi-Xie Consultancy, Inc.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

(* Acknowledgements - AnchorZ Inc.
The initial version or a significant portion of this file is developed
under the funding of AnchorZ Inc. to satisfy its needs in
product development. *)

[@@@warning "-33-32"]

open Bindoj_apidir_shared
open Bindoj_typedesc
open Bindoj_typedesc.Typed_type_desc
open Test_utils

module SampleApiDir = struct

  module Types = struct
    open Bindoj_test_common
    module ExD = Typedesc_examples
    module ExG = Typedesc_generated_examples

    type student = ExG.Ex01.student
    let student : student typed_type_decl = Typed.mk ExG.Ex01.decl ExG.Ex01.reflect

    type person = ExG.Ex02.person
    let person : person typed_type_decl = Typed.mk ExG.Ex02.decl ExG.Ex02.reflect
  end

  open struct
    module R = MakeRegistry()
    module T = Types
  end

  let get_any_student =
    R.register_get "get-any-student"
      ~urlpath:"/student/any-one"
      ~resp_type:T.student
      ~resp_name:"student"
      ~resp_doc:"a student record (could be anyone) in the database"

  let get_student_from_person =
    R.register_post "get-student-from-person"
      ~urlpath:"/student/from-person"
      ~req_type:T.person
      ~req_name:"person"
      ~req_doc:"a person record identifying a student"
      ~resp_type:T.student
      ~resp_name:"student"
      ~resp_doc:"the student record corresponding to the supplied person"

  include R.Public
end

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
