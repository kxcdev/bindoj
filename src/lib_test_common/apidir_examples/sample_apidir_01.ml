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
[@@@warning "-33-32"]

open Utils
open Bindoj_apidir_shared
open Bindoj_typedesc.Typed_type_desc
open Bindoj_test_common_typedesc_generated_examples

module Types = struct
  type student = Ex01.student
  let student : student typed_type_decl = Typed.mk Ex01.decl Ex01.reflect

  type person = Ex02.person
  let person : person typed_type_decl = Typed.mk Ex02.decl Ex02.reflect
end

module Functions = struct
  let student_of_person = function
    (** returns ``response'' from ``request'' *)
    | Ex02.Student { student_id; name; } -> Ex01.{ admission_year = student_id; name; }
    | Anonymous -> failwith "anonymous, not student"
    | With_id _ -> failwith "with_id, not student"
    | Teacher _ -> failwith "teacher, not student"
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

let () = begin
  get_any_student
  |> R.register_response_sample
    ~status:`default
    (Ex01.sample_value01.orig);

  get_student_from_person
  |> R.register_usage_sample
    ~status:`default
    ~req:(Ex02.sample_value03.orig)
    ~resp:(Functions.student_of_person Ex02.sample_value03.orig)
end

include R.Public

open Alcotest

let test_individual_invocation_points() = begin
    check_invp "get_any_student" get_any_student
      ~ip_name:"get-any-student"
      ~ip_urlpath:"/student/any-one"
      ~ip_method:`get;

    check_invp "get_student_from_person" get_student_from_person
      ~ip_name:"get-student-from-person"
      ~ip_urlpath:"/student/from-person"
      ~ip_method:`post
  end

let test_invocation_point_collection() = begin
    let invps, _ =  registry_info() in
    check (list string) "registry_info has all invp listed"
      (List.sort compare
         ["get-any-student";
          "get-student-from-person"])
      (List.sort compare (invps |&> (fun (Invp invp) -> invp.ip_name)))
  end

let tests =  [
  test_case "individual_invocation_points" `Quick
    test_individual_invocation_points;
  test_case "invocation_point_collection" `Quick
    test_invocation_point_collection;
]

let build_mock_server (module M: MockServerBuilder) =
  let open M.Io in
  let open Sample_value in

  let () (* get-any-student *) =
    let invp = get_any_student in
    let { orig; jv } = Ex01.sample_value01 in
    M.register_get_handler invp (fun () -> return (200, orig));
    M.register_get_example invp.ip_urlpath (Invp invp) ~orig ~jv ~pp:Ex01.pp;
    M.register_get_example "/student/any-one" (Invp invp) ~orig ~jv ~pp:Ex01.pp;
    M.register_get_example "/student/any-one/" (Invp invp) ~orig ~jv ~pp:Ex01.pp;
    M.register_get_example "/student/any-one///" (Invp invp) ~orig ~jv ~pp:Ex01.pp;
    M.register_get_example "/student//any-one/" (Invp invp) ~orig ~jv ~pp:Ex01.pp;
    M.register_get_example "/student///any-one/" (Invp invp) ~orig ~jv ~pp:Ex01.pp;
    M.register_get_example "/student///any-one//" (Invp invp) ~orig ~jv ~pp:Ex01.pp;
  in

  let () (* get-student-from-person *) =
    let invp = get_student_from_person in
    let open Functions in

    let reg_sample { orig; jv } =
      M.register_post_example invp.ip_urlpath (Invp invp)
        ~orig_resp:(student_of_person orig) ~orig_req:orig
        ~jv_resp:(student_of_person orig |> Ex01.student_to_json) ~jv_req:jv
        ~pp:Ex01.pp in

    reg_sample Ex02.sample_value03;
    let sample =
      let orig =
        Ex02.(Student {
            student_id = 1984;
            name = "William Gibson";
          }) in
      let jv =
        JvHelper.ctor_record
          "student" [
          ("studentId", `num 1984.);
          ("name", `str "William Gibson");
        ] in
      { orig; jv } in
    reg_sample sample;
    M.register_post_handler invp (fun x -> return (200, student_of_person x))
  in ()
