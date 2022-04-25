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

open Alcotest
open Kxclib

module Testables : sig
  val jv : Json.jv testable
  val yojson : Json.yojson testable
  val jsonm : Json.jsonm testable
end = struct
  type jv = [
    | `null
    | `bool of bool
    | `num of float
    | `str of string
    | `arr of jv list
    | `obj of (string * jv) list
  ] [@@ deriving show]
  let jv = testable pp_jv ( = )
  let yojson = testable Yojson.Safe.pp ( = )
  let jsonm =
    testable
      (fun ppf jsonm ->
         List.pp Jsonm.pp_lexeme ppf (List.of_seq jsonm))
      (fun x y -> List.of_seq x = List.of_seq y)
end

type 'a test_fields = {
  orig : 'a;
  jv : Json.jv;
  yojson : Json.yojson;
  jsonm : Json.jsonm;
}

let ex01_test student =
  let open Ex01_gen in
  let testable_student : student testable =
    testable pp_student ( = ) in
  (* encoding *)
  check Testables.jv "ex01 encoding test"
    student.jv
    (encode_student_json student.orig);
  check Testables.yojson "ex01 encoding test (+ yojson)"
    student.yojson
    (Json.to_yojson (encode_student_json student.orig));
  check Testables.jsonm "ex01 encoding test (+ jsonm)"
    student.jsonm
    (Json.to_jsonm (encode_student_json student.orig));
  (* decoding *)
  check (option testable_student) "ex01 decoding test"
    (Some student.orig)
    (decode_student_json student.jv);
  check (option testable_student) "ex01 decoding test (+ yojson)"
    (Some student.orig)
    (decode_student_json (Json.of_yojson student.yojson));
  check (option testable_student) "ex01 decoding test (+ jsonm)"
    (Some student.orig)
    (Option.bind (Json.of_jsonm student.jsonm) (fun (jv, _) ->
         decode_student_json jv));
  (* encoding & decoding *)
  check (option testable_student) "ex01 encoding and decoding test"
    (Some student.orig)
    (decode_student_json (encode_student_json student.orig));
  check (option Testables.jv) "ex01 decoding and encoding test"
    (Some student.jv)
    (Option.map encode_student_json (decode_student_json student.jv))

let ex02_test person =
  let open Ex02_gen in
  let testable_person : person testable =
    testable pp_person ( = ) in
  (* encoding *)
  check Testables.jv "ex02 anonymous encoding test"
    person.jv
    (encode_person_json person.orig);
  check Testables.yojson "ex02 anonymous encoding test (+ yojson)"
    person.yojson
    (Json.to_yojson (encode_person_json person.orig));
  check Testables.jsonm "ex02 anonymous encoding test (+ jsonm)"
    person.jsonm
    (Json.to_jsonm (encode_person_json person.orig));
  (* decoding *)
  check (option testable_person) "ex02 anonymous decoding test"
    (Some person.orig)
    (decode_person_json person.jv);
  check (option testable_person) "ex02 anonymous decoding test (+ yojson)"
    (Some person.orig)
    (decode_person_json (Json.of_yojson person.yojson));
  check (option testable_person) "ex02 anonymous decoding test (+ jsonm)"
    (Some person.orig)
    (Option.bind (Json.of_jsonm person.jsonm) (fun (jv, _) ->
         decode_person_json jv));
  (* encoding & decoding *)
  check (option testable_person) "ex02 anonymous encoding and decoding test"
    (Some person.orig)
    (decode_person_json (encode_person_json person.orig));
  check (option Testables.jv) "ex02 anonymous decoding and encoding test"
    (Some person.jv)
    (Option.map encode_person_json (decode_person_json person.jv))


let () =
  let open Ex01_gen in
  let open Ex02_gen in
  let ex01_student : student test_fields = {
    orig   = { admission_year = 1984;
               name = "William Gibson"; };
    jv     = `obj [("admission_year", `num 1984.);
                   ("name", `str "William Gibson")];
    yojson = `Assoc [("admission_year", `Int 1984);
                     ("name", `String "William Gibson")];
    jsonm  = [`Os;
              `Name "admission_year"; `Float 1984.0;
              `Name "name"; `String "William Gibson";
              `Oe] |> List.to_seq;
  } in
  let ex02_anonymous : person test_fields = {
    orig   = Anonymous;
    jv     = `obj [("kind", `str "Anonymous")];
    yojson = `Assoc [("kind", `String "Anonymous")];
    jsonm  = [`Os; `Name "kind"; `String "Anonymous"; `Oe] |> List.to_seq;
  } in
  let ex02_student : person test_fields = {
    orig   = Student { student_id = 451;
                       name = "Ray Bradbury"; };
    jv     = `obj [("kind", `str "Student");
                   ("student_id", `num 451.);
                   ("name", `str "Ray Bradbury");];
    yojson = `Assoc [("kind", `String "Student");
                     ("student_id", `Int 451);
                     ("name", `String "Ray Bradbury");];
    jsonm  = [`Os;
              `Name "kind"; `String "Student";
              `Name "student_id"; `Float 451.;
              `Name "name"; `String "Ray Bradbury";
              `Oe] |> List.to_seq;
  } in
  let ex02_teacher : person test_fields = {
    orig   = Teacher { faculty_id = 2001;
                       name = "Arthur C. Clark";
                       department = "Space"; };
    jv     = `obj [("kind", `str "Teacher");
                   ("faculty_id", `num 2001.);
                   ("name", `str "Arthur C. Clark");
                   ("department", `str "Space")];
    yojson = `Assoc [("kind", `String "Teacher");
                     ("faculty_id", `Int 2001);
                     ("name", `String "Arthur C. Clark");
                     ("department", `String "Space")];
    jsonm  = [`Os;
              `Name "kind"; `String "Teacher";
              `Name "faculty_id"; `Float 2001.;
              `Name "name"; `String "Arthur C. Clark";
              `Name "department"; `String "Space";
              `Oe] |> List.to_seq;
  } in
  ex01_test ex01_student;
  ex02_test ex02_anonymous;
  ex02_test ex02_student;
  ex02_test ex02_teacher
