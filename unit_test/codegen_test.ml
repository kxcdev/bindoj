open Codegen
open Kxclib

let%test "ex01" =
  let open Ex01_gen in
  let ex01_student : student = {
    admission_year = 1984;
    name = "William Gibson";
  } in
  let jv_student : Json.jv =
    `obj [("admission_year", `num 1984.);
          ("name", `str "William Gibson")] in
  let yojson_student =
    `Assoc [("admission_year", `Int 1984);
            ("name", `String "William Gibson")] in
  let jsonm_student : Jsonm.lexeme list =
    [`Os;
     `Name "admission_year"; `Float 1984.0;
     `Name "name"; `String "William Gibson";
     `Oe] in
  (* ex01 encoding *)
  encode_student_json ex01_student = jv_student &&
  Json.to_yojson (encode_student_json ex01_student) = yojson_student &&
  Json.to_yojson (encode_student_json ex01_student) |> Json.yojson_basic_of_safe = yojson_student &&
  Json.to_jsonm (encode_student_json ex01_student) |> List.of_seq = jsonm_student &&
  (* ex01 decoding *)
  Some ex01_student = decode_student_json jv_student &&
  Some ex01_student = decode_student_json (Json.of_yojson yojson_student) &&
  Some ex01_student = Option.bind (Json.of_jsonm (List.to_seq jsonm_student)) decode_student_json

let%test "ex02_encoding" =
  let open Ex02_gen in
  (* anonymous *)
  let ex02_anonymous : person = Anonymous in
  let encoded_anonymous = encode_person_json ex02_anonymous in
  let jv_anonymous : Json.jv = `arr [`str "Anonymous"] in
  let yojson_anonymous = `List [`String "Anonymous"] in
  let jsonm_anonymous = [`As; `String "Anonymous"; `Ae] in
  (* with_id *)
  let ex02_with_id : person = With_id 1619 in
  let encoded_with_id = encode_person_json ex02_with_id in
  let jv_with_id : Json.jv = `arr [`str "With_id"; `num 1619.] in
  let yojson_with_id = `List [`String "With_id"; `Int 1619] in
  let jsonm_with_id = [`As; `String "With_id"; `Float 1619.; `Ae] in
  (* student *)
  let ex02_student : person =
    Student {
      student_id = 451;
      name = "Ray Bradbury";
    } in
  let encoded_student = encode_person_json ex02_student in
  let jv_student : Json.jv =
    `arr [`str "Student";
          `obj [("student_id", `num 451.);
                ("name", `str "Ray Bradbury");]] in
  let yojson_student =
    `List [`String "Student";
           `Assoc [("student_id", `Int 451);
                   ("name", `String "Ray Bradbury");]] in
  let jsonm_student =
    [`As; `String "Student";
     `Os;
     `Name "student_id"; `Float 451.;
     `Name "name"; `String "Ray Bradbury";
     `Oe;
     `Ae] in
  (* teacher *)
  let ex02_teacher : person =
    Teacher {
      faculty_id = 2001;
      name = "Arthur C. Clark";
      department = "Space";
    } in
  let encoded_teacher = encode_person_json ex02_teacher in
  let jv_teacher : Json.jv =
    `arr [`str "Teacher";
          `obj [("faculty_id", `num 2001.);
                ("name", `str "Arthur C. Clark");
                ("department", `str "Space")]] in
  let yojson_teacher =
    `List [`String "Teacher";
           `Assoc [("faculty_id", `Int 2001);
                   ("name", `String "Arthur C. Clark");
                   ("department", `String "Space")]] in
  let jsonm_teacher =
    [`As; `String "Teacher";
     `Os;
     `Name "faculty_id"; `Float 2001.;
     `Name "name"; `String "Arthur C. Clark";
     `Name "department"; `String "Space";
     `Oe;
     `Ae] in

  (* anonymous tests *)
  encoded_anonymous = jv_anonymous &&
  Json.to_yojson encoded_anonymous = yojson_anonymous &&
  Json.to_yojson encoded_anonymous |> Json.yojson_basic_of_safe = yojson_anonymous &&
  Json.to_jsonm encoded_anonymous |> List.of_seq = jsonm_anonymous &&
  (* with_id tests *)
  encoded_with_id = jv_with_id &&
  Json.to_yojson encoded_with_id = yojson_with_id &&
  Json.to_yojson encoded_with_id |> Json.yojson_basic_of_safe = yojson_with_id &&
  Json.to_jsonm encoded_with_id |> List.of_seq = jsonm_with_id &&
  (* student tests *)
  encoded_student = jv_student &&
  Json.to_yojson encoded_student = yojson_student &&
  Json.to_yojson encoded_student |> Json.yojson_basic_of_safe = yojson_student &&
  Json.to_jsonm encoded_student |> List.of_seq = jsonm_student &&
  (* teacher tests *)
  encoded_teacher = jv_teacher &&
  Json.to_yojson encoded_teacher = yojson_teacher &&
  Json.to_yojson encoded_teacher |> Json.yojson_basic_of_safe = yojson_teacher &&
  Json.to_jsonm encoded_teacher |> List.of_seq = jsonm_teacher
