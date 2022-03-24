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
  (* encoding *)
  encode_student_json ex01_student = jv_student &&
  Json.to_yojson (encode_student_json ex01_student) = yojson_student &&
  Json.to_yojson (encode_student_json ex01_student) |> Json.yojson_basic_of_safe = yojson_student &&
  Json.to_jsonm (encode_student_json ex01_student) |> List.of_seq = jsonm_student &&
  (* decoding *)
  Some ex01_student = decode_student_json jv_student &&
  Some ex01_student = decode_student_json (Json.of_yojson yojson_student) &&
  Some ex01_student = Option.bind (Json.of_jsonm (List.to_seq jsonm_student)) decode_student_json

let%test "ex02_anonymous" =
  let open Ex02_gen in
  let ex02_anonymous : person = Anonymous in
  let jv_anonymous : Json.jv = `arr [`str "Anonymous"] in
  let yojson_anonymous = `List [`String "Anonymous"] in
  let jsonm_anonymous = [`As; `String "Anonymous"; `Ae] in
  (* encoding *)
  encode_person_json ex02_anonymous = jv_anonymous &&
  Json.to_yojson (encode_person_json ex02_anonymous) = yojson_anonymous &&
  Json.to_yojson (encode_person_json ex02_anonymous) |> Json.yojson_basic_of_safe = yojson_anonymous &&
  Json.to_jsonm (encode_person_json ex02_anonymous) |> List.of_seq = jsonm_anonymous &&
  (* decoding *)
  Some ex02_anonymous = decode_person_json jv_anonymous &&
  Some ex02_anonymous = decode_person_json (Json.of_yojson yojson_anonymous) &&
  Some ex02_anonymous = Option.bind (Json.of_jsonm (List.to_seq jsonm_anonymous)) decode_person_json

let%test "ex02_with_id" =
  let open Ex02_gen in
  let ex02_with_id : person = With_id 1619 in
  let jv_with_id : Json.jv = `arr [`str "With_id"; `num 1619.] in
  let yojson_with_id = `List [`String "With_id"; `Int 1619] in
  let jsonm_with_id = [`As; `String "With_id"; `Float 1619.; `Ae] in
  (* encoding *)
  encode_person_json ex02_with_id = jv_with_id &&
  Json.to_yojson (encode_person_json ex02_with_id) = yojson_with_id &&
  Json.to_yojson (encode_person_json ex02_with_id) |> Json.yojson_basic_of_safe = yojson_with_id &&
  Json.to_jsonm (encode_person_json ex02_with_id) |> List.of_seq = jsonm_with_id &&
  (* decoding *)
  Some ex02_with_id = decode_person_json jv_with_id &&
  Some ex02_with_id = decode_person_json (Json.of_yojson yojson_with_id) &&
  Some ex02_with_id = Option.bind (Json.of_jsonm (List.to_seq jsonm_with_id)) decode_person_json

let%test "ex02_student" =
  let open Ex02_gen in
  let ex02_student : person =
    Student {
      student_id = 451;
      name = "Ray Bradbury";
    } in
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
  (* encoding *)
  encode_person_json ex02_student = jv_student &&
  Json.to_yojson (encode_person_json ex02_student) = yojson_student &&
  Json.to_yojson (encode_person_json ex02_student) |> Json.yojson_basic_of_safe = yojson_student &&
  Json.to_jsonm (encode_person_json ex02_student) |> List.of_seq = jsonm_student &&
  (* decoding *)
  Some ex02_student = decode_person_json jv_student &&
  Some ex02_student = decode_person_json (Json.of_yojson yojson_student) &&
  Some ex02_student = Option.bind (Json.of_jsonm (List.to_seq jsonm_student)) decode_person_json

let%test "ex02_teacher" =
  let open Ex02_gen in
  let ex02_teacher : person =
    Teacher {
      faculty_id = 2001;
      name = "Arthur C. Clark";
      department = "Space";
    } in
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
  (* encoding *)
  encode_person_json ex02_teacher = jv_teacher &&
  Json.to_yojson (encode_person_json ex02_teacher) = yojson_teacher &&
  Json.to_yojson (encode_person_json ex02_teacher) |> Json.yojson_basic_of_safe = yojson_teacher &&
  Json.to_jsonm (encode_person_json ex02_teacher) |> List.of_seq = jsonm_teacher &&
  (* decoding *)
  Some ex02_teacher = decode_person_json jv_teacher &&
  Some ex02_teacher = decode_person_json (Json.of_yojson yojson_teacher) &&
  Some ex02_teacher = Option.bind (Json.of_jsonm (List.to_seq jsonm_teacher)) decode_person_json
