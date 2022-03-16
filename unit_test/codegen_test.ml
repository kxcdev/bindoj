open Codegen
open Kxclib

let%test "ex01_encoding" =
  let open Ex01_gen in
  let ex01_student : student = {
    admission_year = 1984;
    name = "William Gibson";
  } in
  let encoded_student = encode_student_json ex01_student in
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
  encoded_student = jv_student
  && Json.to_yojson encoded_student = yojson_student
  && Json.to_yojson encoded_student |> Json.yojson_basic_of_safe = yojson_student
  && Json.to_jsonm encoded_student |> List.of_seq = jsonm_student
