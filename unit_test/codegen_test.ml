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
  let yojson_student : Yojson.Safe.t =
    `Assoc [("admission_year", `Int 1984);
            ("name", `String "William Gibson")] in
  encoded_student = jv_student
  && (Json.to_yojson encoded_student :> Yojson.Safe.t) = yojson_student
