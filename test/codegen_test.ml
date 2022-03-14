(*
open Codegen

let jsonms_of_json_string json_string =
  let decoder = Jsonm.decoder (`String json_string) in
  let rec loop () =
    match Jsonm.decode decoder with
    | `Await -> loop ()
    | `End -> []
    | `Lexeme lxm -> lxm :: (loop ())
    | `Error _ -> failwith "get invalid JSON string" in
  loop ()

let%test "ex01_encode" =
  let open Ex01_gen in
  let ocaml_record = { admission_year = 1984; name = "William Gibson"; } in
  let json_string = {|{"admission_year":1984,"name":"William Gibson"}|} in
  encode_student_jsonm ocaml_record |> List.of_seq = jsonms_of_json_string json_string
*)
