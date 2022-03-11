open Codegen

let string_of_jsonm_seq jsonm_seq =
  let buf = Buffer.create (Seq.fold_left (fun n _ -> n+1) 0 jsonm_seq) in
  let encoder = Jsonm.encoder (`Buffer buf) in
  Seq.iter (fun lxm -> let _ = Jsonm.encode encoder (`Lexeme lxm) in ()) jsonm_seq;
  let _ = Jsonm.encode encoder `End in
  String.of_bytes (Buffer.to_bytes buf)

let%test "ex01_encode" =
  let open Ex01_gen in
  let s = { admission_year = 1984; name = "William Gibson"; } in
  (string_of_jsonm_seq (encode_student_jsonm s))
  = {|{"admission_year":1984,"name":"William Gibson"}|}
