open Gen_lib.Datatype_desc

let ex01 : type_decl =
  { td_name = "student";
    td_kind =
      Record_kind
        ([{ rf_name = "admission_year"; rf_type = "int"; }, `nodoc;
          { rf_name = "name"; rf_type = "string"; }, `nodoc;]),
      `nodoc; }

let () =
  let open Ppxlib in
  let open Ast_builder.Default in
  let loc = Location.none in
  Astlib.Pprintast.structure Format.std_formatter [
    (pstr_type ~loc Recursive [type_declaration_of_type_decl ex01]);
    [%stri let encode_student_jsonm = [%e gen_jsonm_encoder ex01]];
  ]
