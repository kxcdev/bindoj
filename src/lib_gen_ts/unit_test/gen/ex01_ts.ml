let ex01 : type_decl =
  { td_name = "student";
    td_kind =
      Record_kind
        ([{ rf_name = "admission_year"; rf_type = "int"; rf_codec = `default_codec }, `nodoc;
          { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec }, `nodoc;]),
      `nodoc; }

let () =
  print_endline (gen_ts_type ex01)
