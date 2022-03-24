open Gen_lib.Datatype_desc

let ex02 : type_decl =
  { td_name = "person";
    td_kind =
      Variant_kind
        ([ Cstr_tuple { ct_name = "Anonymous"; ct_args = []; ct_codec = `default_codec }, `nodoc;
           Cstr_tuple { ct_name = "With_id"; ct_args = ["int"]; ct_codec = `default_codec }, `nodoc;
           Cstr_record { cr_name = "Student";
                         cr_fields =
                           [{ rf_name = "student_id"; rf_type = "int"; rf_codec = `default_codec; }, `nodoc;
                            { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec; }, `nodoc];
                         cr_codec = `default_codec; },
           `nodoc;
           Cstr_record { cr_name = "Teacher";
                         cr_fields =
                           [{ rf_name = "faculty_id"; rf_type = "int"; rf_codec = `default_codec; }, `nodoc;
                            { rf_name = "name"; rf_type = "string"; rf_codec = `default_codec; }, `nodoc;
                            { rf_name = "department"; rf_type = "string"; rf_codec = `default_codec; }, `nodoc ];
                         cr_codec = `default_codec; },
           `nodoc]),
      `nodoc; }

let () =
  let open Ppxlib in
  let open Ast_builder.Default in
  let loc = Location.none in
  Astlib.Pprintast.structure Format.std_formatter [
    (pstr_type ~loc Recursive [type_declaration_of_type_decl ex02]);
    (pstr_value ~loc Recursive
       (gen_json_encoder ex02 `default_codec :: gen_primitive_encoders `default_codec));
    (pstr_value ~loc Recursive
       (gen_json_decoder ex02 `default_codec :: gen_primitive_decoders `default_codec));
  ]
