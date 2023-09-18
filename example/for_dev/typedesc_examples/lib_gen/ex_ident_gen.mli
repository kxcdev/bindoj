type ex_ident_student_pair = {
  student1 : Ex_record_gen.ex_record_student;
  student2 : Ex_record_gen.ex_record_student;
}

val ex_ident_student_pair_reflect : ex_ident_student_pair Bindoj_runtime.Refl.t

val ex_ident_student_pair_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_ident_student_pair_to_json : ex_ident_student_pair -> Kxclib.Json.jv

val ex_ident_student_pair_of_json' :
  ex_ident_student_pair Bindoj_runtime.json_full_decoder

val ex_ident_student_pair_of_json :
  Kxclib.Json.jv -> ex_ident_student_pair option

val ex_ident_student_pair_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_ident_student_pair_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_ident_student_pair )
  Bindoj_runtime.generic_typed_type_decl
