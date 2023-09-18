type ex_record_student = { admission_year : int; name : string }

val ex_record_student_reflect : ex_record_student Bindoj_runtime.Refl.t

val ex_record_student_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_record_student_to_json : ex_record_student -> Kxclib.Json.jv

val ex_record_student_of_json' :
  ex_record_student Bindoj_runtime.json_full_decoder

val ex_record_student_of_json : Kxclib.Json.jv -> ex_record_student option
val ex_record_student_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_record_student_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_record_student )
  Bindoj_runtime.generic_typed_type_decl

type ex_record_teacher = {
  faculty_id : int;
  name : string;
  department : string;
}

val ex_record_teacher_reflect : ex_record_teacher Bindoj_runtime.Refl.t

val ex_record_teacher_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_record_teacher_to_json : ex_record_teacher -> Kxclib.Json.jv

val ex_record_teacher_of_json' :
  ex_record_teacher Bindoj_runtime.json_full_decoder

val ex_record_teacher_of_json : Kxclib.Json.jv -> ex_record_teacher option
val ex_record_teacher_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_record_teacher_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_record_teacher )
  Bindoj_runtime.generic_typed_type_decl
