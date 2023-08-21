type student_pair = { student1 : Ex01_gen.student; student2 : Ex01_gen.student }

val student_pair_reflect : student_pair Bindoj_runtime.Refl.t
val student_pair_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val student_pair_to_json : student_pair -> Kxclib.Json.jv
val student_pair_of_json' : student_pair Bindoj_runtime.json_full_decoder
val student_pair_of_json : Kxclib.Json.jv -> student_pair option
val student_pair_decl : Bindoj_typedesc.Type_desc.type_decl

val student_pair_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    student_pair )
  Bindoj_runtime.generic_typed_type_decl
