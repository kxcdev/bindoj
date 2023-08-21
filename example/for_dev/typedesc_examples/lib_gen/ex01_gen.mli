type student = { admission_year : int; name : string }

val student_reflect : student Bindoj_runtime.Refl.t
val student_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val student_to_json : student -> Kxclib.Json.jv
val student_of_json' : student Bindoj_runtime.json_full_decoder
val student_of_json : Kxclib.Json.jv -> student option
val student_decl : Bindoj_typedesc.Type_desc.type_decl

val student_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    student )
  Bindoj_runtime.generic_typed_type_decl
