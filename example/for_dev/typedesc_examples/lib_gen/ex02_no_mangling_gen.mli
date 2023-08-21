type person =
  | Anonymous
  | With_id of int
  | Student of { student_id : int; name : string }
  | Teacher of { faculty_id : int; name : string; department : string }

val person_reflect : person Bindoj_runtime.Refl.t
val person_json_discriminator_value : person -> string
val person_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val person_to_json : person -> Kxclib.Json.jv
val person_of_json' : person Bindoj_runtime.json_full_decoder
val person_of_json : Kxclib.Json.jv -> person option
val person_decl : Bindoj_typedesc.Type_desc.type_decl

val person_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    person )
  Bindoj_runtime.generic_typed_type_decl
