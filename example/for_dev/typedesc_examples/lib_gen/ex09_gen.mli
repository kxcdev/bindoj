type with_int53p = { value : Kxclib.int53p }

val with_int53p_reflect : with_int53p Bindoj_runtime.Refl.t
val with_int53p_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val with_int53p_to_json : with_int53p -> Kxclib.Json.jv
val with_int53p_of_json' : with_int53p Bindoj_runtime.json_full_decoder
val with_int53p_of_json : Kxclib.Json.jv -> with_int53p option
val with_int53p_decl : Bindoj_typedesc.Type_desc.type_decl

val with_int53p_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    with_int53p )
  Bindoj_runtime.generic_typed_type_decl
