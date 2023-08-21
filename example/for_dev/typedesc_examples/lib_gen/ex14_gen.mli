type nonrec objtuple = float * string

val objtuple_reflect : objtuple Bindoj_runtime.Refl.t
val objtuple_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val objtuple_to_json : objtuple -> Kxclib.Json.jv
val objtuple_of_json' : objtuple Bindoj_runtime.json_full_decoder
val objtuple_of_json : Kxclib.Json.jv -> objtuple option
val objtuple_decl : Bindoj_typedesc.Type_desc.type_decl

val objtuple_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    objtuple )
  Bindoj_runtime.generic_typed_type_decl
