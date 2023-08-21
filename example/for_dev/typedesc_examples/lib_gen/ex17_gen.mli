type nonrec int_opt = int option

val int_opt_reflect : int_opt Bindoj_runtime.Refl.t
val int_opt_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val int_opt_to_json : int_opt -> Kxclib.Json.jv
val int_opt_of_json' : int_opt Bindoj_runtime.json_full_decoder
val int_opt_of_json : Kxclib.Json.jv -> int_opt option
val int_opt_decl : Bindoj_typedesc.Type_desc.type_decl

val int_opt_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    int_opt )
  Bindoj_runtime.generic_typed_type_decl
