type xy_opt = { x_opt : int option; y_opt : int option }

val xy_opt_reflect : xy_opt Bindoj_runtime.Refl.t
val xy_opt_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val xy_opt_to_json : xy_opt -> Kxclib.Json.jv
val xy_opt_of_json' : xy_opt Bindoj_runtime.json_full_decoder
val xy_opt_of_json : Kxclib.Json.jv -> xy_opt option
val xy_opt_decl : Bindoj_typedesc.Type_desc.type_decl

val xy_opt_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    xy_opt )
  Bindoj_runtime.generic_typed_type_decl
