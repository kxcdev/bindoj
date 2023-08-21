type named_json = { name : string; json : Bindoj_std_runtime.Json_value.t }

val named_json_reflect : named_json Bindoj_runtime.Refl.t
val named_json_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val named_json_to_json : named_json -> Kxclib.Json.jv
val named_json_of_json' : named_json Bindoj_runtime.json_full_decoder
val named_json_of_json : Kxclib.Json.jv -> named_json option
val named_json_decl : Bindoj_typedesc.Type_desc.type_decl

val named_json_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    named_json )
  Bindoj_runtime.generic_typed_type_decl
