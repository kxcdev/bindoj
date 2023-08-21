type customized_union = Case1 of int | Case2 of { x : int; y : int }

val customized_union_reflect : customized_union Bindoj_runtime.Refl.t
val customized_union_json_discriminator_value : customized_union -> string

val customized_union_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val customized_union_to_json : customized_union -> Kxclib.Json.jv

val customized_union_of_json' :
  customized_union Bindoj_runtime.json_full_decoder

val customized_union_of_json : Kxclib.Json.jv -> customized_union option
val customized_union_decl : Bindoj_typedesc.Type_desc.type_decl

val customized_union_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    customized_union )
  Bindoj_runtime.generic_typed_type_decl
