type int_list = IntNil | IntCons of int * int_list

val int_list_reflect : int_list Bindoj_runtime.Refl.t
val int_list_json_discriminator_value : int_list -> string
val int_list_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val int_list_to_json : int_list -> Kxclib.Json.jv
val int_list_of_json' : int_list Bindoj_runtime.json_full_decoder
val int_list_of_json : Kxclib.Json.jv -> int_list option
val int_list_decl : Bindoj_typedesc.Type_desc.type_decl

val int_list_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    int_list )
  Bindoj_runtime.generic_typed_type_decl
