type nested_variant =
  | Student1 of { student : Ex01_gen.student }
  | Student2 of { student : Ex01_gen.student }
  | Student3 of Ex01_gen.student
  | Student4 of Ex01_gen.student
  | Int_list1 of Ex03_gen.int_list
  | Int_list2 of Ex03_gen.int_list

val nested_variant_reflect : nested_variant Bindoj_runtime.Refl.t
val nested_variant_json_discriminator_value : nested_variant -> string

val nested_variant_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val nested_variant_to_json : nested_variant -> Kxclib.Json.jv
val nested_variant_of_json' : nested_variant Bindoj_runtime.json_full_decoder
val nested_variant_of_json : Kxclib.Json.jv -> nested_variant option
val nested_variant_decl : Bindoj_typedesc.Type_desc.type_decl

val nested_variant_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    nested_variant )
  Bindoj_runtime.generic_typed_type_decl
