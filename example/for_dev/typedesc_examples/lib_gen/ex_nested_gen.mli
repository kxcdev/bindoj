type ex_nested_point2 = { x : float; y : float }

val ex_nested_point2_reflect : ex_nested_point2 Bindoj_runtime.Refl.t

val ex_nested_point2_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_nested_point2_to_json : ex_nested_point2 -> Kxclib.Json.jv

val ex_nested_point2_of_json' :
  ex_nested_point2 Bindoj_runtime.json_full_decoder

val ex_nested_point2_of_json : Kxclib.Json.jv -> ex_nested_point2 option
val ex_nested_point2_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_nested_point2_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_nested_point2 )
  Bindoj_runtime.generic_typed_type_decl

type ex_nested_record = {
  unit : Ex_alias_gen.ex_alias_unit;
  point2 : ex_nested_point2;
  point2_spread : ex_nested_point2;
  person : Ex_mangling_gen.ex_mangling_person_inherited;
  person_spread : Ex_mangling_gen.ex_mangling_person_inherited;
}

val ex_nested_record_reflect : ex_nested_record Bindoj_runtime.Refl.t

val ex_nested_record_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_nested_record_to_json : ex_nested_record -> Kxclib.Json.jv

val ex_nested_record_of_json' :
  ex_nested_record Bindoj_runtime.json_full_decoder

val ex_nested_record_of_json : Kxclib.Json.jv -> ex_nested_record option
val ex_nested_record_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_nested_record_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_nested_record )
  Bindoj_runtime.generic_typed_type_decl

type ex_nested_variant =
  | Student1 of { student : Ex_record_gen.ex_record_student }
  | Student2 of { student : Ex_record_gen.ex_record_student }
  | Student3 of Ex_record_gen.ex_record_student
  | Student4 of Ex_record_gen.ex_record_student
  | Int_list1 of Ex_variant_gen.ex_variant_int_list
  | Int_list2 of Ex_variant_gen.ex_variant_int_list

val ex_nested_variant_reflect : ex_nested_variant Bindoj_runtime.Refl.t
val ex_nested_variant_json_discriminator_value : ex_nested_variant -> string

val ex_nested_variant_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_nested_variant_to_json : ex_nested_variant -> Kxclib.Json.jv

val ex_nested_variant_of_json' :
  ex_nested_variant Bindoj_runtime.json_full_decoder

val ex_nested_variant_of_json : Kxclib.Json.jv -> ex_nested_variant option
val ex_nested_variant_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_nested_variant_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_nested_variant )
  Bindoj_runtime.generic_typed_type_decl
