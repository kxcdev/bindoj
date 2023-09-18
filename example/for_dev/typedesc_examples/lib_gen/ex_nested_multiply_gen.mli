type ex_nested_multiply_record = {
  nested_record : Ex_nested_gen.ex_nested_record;
  nested_record_spread : Ex_nested_gen.ex_nested_record;
}

val ex_nested_multiply_record_reflect :
  ex_nested_multiply_record Bindoj_runtime.Refl.t

val ex_nested_multiply_record_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_nested_multiply_record_to_json :
  ex_nested_multiply_record -> Kxclib.Json.jv

val ex_nested_multiply_record_of_json' :
  ex_nested_multiply_record Bindoj_runtime.json_full_decoder

val ex_nested_multiply_record_of_json :
  Kxclib.Json.jv -> ex_nested_multiply_record option

val ex_nested_multiply_record_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_nested_multiply_record_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_nested_multiply_record )
  Bindoj_runtime.generic_typed_type_decl

type ex_nested_multiply_variant =
  | Nested_record of {
      nested_record : Ex_nested_gen.ex_nested_record;
      nested_record_spread : Ex_nested_gen.ex_nested_record;
    }
  | Nested_variant of {
      nested_variant : Ex_nested_gen.ex_nested_variant;
      nested_variant_spread : Ex_nested_gen.ex_nested_variant;
    }

val ex_nested_multiply_variant_reflect :
  ex_nested_multiply_variant Bindoj_runtime.Refl.t

val ex_nested_multiply_variant_json_discriminator_value :
  ex_nested_multiply_variant -> string

val ex_nested_multiply_variant_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_nested_multiply_variant_to_json :
  ex_nested_multiply_variant -> Kxclib.Json.jv

val ex_nested_multiply_variant_of_json' :
  ex_nested_multiply_variant Bindoj_runtime.json_full_decoder

val ex_nested_multiply_variant_of_json :
  Kxclib.Json.jv -> ex_nested_multiply_variant option

val ex_nested_multiply_variant_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_nested_multiply_variant_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_nested_multiply_variant )
  Bindoj_runtime.generic_typed_type_decl
