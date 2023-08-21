type optional_variant =
  | Tuple_like of int option
  | Tuple_like_alias of Ex17_gen.int_opt
  | Tuple_like_obj of int option * Ex17_gen.int_opt
  | Tuple_like_spreading of Ex10_gen.xy_opt
  | Inline_record of {
      int_opt : Ex17_gen.int_opt;
      x_opt : int option;
      y_opt : int option;
      objtuple : int option * int option;
    }
  | Inline_record_spreading of {
      int_opt : Ex17_gen.int_opt;
      xy_opt : Ex10_gen.xy_opt;
    }
  | Reused_inline_record of { x_opt : int option; y_opt : int option }

val optional_variant_reflect : optional_variant Bindoj_runtime.Refl.t
val optional_variant_json_discriminator_value : optional_variant -> string

val optional_variant_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val optional_variant_to_json : optional_variant -> Kxclib.Json.jv

val optional_variant_of_json' :
  optional_variant Bindoj_runtime.json_full_decoder

val optional_variant_of_json : Kxclib.Json.jv -> optional_variant option
val optional_variant_decl : Bindoj_typedesc.Type_desc.type_decl

val optional_variant_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    optional_variant )
  Bindoj_runtime.generic_typed_type_decl
