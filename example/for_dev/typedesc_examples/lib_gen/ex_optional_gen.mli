type ex_optional_xy_opt = { x_opt : int option; y_opt : int option }

val ex_optional_xy_opt_reflect : ex_optional_xy_opt Bindoj_runtime.Refl.t

val ex_optional_xy_opt_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_optional_xy_opt_to_json : ex_optional_xy_opt -> Kxclib.Json.jv

val ex_optional_xy_opt_of_json' :
  ex_optional_xy_opt Bindoj_runtime.json_full_decoder

val ex_optional_xy_opt_of_json : Kxclib.Json.jv -> ex_optional_xy_opt option
val ex_optional_xy_opt_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_optional_xy_opt_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_optional_xy_opt )
  Bindoj_runtime.generic_typed_type_decl

type ex_optional_variant =
  | Tuple_like of int option
  | Tuple_like_alias of Ex_alias_gen.ex_alias_int_opt
  | Tuple_like_obj of int option * Ex_alias_gen.ex_alias_int_opt
  | Tuple_like_spreading of ex_optional_xy_opt
  | Inline_record of {
      int_opt : Ex_alias_gen.ex_alias_int_opt;
      x_opt : int option;
      y_opt : int option;
      objtuple : int option * int option;
    }
  | Inline_record_spreading of {
      int_opt : Ex_alias_gen.ex_alias_int_opt;
      xy_opt : ex_optional_xy_opt;
    }
  | Reused_inline_record of { x_opt : int option; y_opt : int option }

val ex_optional_variant_reflect : ex_optional_variant Bindoj_runtime.Refl.t
val ex_optional_variant_json_discriminator_value : ex_optional_variant -> string

val ex_optional_variant_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_optional_variant_to_json : ex_optional_variant -> Kxclib.Json.jv

val ex_optional_variant_of_json' :
  ex_optional_variant Bindoj_runtime.json_full_decoder

val ex_optional_variant_of_json : Kxclib.Json.jv -> ex_optional_variant option
val ex_optional_variant_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_optional_variant_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_optional_variant )
  Bindoj_runtime.generic_typed_type_decl
