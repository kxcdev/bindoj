type nonrec ex_alias_unit = unit

val ex_alias_unit_reflect : ex_alias_unit Bindoj_runtime.Refl.t
val ex_alias_unit_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val ex_alias_unit_to_json : ex_alias_unit -> Kxclib.Json.jv
val ex_alias_unit_of_json' : ex_alias_unit Bindoj_runtime.json_full_decoder
val ex_alias_unit_of_json : Kxclib.Json.jv -> ex_alias_unit option
val ex_alias_unit_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_alias_unit_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_alias_unit )
  Bindoj_runtime.generic_typed_type_decl

type nonrec ex_alias_int_opt = int option

val ex_alias_int_opt_reflect : ex_alias_int_opt Bindoj_runtime.Refl.t

val ex_alias_int_opt_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_alias_int_opt_to_json : ex_alias_int_opt -> Kxclib.Json.jv

val ex_alias_int_opt_of_json' :
  ex_alias_int_opt Bindoj_runtime.json_full_decoder

val ex_alias_int_opt_of_json : Kxclib.Json.jv -> ex_alias_int_opt option
val ex_alias_int_opt_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_alias_int_opt_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_alias_int_opt )
  Bindoj_runtime.generic_typed_type_decl

type nonrec ex_alias_objtuple = float * string

val ex_alias_objtuple_reflect : ex_alias_objtuple Bindoj_runtime.Refl.t

val ex_alias_objtuple_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_alias_objtuple_to_json : ex_alias_objtuple -> Kxclib.Json.jv

val ex_alias_objtuple_of_json' :
  ex_alias_objtuple Bindoj_runtime.json_full_decoder

val ex_alias_objtuple_of_json : Kxclib.Json.jv -> ex_alias_objtuple option
val ex_alias_objtuple_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_alias_objtuple_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_alias_objtuple )
  Bindoj_runtime.generic_typed_type_decl
