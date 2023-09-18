type ex_coretype_various_prim_types = {
  unit : unit;
  bool : bool;
  int : int;
  float : float;
  string : string;
  uchar : Uchar.t;
  byte : char;
  bytes : Bytes.t;
}

val ex_coretype_various_prim_types_reflect :
  ex_coretype_various_prim_types Bindoj_runtime.Refl.t

val ex_coretype_various_prim_types_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_coretype_various_prim_types_to_json :
  ex_coretype_various_prim_types -> Kxclib.Json.jv

val ex_coretype_various_prim_types_of_json' :
  ex_coretype_various_prim_types Bindoj_runtime.json_full_decoder

val ex_coretype_various_prim_types_of_json :
  Kxclib.Json.jv -> ex_coretype_various_prim_types option

val ex_coretype_various_prim_types_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_coretype_various_prim_types_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_coretype_various_prim_types )
  Bindoj_runtime.generic_typed_type_decl

type ex_coretype_with_int53p = { value : Kxclib.int53p }

val ex_coretype_with_int53p_reflect :
  ex_coretype_with_int53p Bindoj_runtime.Refl.t

val ex_coretype_with_int53p_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_coretype_with_int53p_to_json : ex_coretype_with_int53p -> Kxclib.Json.jv

val ex_coretype_with_int53p_of_json' :
  ex_coretype_with_int53p Bindoj_runtime.json_full_decoder

val ex_coretype_with_int53p_of_json :
  Kxclib.Json.jv -> ex_coretype_with_int53p option

val ex_coretype_with_int53p_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_coretype_with_int53p_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_coretype_with_int53p )
  Bindoj_runtime.generic_typed_type_decl

type ex_coretype_various_complex_types = {
  option : int option;
  list : int list;
  map : (string * int) list;
}

val ex_coretype_various_complex_types_reflect :
  ex_coretype_various_complex_types Bindoj_runtime.Refl.t

val ex_coretype_various_complex_types_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_coretype_various_complex_types_to_json :
  ex_coretype_various_complex_types -> Kxclib.Json.jv

val ex_coretype_various_complex_types_of_json' :
  ex_coretype_various_complex_types Bindoj_runtime.json_full_decoder

val ex_coretype_various_complex_types_of_json :
  Kxclib.Json.jv -> ex_coretype_various_complex_types option

val ex_coretype_various_complex_types_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_coretype_various_complex_types_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_coretype_various_complex_types )
  Bindoj_runtime.generic_typed_type_decl

type ex_coretype_various_tuple_types = {
  tuple : int * int;
  objtuple : int * int;
  nested : int option * int list * (int * int);
}

val ex_coretype_various_tuple_types_reflect :
  ex_coretype_various_tuple_types Bindoj_runtime.Refl.t

val ex_coretype_various_tuple_types_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_coretype_various_tuple_types_to_json :
  ex_coretype_various_tuple_types -> Kxclib.Json.jv

val ex_coretype_various_tuple_types_of_json' :
  ex_coretype_various_tuple_types Bindoj_runtime.json_full_decoder

val ex_coretype_various_tuple_types_of_json :
  Kxclib.Json.jv -> ex_coretype_various_tuple_types option

val ex_coretype_various_tuple_types_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_coretype_various_tuple_types_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_coretype_various_tuple_types )
  Bindoj_runtime.generic_typed_type_decl

type ex_coretype_named_json = {
  name : string;
  json : Bindoj_std_runtime.Json_value.t;
}

val ex_coretype_named_json_reflect :
  ex_coretype_named_json Bindoj_runtime.Refl.t

val ex_coretype_named_json_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_coretype_named_json_to_json : ex_coretype_named_json -> Kxclib.Json.jv

val ex_coretype_named_json_of_json' :
  ex_coretype_named_json Bindoj_runtime.json_full_decoder

val ex_coretype_named_json_of_json :
  Kxclib.Json.jv -> ex_coretype_named_json option

val ex_coretype_named_json_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_coretype_named_json_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_coretype_named_json )
  Bindoj_runtime.generic_typed_type_decl
