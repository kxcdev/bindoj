type complex_types = {
  option : int option;
  list : int list;
  tuple : int * int;
  objtuple : int * int;
  nested : int option * int list * (int * int);
  map : (string * int) list;
}

val complex_types_reflect : complex_types Bindoj_runtime.Refl.t
val complex_types_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val complex_types_to_json : complex_types -> Kxclib.Json.jv
val complex_types_of_json' : complex_types Bindoj_runtime.json_full_decoder
val complex_types_of_json : Kxclib.Json.jv -> complex_types option
val complex_types_decl : Bindoj_typedesc.Type_desc.type_decl

val complex_types_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    complex_types )
  Bindoj_runtime.generic_typed_type_decl
