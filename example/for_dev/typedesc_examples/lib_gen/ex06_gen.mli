type various_prim_types = {
  unit : unit;
  bool : bool;
  int : int;
  float : float;
  string : string;
  uchar : Uchar.t;
  byte : char;
  bytes : Bytes.t;
}

val various_prim_types_reflect : various_prim_types Bindoj_runtime.Refl.t

val various_prim_types_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val various_prim_types_to_json : various_prim_types -> Kxclib.Json.jv

val various_prim_types_of_json' :
  various_prim_types Bindoj_runtime.json_full_decoder

val various_prim_types_of_json : Kxclib.Json.jv -> various_prim_types option
val various_prim_types_decl : Bindoj_typedesc.Type_desc.type_decl

val various_prim_types_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    various_prim_types )
  Bindoj_runtime.generic_typed_type_decl
