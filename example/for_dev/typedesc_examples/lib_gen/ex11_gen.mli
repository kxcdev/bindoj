type nonrec unit = unit

val unit_reflect : unit Bindoj_runtime.Refl.t
val unit_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val unit_to_json : unit -> Kxclib.Json.jv
val unit_of_json' : unit Bindoj_runtime.json_full_decoder
val unit_of_json : Kxclib.Json.jv -> unit option
val unit_decl : Bindoj_typedesc.Type_desc.type_decl

val unit_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    unit )
  Bindoj_runtime.generic_typed_type_decl
