type nonrec cases = [ `Case_at0 | `case_at1 | `Case_at2 | `Case_at3 ]

val cases_reflect : cases Bindoj_runtime.Refl.t
val cases_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val cases_to_json : cases -> Kxclib.Json.jv
val cases_of_json' : cases Bindoj_runtime.json_full_decoder
val cases_of_json : Kxclib.Json.jv -> cases option
val cases_decl : Bindoj_typedesc.Type_desc.type_decl

val cases_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    cases )
  Bindoj_runtime.generic_typed_type_decl
