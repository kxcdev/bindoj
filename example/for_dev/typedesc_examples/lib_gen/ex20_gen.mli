type v3_2_1_preserving_version_info = {
  v5_3_version_info :
    [ `Case_version_v1 | `Case_v2_0_version | `v3_0_1_case_version ];
  version_info_v2 : int;
  version_info_v2_0 : int;
  version_info_v2_0_1 : int;
  version_v3_info : int;
  version_v3_0_info : int;
  version_v3_0_1_info : int;
  v4_version_info : int;
  v4_0_version_info : int;
  v4_0_1_version_info : int;
  no_preserving_v1_2_version : int;
}

val v3_2_1_preserving_version_info_reflect :
  v3_2_1_preserving_version_info Bindoj_runtime.Refl.t

val v3_2_1_preserving_version_info_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val v3_2_1_preserving_version_info_to_json :
  v3_2_1_preserving_version_info -> Kxclib.Json.jv

val v3_2_1_preserving_version_info_of_json' :
  v3_2_1_preserving_version_info Bindoj_runtime.json_full_decoder

val v3_2_1_preserving_version_info_of_json :
  Kxclib.Json.jv -> v3_2_1_preserving_version_info option

val v3_2_1_preserving_version_info_decl : Bindoj_typedesc.Type_desc.type_decl

val v3_2_1_preserving_version_info_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    v3_2_1_preserving_version_info )
  Bindoj_runtime.generic_typed_type_decl
