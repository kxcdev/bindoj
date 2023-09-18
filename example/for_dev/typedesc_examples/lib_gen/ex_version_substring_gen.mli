type ex_version_substring_record_v3_2_1 = {
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

val ex_version_substring_record_v3_2_1_reflect :
  ex_version_substring_record_v3_2_1 Bindoj_runtime.Refl.t

val ex_version_substring_record_v3_2_1_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_version_substring_record_v3_2_1_to_json :
  ex_version_substring_record_v3_2_1 -> Kxclib.Json.jv

val ex_version_substring_record_v3_2_1_of_json' :
  ex_version_substring_record_v3_2_1 Bindoj_runtime.json_full_decoder

val ex_version_substring_record_v3_2_1_of_json :
  Kxclib.Json.jv -> ex_version_substring_record_v3_2_1 option

val ex_version_substring_record_v3_2_1_decl :
  Bindoj_typedesc.Type_desc.type_decl

val ex_version_substring_record_v3_2_1_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_version_substring_record_v3_2_1 )
  Bindoj_runtime.generic_typed_type_decl

type ex_version_substring_variant_v1_0 =
  | Version_info_v1_0 of {
      version_info_v1 : int;
      version_info_v1_0 : int;
      version_info_v1_0_1 : int;
    }
  | Version_v1_0_info of {
      version_v1_info : int;
      version_v1_0_info : int;
      version_v1_0_1_info : int;
    }
  | V1_0_version_info of {
      v1_version_info : int;
      v1_0_version_info : int;
      v1_0_1_version_info : int;
    }
  | No_preserving_version_substring_v1_0 of {
      version_info_v1 : int;
      version_info_v1_0 : int;
      version_info_v1_0_1 : int;
    }

val ex_version_substring_variant_v1_0_reflect :
  ex_version_substring_variant_v1_0 Bindoj_runtime.Refl.t

val ex_version_substring_variant_v1_0_json_discriminator_value :
  ex_version_substring_variant_v1_0 -> string

val ex_version_substring_variant_v1_0_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_version_substring_variant_v1_0_to_json :
  ex_version_substring_variant_v1_0 -> Kxclib.Json.jv

val ex_version_substring_variant_v1_0_of_json' :
  ex_version_substring_variant_v1_0 Bindoj_runtime.json_full_decoder

val ex_version_substring_variant_v1_0_of_json :
  Kxclib.Json.jv -> ex_version_substring_variant_v1_0 option

val ex_version_substring_variant_v1_0_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_version_substring_variant_v1_0_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_version_substring_variant_v1_0 )
  Bindoj_runtime.generic_typed_type_decl
