type preserving_version_info_v1_0 =
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

val preserving_version_info_v1_0_reflect :
  preserving_version_info_v1_0 Bindoj_runtime.Refl.t

val preserving_version_info_v1_0_json_discriminator_value :
  preserving_version_info_v1_0 -> string

val preserving_version_info_v1_0_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val preserving_version_info_v1_0_to_json :
  preserving_version_info_v1_0 -> Kxclib.Json.jv

val preserving_version_info_v1_0_of_json' :
  preserving_version_info_v1_0 Bindoj_runtime.json_full_decoder

val preserving_version_info_v1_0_of_json :
  Kxclib.Json.jv -> preserving_version_info_v1_0 option

val preserving_version_info_v1_0_decl : Bindoj_typedesc.Type_desc.type_decl

val preserving_version_info_v1_0_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    preserving_version_info_v1_0 )
  Bindoj_runtime.generic_typed_type_decl
