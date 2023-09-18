type ex_mangling_student_inherited = {
  admission_year : int;
  name : string;
  case_value : [ `Case_at0 | `case_at1 ];
}

val ex_mangling_student_inherited_reflect :
  ex_mangling_student_inherited Bindoj_runtime.Refl.t

val ex_mangling_student_inherited_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_mangling_student_inherited_to_json :
  ex_mangling_student_inherited -> Kxclib.Json.jv

val ex_mangling_student_inherited_of_json' :
  ex_mangling_student_inherited Bindoj_runtime.json_full_decoder

val ex_mangling_student_inherited_of_json :
  Kxclib.Json.jv -> ex_mangling_student_inherited option

val ex_mangling_student_inherited_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_mangling_student_inherited_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_mangling_student_inherited )
  Bindoj_runtime.generic_typed_type_decl

type ex_mangling_person_no_mangling =
  | Anonymous
  | With_id of int
  | Student of { student_id : int; name : string }
  | Teacher of { faculty_id : int; name : string; department : string }

val ex_mangling_person_no_mangling_reflect :
  ex_mangling_person_no_mangling Bindoj_runtime.Refl.t

val ex_mangling_person_no_mangling_json_discriminator_value :
  ex_mangling_person_no_mangling -> string

val ex_mangling_person_no_mangling_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_mangling_person_no_mangling_to_json :
  ex_mangling_person_no_mangling -> Kxclib.Json.jv

val ex_mangling_person_no_mangling_of_json' :
  ex_mangling_person_no_mangling Bindoj_runtime.json_full_decoder

val ex_mangling_person_no_mangling_of_json :
  Kxclib.Json.jv -> ex_mangling_person_no_mangling option

val ex_mangling_person_no_mangling_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_mangling_person_no_mangling_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_mangling_person_no_mangling )
  Bindoj_runtime.generic_typed_type_decl

type ex_mangling_person_inherited =
  | Anonymous
  | With_id of int
  | Student of {
      student_id : int;
      name : string;
      case_value : [ `Case_at0 | `case_at1 ];
    }
  | Teacher of { faculty_id : int; name : string; department : string }

val ex_mangling_person_inherited_reflect :
  ex_mangling_person_inherited Bindoj_runtime.Refl.t

val ex_mangling_person_inherited_json_discriminator_value :
  ex_mangling_person_inherited -> string

val ex_mangling_person_inherited_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_mangling_person_inherited_to_json :
  ex_mangling_person_inherited -> Kxclib.Json.jv

val ex_mangling_person_inherited_of_json' :
  ex_mangling_person_inherited Bindoj_runtime.json_full_decoder

val ex_mangling_person_inherited_of_json :
  Kxclib.Json.jv -> ex_mangling_person_inherited option

val ex_mangling_person_inherited_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_mangling_person_inherited_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_mangling_person_inherited )
  Bindoj_runtime.generic_typed_type_decl

type nonrec ex_mangling_enum = [ `Case_at0 | `case_at1 | `Case_at2 | `Case_at3 ]

val ex_mangling_enum_reflect : ex_mangling_enum Bindoj_runtime.Refl.t

val ex_mangling_enum_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_mangling_enum_to_json : ex_mangling_enum -> Kxclib.Json.jv

val ex_mangling_enum_of_json' :
  ex_mangling_enum Bindoj_runtime.json_full_decoder

val ex_mangling_enum_of_json : Kxclib.Json.jv -> ex_mangling_enum option
val ex_mangling_enum_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_mangling_enum_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_mangling_enum )
  Bindoj_runtime.generic_typed_type_decl
