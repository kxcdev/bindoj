type nested_record = {
  unit : Ex11_gen.unit;
  student : Ex01_gen.student;
  int53p : Ex09_gen.with_int53p;
  person1 : Ex02_no_mangling_gen.person;
  person2 : Ex02_no_mangling_gen.person;
}

val nested_record_reflect : nested_record Bindoj_runtime.Refl.t
val nested_record_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val nested_record_to_json : nested_record -> Kxclib.Json.jv
val nested_record_of_json' : nested_record Bindoj_runtime.json_full_decoder
val nested_record_of_json : Kxclib.Json.jv -> nested_record option
val nested_record_decl : Bindoj_typedesc.Type_desc.type_decl

val nested_record_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    nested_record )
  Bindoj_runtime.generic_typed_type_decl
