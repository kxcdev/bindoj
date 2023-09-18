type ex_variant_person =
  | Anonymous
  | With_id of int
  | Student of { student_id : int; name : string }
  | Teacher of { faculty_id : int; name : string; department : string }

val ex_variant_person_reflect : ex_variant_person Bindoj_runtime.Refl.t
val ex_variant_person_json_discriminator_value : ex_variant_person -> string

val ex_variant_person_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_variant_person_to_json : ex_variant_person -> Kxclib.Json.jv

val ex_variant_person_of_json' :
  ex_variant_person Bindoj_runtime.json_full_decoder

val ex_variant_person_of_json : Kxclib.Json.jv -> ex_variant_person option
val ex_variant_person_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_variant_person_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_variant_person )
  Bindoj_runtime.generic_typed_type_decl

type ex_variant_person_reused =
  | Anonymous
  | With_id of int
  | Student of { student_id : int; name : string }
  | Teacher of { faculty_id : int; name : string; department : string }

val ex_variant_person_reused_reflect :
  ex_variant_person_reused Bindoj_runtime.Refl.t

val ex_variant_person_reused_json_discriminator_value :
  ex_variant_person_reused -> string

val ex_variant_person_reused_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_variant_person_reused_to_json :
  ex_variant_person_reused -> Kxclib.Json.jv

val ex_variant_person_reused_of_json' :
  ex_variant_person_reused Bindoj_runtime.json_full_decoder

val ex_variant_person_reused_of_json :
  Kxclib.Json.jv -> ex_variant_person_reused option

val ex_variant_person_reused_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_variant_person_reused_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_variant_person_reused )
  Bindoj_runtime.generic_typed_type_decl

type ex_variant_int_list = IntNil | IntCons of int * ex_variant_int_list

val ex_variant_int_list_reflect : ex_variant_int_list Bindoj_runtime.Refl.t
val ex_variant_int_list_json_discriminator_value : ex_variant_int_list -> string

val ex_variant_int_list_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_variant_int_list_to_json : ex_variant_int_list -> Kxclib.Json.jv

val ex_variant_int_list_of_json' :
  ex_variant_int_list Bindoj_runtime.json_full_decoder

val ex_variant_int_list_of_json : Kxclib.Json.jv -> ex_variant_int_list option
val ex_variant_int_list_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_variant_int_list_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_variant_int_list )
  Bindoj_runtime.generic_typed_type_decl

type ex_variant_int_list_objtuple =
  | IntNil
  | IntCons of int * ex_variant_int_list_objtuple

val ex_variant_int_list_objtuple_reflect :
  ex_variant_int_list_objtuple Bindoj_runtime.Refl.t

val ex_variant_int_list_objtuple_json_discriminator_value :
  ex_variant_int_list_objtuple -> string

val ex_variant_int_list_objtuple_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_variant_int_list_objtuple_to_json :
  ex_variant_int_list_objtuple -> Kxclib.Json.jv

val ex_variant_int_list_objtuple_of_json' :
  ex_variant_int_list_objtuple Bindoj_runtime.json_full_decoder

val ex_variant_int_list_objtuple_of_json :
  Kxclib.Json.jv -> ex_variant_int_list_objtuple option

val ex_variant_int_list_objtuple_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_variant_int_list_objtuple_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_variant_int_list_objtuple )
  Bindoj_runtime.generic_typed_type_decl

type ex_variant_foo = [ `Foo0 | `Foo1 of int | `Foo2 of int * int ]

val ex_variant_foo_reflect : ex_variant_foo Bindoj_runtime.Refl.t
val ex_variant_foo_json_discriminator_value : ex_variant_foo -> string

val ex_variant_foo_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_variant_foo_to_json : ex_variant_foo -> Kxclib.Json.jv
val ex_variant_foo_of_json' : ex_variant_foo Bindoj_runtime.json_full_decoder
val ex_variant_foo_of_json : Kxclib.Json.jv -> ex_variant_foo option
val ex_variant_foo_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_variant_foo_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_variant_foo )
  Bindoj_runtime.generic_typed_type_decl

type ex_variant_customized_union =
  | Case1 of int
  | Case2 of { x : int; y : int }

val ex_variant_customized_union_reflect :
  ex_variant_customized_union Bindoj_runtime.Refl.t

val ex_variant_customized_union_json_discriminator_value :
  ex_variant_customized_union -> string

val ex_variant_customized_union_json_shape_explanation :
  Bindoj_runtime.json_shape_explanation

val ex_variant_customized_union_to_json :
  ex_variant_customized_union -> Kxclib.Json.jv

val ex_variant_customized_union_of_json' :
  ex_variant_customized_union Bindoj_runtime.json_full_decoder

val ex_variant_customized_union_of_json :
  Kxclib.Json.jv -> ex_variant_customized_union option

val ex_variant_customized_union_decl : Bindoj_typedesc.Type_desc.type_decl

val ex_variant_customized_union_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    ex_variant_customized_union )
  Bindoj_runtime.generic_typed_type_decl
