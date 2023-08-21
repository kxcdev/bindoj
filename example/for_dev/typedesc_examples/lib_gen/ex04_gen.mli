type foo = [ `Foo0 | `Foo1 of int | `Foo2 of int * int ]

val foo_reflect : foo Bindoj_runtime.Refl.t
val foo_json_discriminator_value : foo -> string
val foo_json_shape_explanation : Bindoj_runtime.json_shape_explanation
val foo_to_json : foo -> Kxclib.Json.jv
val foo_of_json' : foo Bindoj_runtime.json_full_decoder
val foo_of_json : Kxclib.Json.jv -> foo option
val foo_decl : Bindoj_typedesc.Type_desc.type_decl

val foo_typed_decl :
  ( Bindoj_typedesc.Type_desc.type_decl,
    foo )
  Bindoj_runtime.generic_typed_type_decl
