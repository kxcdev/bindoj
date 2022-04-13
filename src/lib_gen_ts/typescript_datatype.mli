type flavor = Bindoj_gen.Json_codec.variant_type_flavor

val gen_ts_type : ?flavor:flavor -> type_decl -> string
val gen_ts_case_analyzer : ?flavor:flavor -> type_decl -> string
