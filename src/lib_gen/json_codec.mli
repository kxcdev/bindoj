open Ppxlib

val gen_primitive_encoders : codec -> value_binding list

val gen_primitive_decoders : codec -> value_binding list

val gen_json_encoder : ?self_contained:bool -> ?flavor:flavor -> ?codec:codec -> type_decl -> value_binding

val gen_json_decoder : ?self_contained:bool -> ?flavor:flavor -> ?codec:codec -> type_decl -> value_binding
