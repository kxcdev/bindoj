open Ppxlib

type variant_type_flavor = [
    `flat_kind
  (* | `tuple *)
  (* | `nested_kind *)
  ]
type ('pos, 'flavor) flavor_config +=
   | Flvconfig_flat_kind : {
       kind_fname : string option;
       arg_fname : string option;
     } -> ([ `branch ], [ `flat_kind ]) flavor_config

val gen_primitive_encoders : codec -> value_binding list

val gen_primitive_decoders : codec -> value_binding list

val gen_json_encoder : ?self_contained:bool -> ?flavor:variant_type_flavor -> ?codec:codec -> type_decl -> value_binding

val gen_json_decoder : ?self_contained:bool -> ?flavor:variant_type_flavor -> ?codec:codec -> type_decl -> value_binding

val default_kind_fname : string
val default_arg_fname : string
