type codec = [ `default_codec | `codec_val of string | `codec_in_module of string ]
type 'x with_docstr = 'x*[ `docstr of string | `nodoc ]

type flavor = [
  | `flat_kind
  | `tuple
]

type ('pos, 'flavor) flavor_config = ..

module FlavorConfigs = struct
  type 'pos t = [] : 'pos t | (::) : (('pos, _) flavor_config * 'pos t) -> 'pos t
end
type 'pos flavor_configs = 'pos FlavorConfigs.t

type ('pos, 'flavor) flavor_config +=
  | Flvconfig_flat_kind : {
      kind_fname : string option;
      arg_fname : string option;
    } -> ([ `branch ], [ `flat_kind ]) flavor_config

let default_kind_fname = "kind"
let default_arg_fname = "arg"

type record_type_desc = record_field_desc with_docstr list
and record_field_desc = {
  rf_name : string;
  rf_type : string;
  rf_codec : codec;
}

type variant_type_desc = variant_constructor_desc with_docstr list
and variant_constructor_desc =
  | Cstr_tuple of {
      ct_name : string;
      ct_args : string list;
      ct_codec : codec;
      ct_flvconfigs : [ `branch ] flavor_configs;
    }
  | Cstr_record of {
      cr_name : string;
      cr_fields : record_type_desc;
      cr_codec : codec;
      cr_flvconfigs : [ `branch ] flavor_configs;
    }

type generic_kind =
  | Record_kind of record_type_desc
  | Variant_kind of variant_type_desc

type type_decl = {
  td_name : string;
  td_kind : generic_kind with_docstr;
}

