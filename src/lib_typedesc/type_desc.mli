type ('pos, 'flavor) flavor_config = ..
module FlavorConfigs : sig
  type 'pos t =
    | [] : 'pos t
    | (::) : (('pos, 'a) flavor_config * 'pos t) -> 'pos t
end
type 'pos flavor_configs = 'pos FlavorConfigs.t

type codec = [
    `codec_in_module of string
  | `codec_val of string
  | `default_codec
  ]

type 'x with_docstr = 'x * [ `docstr of string | `nodoc ]

type type_decl = {
    td_name : string;
    td_kind : generic_kind with_docstr;
  }

and generic_kind =
  | Record_kind of record_type_desc
  | Variant_kind of variant_type_desc

and record_type_desc =
  record_field_desc with_docstr list
and record_field_desc = {
  rf_name : string;
  rf_type : string;
  rf_codec : codec;
}

and variant_type_desc =
  variant_constructor_desc with_docstr list
and variant_constructor_desc =
  | Cstr_tuple of {
      ct_name       : string;
      ct_args       : string list;
      ct_codec      : codec;
      ct_flvconfigs : [ `branch ] flavor_configs;
    }
  | Cstr_record of {
      cr_name       : string;
      cr_fields     : record_type_desc;
      cr_codec      : codec;
      cr_flvconfigs : [ `branch ] flavor_configs;
    }
