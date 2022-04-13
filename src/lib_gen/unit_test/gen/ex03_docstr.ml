let ex03 : type_decl =
  { td_name = "int_list";
    td_kind =
      Variant_kind
        [Cstr_tuple { ct_name = "IntNil";
                      ct_args = [];
                      ct_codec = `default_codec;
                      ct_flvconfigs = [Flvconfig_flat_kind
                                         { kind_fname=Some "kind"; arg_fname=Some "arg"; }]
                    }, `docstr "nil for int_list";
         Cstr_tuple { ct_name = "IntCons";
                      ct_args = ["int"; "int_list"];
                      ct_codec = `default_codec;
                      ct_flvconfigs = [Flvconfig_flat_kind
                                         { kind_fname=Some "kind"; arg_fname=Some "arg"; }]
                    }, `docstr "cons for int_list"],
      `docstr "int list"; }

let () =
  let open Ppxlib in
  let open Ast_builder.Default in
  let loc = Location.none in
  Astlib.Pprintast.structure Format.std_formatter [
    (pstr_type ~loc Recursive [type_declaration_of_type_decl ex03]);
    (pstr_value ~loc Recursive
       [gen_json_encoder ~self_contained:true ex03]);
    (pstr_value ~loc Recursive
       [gen_json_decoder ~self_contained:true ex03]);
  ]
