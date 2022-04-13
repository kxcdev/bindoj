let ex03 : type_decl =
  { td_name = "intlist";
    td_kind =
      Variant_kind
        [Cstr_tuple { ct_name = "NIL";
                      ct_args = [];
                      ct_codec = `default_codec;
                      ct_flvconfigs = [Flvconfig_flat_kind
                                         { kind_fname=Some "kind"; arg_fname=Some "arg"; }]
                    }, `nodoc;
         Cstr_tuple { ct_name = "CONS";
                      ct_args = ["intlist"];
                      ct_codec = `default_codec;
                      ct_flvconfigs = [Flvconfig_flat_kind
                                         { kind_fname=Some "kind"; arg_fname=Some "arg"; }]
                    }, `nodoc],
      `nodoc; }

let () =
  print_endline (gen_ts_type ex03);
  print_endline (gen_ts_case_analyzer ex03)
