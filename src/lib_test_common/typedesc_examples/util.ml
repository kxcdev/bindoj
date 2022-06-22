open Bindoj_gen_ts.Typescript_datatype

module Ts_ast = struct
  type options =
    { discriminator : string;
      var_x : string;
      var_v : string;
      var_fns : string;
      ret : string; }

  type literal = (string * [`type_literal of ts_property_signature list])
  let compare_literal (xname, `type_literal _) (yname, `type_literal _) = String.compare xname yname

  let case_analyzer_parameters :
    options -> literal list -> ts_parameter list =
    fun options cstrs ->
    [ { tsp_name = options.var_fns;
        tsp_type_desc =
          `type_literal
            (cstrs |&> fun (_, `type_literal cstr) ->
                match List.find (fun { tsps_name; _; } -> tsps_name = options.discriminator) cstr with
                | { tsps_type_desc = `literal_type (`string_literal kind); _; } ->
                  { tsps_modifiers = [];
                    tsps_name = kind;
                    tsps_type_desc =
                      `func_type
                        { tsft_parameters =
                            [ { tsp_name = options.var_v;
                                tsp_type_desc = `type_literal cstr; } ];
                          tsft_type_desc = `type_reference options.ret; }; }
                | _ -> failwith "impossible case"); } ]


  let case_analyzer_body :
    string -> options -> literal list -> ts_ast =
    fun name options cstrs ->
    [ `return_statement
        (`arrow_function
           { tsaf_parameters =
               [ { tsp_name = options.var_x;
                   tsp_type_desc = `type_reference name; } ];
             tsaf_body =
               [ cstrs |> List.sort compare_literal |> List.rev |@>
                 (`throw_statement
                    (`new_expression
                       { tsne_expression = `identifier "TypeError";
                         tsne_arguments =
                           [ `binary_expression
                               { tsbe_left =
                                   `literal_expression
                                     (`string_literal ("panic @analyze_" ^ name ^ " - unrecognized: "));
                                 tsbe_operator_token = "+";
                                 tsbe_right = `identifier options.var_x; } ]; }),
                  fun (statement, (_, `type_literal person)) ->
                    match List.find (fun { tsps_name; _; } -> tsps_name = options.discriminator) person with
                    | { tsps_type_desc = `literal_type (`string_literal kind); _; } ->
                      `if_statement
                        ((`binary_expression
                            { tsbe_left =
                                `property_access_expression
                                  { tspa_expression = `identifier options.var_x;
                                    tspa_name = options.discriminator; };
                              tsbe_operator_token = "===";
                              tsbe_right = `literal_expression (`string_literal kind); }),
                         (`return_statement
                            (`call_expression
                               { tsce_expression =
                                   `element_access_expression
                                     { tsea_expression = (`identifier options.var_fns);
                                       tsea_argument =
                                         `property_access_expression
                                           { tspa_expression = `identifier options.var_x;
                                             tspa_name = options.discriminator; }; };
                                 tsce_arguments = [ `identifier options.var_x ]; })),
                         statement)
                    | _ -> failwith "impossible case in test") ]; } ) ]
end
