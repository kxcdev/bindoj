(* Copyright 2022 Kotoi-Xie Consultancy, Inc. This file is a part of the

==== Bindoj (https://kxc.dev/bindoj) ====

software project that is developed, maintained, and distributed by
Kotoi-Xie Consultancy, Inc. (https://kxc.inc) which is also known as KXC.

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy
of the License at http://www.apache.org/licenses/LICENSE-2.0. Unless required
by applicable law or agreed to in writing, software distributed under the
License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
OF ANY KIND, either express or implied. See the License for the specific
language governing permissions and limitations under the License.
                                                                              *)
(* Acknowledgements  --- AnchorZ Inc. ---  The current/initial version or a
significant portion of this file is developed under the funding provided by
AnchorZ Inc. to satisfy its needs in its product development workflow.
                                                                              *)
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
