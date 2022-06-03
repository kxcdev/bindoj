(* Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

type ts_ast = ts_statement list [@@deriving show]
and ts_statement = [
  | `type_alias_declaration of ts_type_alias_decl
  | `function_declaration of ts_func_decl
  | `return_statement of ts_expression
  | `if_statement of ts_expression * ts_statement * ts_statement
  | `throw_statement of ts_expression
  | `block of ts_ast
]
and ts_type_alias_decl = {
  tsa_modifiers : ts_modifier list;
  tsa_name : string;
  tsa_type_parameters : string list;
  tsa_type_desc : ts_type_desc;
}
and ts_func_decl = {
  tsf_modifiers : ts_modifier list;
  tsf_name : string;
  tsf_type_parameters : string list;
  tsf_parameters : ts_parameter list;
  tsf_type_desc : ts_type_desc;
  tsf_body : ts_ast;
}
and ts_type_desc = [
  | `type_reference of string (* includes primitive types *)
  | `type_literal of ts_property_signature list
  | `literal_type of ts_literal_type
  | `tuple of ts_type_desc list
  | `union of ts_type_desc list
  | `func_type of ts_func_type_desc
]
and ts_property_signature = {
  tsps_modifiers : [ `read_only ] list;
  tsps_name : string;
  tsps_type_desc : ts_type_desc;
}
and ts_literal_type = [
  | `numeric_literal of float
  | `string_literal of string
]
and ts_parameter = {
  tsp_name : string;
  tsp_type_desc : ts_type_desc;
}
and ts_func_type_desc = {
  tsft_parameters : ts_parameter list;
  tsft_type_desc : ts_type_desc;
}
and ts_expression = [
  | `identifier of string
  | `literal_expression of ts_literal_expression
  | `call_expression of ts_call_expression
  | `element_access_expression of ts_element_access_expression
  | `property_access_expression of ts_property_access_expression
  | `binary_expression of ts_binary_expression
  | `arrow_function of ts_arrow_function
  | `new_expression of ts_new_expression
]
and ts_literal_expression = [
  | `string_literal of string
]
and ts_call_expression = {
  tsce_expression : ts_expression;
  tsce_arguments : ts_expression list;
}
and ts_element_access_expression = {
  tsea_expression : ts_expression;
  tsea_argument : ts_expression;
}
and ts_property_access_expression = {
  tspa_expression : ts_expression;
  tspa_name : string;
}
and ts_binary_expression = {
  tsbe_left : ts_expression;
  tsbe_operator_token : string;
  tsbe_right : ts_expression;
}
and ts_arrow_function = {
  tsaf_parameters : ts_parameter list;
  tsaf_body : ts_ast;
}
and ts_new_expression = {
  tsne_expression : ts_expression;
  tsne_arguments : ts_expression list;
}
and ts_modifier = [
  | `export
]

open Bindoj_gen.Json_codec
type flavor = variant_type_flavor

open Bindoj_gen_foreign.Foreign_datatype

let annotate_fwrt_decl : bool -> bool -> (unit, unit) fwrt_decl -> (ts_modifier list, [`read_only] list) fwrt_decl =
  fun export read_only (name, env) ->
  (name,
   FwrtTypeEnv.annotate
     name
     (if export then ([`export], []) else ([], []))
     (if read_only then ([`read_only], []) else ([], []))
     env)

let default_type_convertion_map : TypeMap.t =
  TypeMap.empty
  |> TypeMap.add_convertion "int" "number"
  |> TypeMap.add_convertion "float" "number"
  |> TypeMap.add_convertion "bool" "boolean"
  |> TypeMap.add_convertion "string" "string"

let type_reference x =
  `type_reference (TypeMap.convert_type default_type_convertion_map x)

let rec ts_ast_of_fwrt_decl : (ts_modifier list, [`read_only] list) fwrt_decl -> ts_ast =
  fun fwrt_decl ->
  match FwrtTypeEnv.lookup (fst fwrt_decl) (snd fwrt_decl) with
  | { fd_children = []; _; }, _ ->
    [ `type_alias_declaration (ts_type_alias_decl_of_fwrt_decl fwrt_decl) ]
  | _ ->
    [ `type_alias_declaration (ts_type_alias_decl_of_fwrt_decl fwrt_decl);
      `function_declaration (ts_func_decl_of_fwrt_decl fwrt_decl) ]

and ts_type_alias_decl_of_fwrt_decl :
  (ts_modifier list, [`read_only] list) fwrt_decl -> ts_type_alias_decl =
  fun (name, env) ->
  let ({ fd_name; fd_kind_fname; fd_children; fd_fields; fd_annot; _; }, _doc) = FwrtTypeEnv.lookup name env in
  let () = assert (name = fd_name) in
  let members =
    fd_fields |&> fun ({ ff_name; ff_type; ff_annot; }, _doc) ->
      { tsps_modifiers = ff_annot;
        tsps_name = ff_name;
        tsps_type_desc = match ff_type with
          | [] -> failwith "impossible ff_type"
          | [typ] ->
            type_reference typ
          | typs ->
            `tuple
              (typs |&> fun typ ->
                type_reference typ); } in
  let children =
    fd_children |&> fun child ->
      let { tsa_name; tsa_type_desc; _; } =
        ts_type_alias_decl_of_fwrt_decl (child, env) in
      let kind_field =
        { tsps_modifiers = [];
          tsps_name = fd_kind_fname;
          tsps_type_desc = `literal_type (`string_literal tsa_name); } in
      begin match tsa_type_desc with
        | `type_literal fields -> `type_literal (kind_field :: fields)
        | _ -> failwith "tsa_type_desc in children must be type literal"
      end in
  let desc = match members, children with
    | _, [] -> `type_literal members
    | [], _ -> `union children
    | _ -> `union (`type_literal members :: children) in
  { tsa_modifiers = fd_annot;
    tsa_name = name;
    tsa_type_parameters = [];
    tsa_type_desc = desc; }

and ts_func_decl_of_fwrt_decl :
  (ts_modifier list, [`read_only] list) fwrt_decl -> ts_func_decl =
  fun (name, env) ->
  let ({ fd_name; fd_kind_fname; fd_children; fd_fields; fd_annot; _; }, _doc) = FwrtTypeEnv.lookup name env in
  let () = assert (name = fd_name) in
  let () = assert (fd_fields = []) in
  let name = "analyze_" ^ fd_name in
  let type_param = "__bindoj_ret" in
  let param = "__bindoj_fns" in
  let var_v = "__bindoj_v" in
  let param_type =
    `type_literal
      (fd_children |&> fun child ->
          let ({ fd_name; fd_kind_fname; fd_fields; _; }, _) = FwrtTypeEnv.lookup child env in
          { tsps_modifiers = [];
            tsps_name = fd_name;
            tsps_type_desc =
              (`func_type
                 { tsft_parameters =
                     [{ tsp_name = var_v;
                        tsp_type_desc =
                          `type_literal
                            ({ tsps_modifiers = [];
                               tsps_name = fd_kind_fname;
                               tsps_type_desc = `literal_type (`string_literal fd_name); } ::
                             (fd_fields |&> fun ({ ff_name; ff_type; ff_annot; }, _) ->
                                 { tsps_modifiers = ff_annot;
                                   tsps_name = ff_name;
                                   tsps_type_desc = match ff_type with
                                     | [] -> failwith "ff_type must have content"
                                     | [typ] -> type_reference typ
                                     | typs -> `tuple (typs |&> fun typ -> type_reference typ); })); }];
                   tsft_type_desc = type_reference type_param; }); }) in
  let var_x = "__bindoj_x" in
  let type_desc =
    `func_type
      { tsft_parameters = [{ tsp_name = var_x; tsp_type_desc = type_reference fd_name; }];
        tsft_type_desc = type_reference type_param; } in
  let body =
    (`return_statement
       (`arrow_function
          { tsaf_parameters =
              [{ tsp_name = var_x;
                 tsp_type_desc = type_reference fd_name; }];
            tsaf_body =
              [(List.rev fd_children) |@>
               (`throw_statement
                  (`new_expression
                     { tsne_expression = `identifier "TypeError";
                       tsne_arguments =
                         [`binary_expression
                            { tsbe_left =
                                `literal_expression (`string_literal ("panic @analyze_"^fd_name^" - unrecognized: "));
                              tsbe_operator_token = "+";
                              tsbe_right = `identifier var_x; }]; }),
                fun (acc, child) ->
                  let ({ fd_name; _; }, _) = FwrtTypeEnv.lookup child env in
                  `if_statement
                    ((`binary_expression
                        { tsbe_left =
                            `property_access_expression
                              { tspa_expression = `identifier var_x;
                                tspa_name = fd_kind_fname; };
                          tsbe_operator_token = "===";
                          tsbe_right = `literal_expression (`string_literal fd_name); }),
                     (`return_statement
                        (`call_expression
                           { tsce_expression =
                               `element_access_expression
                                 { tsea_expression = `identifier param;
                                   tsea_argument =
                                     `property_access_expression
                                       { tspa_expression = `identifier var_x;
                                         tspa_name = fd_kind_fname; }; };
                             tsce_arguments = [`identifier var_x]; })),
                     acc))]; })) in
  { tsf_modifiers = fd_annot;
    tsf_name = name;
    tsf_type_parameters = [type_param];
    tsf_parameters = [{ tsp_name = param; tsp_type_desc = param_type; }];
    tsf_type_desc = type_desc;
    tsf_body = [body]; }


module Rope = struct
  [@ocaml.warning "-32"]
  [@ocaml.warning "-34"]
  type t = Zed_rope.t
  type rope = Zed_rope.rope
  let empty = Zed_rope.empty
  let length = Zed_rope.length
  let is_empty = Zed_rope.is_empty
  let get = Zed_rope.get
  let sub = Zed_rope.sub
  let append = Zed_rope.append
  let concat = Zed_rope.concat
  let insert = Zed_rope.insert
  let of_string s = Zed_rope.of_string (Zed_string.of_utf8 s)
  let to_string r = Zed_string.to_utf8 (Zed_rope.to_string r)
end

module RopeUtil = struct
  [@ocaml.warning "-32"]
  [@ocaml.warning "-34"]
  let rope = Rope.of_string
  let (++) = Rope.append
  let (^) = Rope.append
  let (@+) s t = rope s ++ t
  let (+@) s t = s ++ rope t
  let between l r s = l @+ s +@ r

  let roprintf fmt = Format.ksprintf Rope.of_string fmt
  (** sprintf のように rope を作れる *)

  let concat = Rope.concat
  let concat_str sep xs = Rope.concat (rope sep) xs
  let comma_separated_list xs = Rope.concat (rope ", ") xs
end

let rec rope_of_ts_ast : ts_ast -> Rope.t = fun statements ->
  let open RopeUtil in
  (statements |&> rope_of_ts_statement)
  |> concat_str "\n"

and rope_of_ts_statement : ts_statement -> Rope.t =
  let open RopeUtil in
  function
  | `type_alias_declaration type_alias_decl ->
    rope_of_ts_type_alias_decl type_alias_decl
  | `function_declaration func_decl ->
    rope_of_ts_func_decl func_decl
  | `return_statement expr ->
    "return " @+ rope_of_ts_expression expr
  | `if_statement (test, then_stat, else_stat) ->
    "if (" @+
    rope_of_ts_expression test
    +@ ") {\n" ++
    rope_of_ts_statement then_stat
    +@ "\n} else " ++
    (match else_stat with
     | `if_statement _ ->
       rope_of_ts_statement else_stat
     | _ ->
       "{\n" @+ rope_of_ts_statement else_stat +@ "\n}")
  | `throw_statement expr ->
    "throw " @+ rope_of_ts_expression expr
  | `block body ->
    between "{\n" "\n}" (rope_of_ts_ast body)

and rope_of_ts_type_alias_decl : ts_type_alias_decl -> Rope.t =
  fun { tsa_modifiers; tsa_name; tsa_type_parameters; tsa_type_desc; } ->
  let open RopeUtil in
  let export =
    if List.exists (( = ) `export) tsa_modifiers then
      rope "export "
    else
      rope "" in
  let name = rope tsa_name in
  let type_parameters =
    tsa_type_parameters |&> rope
    |> comma_separated_list in
  let type_desc = rope_of_ts_type_desc tsa_type_desc in
  export +@ "type " ++ name +@ " = " ++ type_parameters ++ type_desc

and rope_of_ts_func_decl : ts_func_decl -> Rope.t =
  fun { tsf_modifiers; tsf_name; tsf_type_parameters; tsf_parameters; tsf_type_desc; tsf_body; } ->
  let open RopeUtil in
  let export =
    if List.exists (( = ) `export) tsf_modifiers then
      rope "export "
    else
      rope "" in
  let name = rope tsf_name in
  let type_parameters =
    tsf_type_parameters |&> rope
    |> comma_separated_list
    |> between "<" ">" in
  let parameters =
    (tsf_parameters |&> fun { tsp_name; tsp_type_desc; } ->
        tsp_name @+ " :\n" @+ rope_of_ts_type_desc tsp_type_desc)
    |> comma_separated_list
    |> between "(\n" "\n)" in
  let type_desc = rope_of_ts_type_desc tsf_type_desc in
  let body =
    rope_of_ts_ast tsf_body
    |> between "{\n" "\n}" in
  export +@ "function " ++ name ++ type_parameters ++ parameters ++ (" : " @+ type_desc +@ "\n") ++ body

and rope_of_ts_type_desc : ts_type_desc -> Rope.t =
  let open RopeUtil in
  function
  | `type_reference s ->
    rope s
  | `type_literal members ->
    (members |&> fun { tsps_modifiers; tsps_name; tsps_type_desc; } ->
        let read_only =
          if List.exists (( = ) `read_only) tsps_modifiers then
            rope "readonly "
          else
            rope "" in
        let name = rope tsps_name in
        let type_desc = rope_of_ts_type_desc tsps_type_desc in
        read_only ++ (name +@ " : ") ++ type_desc)
    |> comma_separated_list
    |> between "{ " " }"
  | `literal_type (`numeric_literal f) ->
    rope (string_of_float f)
  | `literal_type (`string_literal s) ->
    between "\"" "\"" (rope s)
  | `tuple type_descs ->
    type_descs |&> rope_of_ts_type_desc
    |> comma_separated_list
    |> between "[" "]"
  | `union type_descs ->
    type_descs |&> rope_of_ts_type_desc
    |> concat_str "\n| "
  | `func_type { tsft_parameters;
                 tsft_type_desc; } ->
    let parameters =
      (tsft_parameters |&> fun { tsp_name; tsp_type_desc; } ->
          tsp_name @+ " : " @+ rope_of_ts_type_desc tsp_type_desc)
      |> comma_separated_list
      |> between "(" ")" in
    let type_desc = rope_of_ts_type_desc tsft_type_desc in
    parameters +@ " => " ++ type_desc

and rope_of_ts_expression : ts_expression -> Rope.t =
  let open RopeUtil in
  function
  | `identifier id -> rope id
  | `literal_expression lit -> begin match lit with
      | `string_literal s -> between "\"" "\"" (rope s)
    end
  | `call_expression { tsce_expression; tsce_arguments; } ->
    rope_of_ts_expression tsce_expression
    +@ "(" ++
    (tsce_arguments |&> rope_of_ts_expression |> comma_separated_list)
    +@ ")"
  | `element_access_expression { tsea_expression; tsea_argument; } ->
    rope_of_ts_expression tsea_expression
    ++ between "[" "]" (rope_of_ts_expression tsea_argument)
  | `property_access_expression { tspa_expression; tspa_name; } ->
    rope_of_ts_expression tspa_expression +@ "." +@ tspa_name
  | `binary_expression { tsbe_left; tsbe_operator_token; tsbe_right; } ->
    rope_of_ts_expression tsbe_left
    +@ " " +@ tsbe_operator_token +@ " " ++
    rope_of_ts_expression tsbe_right
  | `arrow_function { tsaf_parameters; tsaf_body; } ->
    (tsaf_parameters |&> fun { tsp_name; tsp_type_desc; } ->
        tsp_name @+ " : " @+ rope_of_ts_type_desc tsp_type_desc)
    |> comma_separated_list
    |> between "(" ")"
    |> fun param -> param +@ " => " ++ between "{\n" "\n}" (rope_of_ts_ast tsaf_body)
  | `new_expression { tsne_expression; tsne_arguments; } ->
    "new " @+
    rope_of_ts_expression tsne_expression ++
    between "(" ")" (tsne_arguments |&> rope_of_ts_expression |> comma_separated_list)


let gen_ts_type : ?export:bool -> ?flavor:flavor -> type_decl -> string =
  fun ?(export=true) ?(flavor=`flat_kind) type_decl ->
  let fwrt_decl = annotate_fwrt_decl export false (fwrt_decl_of_type_decl flavor type_decl) in
  let ts_type_alias_decl = ts_type_alias_decl_of_fwrt_decl fwrt_decl in
  let rope = rope_of_ts_type_alias_decl ts_type_alias_decl in
  Rope.to_string rope

let gen_ts_case_analyzer : ?export:bool -> ?flavor:flavor -> type_decl -> string =
  fun ?(export=true) ?(flavor=`flat_kind) type_decl ->
  let fwrt_decl = annotate_fwrt_decl export false (fwrt_decl_of_type_decl flavor type_decl) in
  let ts_func_decl = ts_func_decl_of_fwrt_decl fwrt_decl in
  let rope = rope_of_ts_func_decl ts_func_decl in
  Rope.to_string rope

