(* Copyright 2022 Kotoi-Xie Consultancy, Inc.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

(* Acknowledgements - AnchorZ Inc.
The initial version or a significant portion of this file is developed
under the funding of AnchorZ Inc. to satisfy its needs in
product development. *)

module Ts_config = struct
  include Bindoj_gen.Json_codec.Json_config
end

type 't ignore_order_list = 't list [@@deriving show]
let equal_ignore_order_list equal_t xs ys =
  List.equal equal_t (List.sort compare xs) (List.sort compare ys)

type ts_ast = ts_statement list [@@deriving show, eq]

and ts_statement = [
  | `type_alias_declaration of ts_type_alias_decl
  | `function_declaration of ts_func_decl
  | `return_statement of ts_expression
  | `if_statement of ts_expression * ts_statement * ts_statement
  | `throw_statement of ts_expression
  | `block of ts_ast
]

and ts_type_alias_decl = {
  tsa_modifiers : ts_modifier ignore_order_list;
  tsa_name : string;
  tsa_type_parameters : string list;
  tsa_type_desc : ts_type_desc;
}

and ts_func_decl = {
  tsf_modifiers : ts_modifier ignore_order_list;
  tsf_name : string;
  tsf_type_parameters : string list;
  tsf_parameters : ts_parameter list;
  tsf_type_desc : ts_type_desc;
  tsf_body : ts_ast;
}

and ts_type_desc = [
  | `type_reference of string (* includes primitive types *)
  | `type_literal of ts_property_signature ignore_order_list
  | `literal_type of ts_literal_type
  | `tuple of ts_type_desc list
  | `union of ts_type_desc ignore_order_list
  | `intersection of ts_type_desc ignore_order_list
  | `array of ts_type_desc
  | `func_type of ts_func_type_desc
  | `record of ts_type_desc * ts_type_desc (* https://www.typescriptlang.org/docs/handbook/utility-types.html#recordkeys-type *)
]

and ts_property_signature = {
  tsps_modifiers : [ `read_only ] ignore_order_list;
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

open Bindoj_typedesc.Type_desc
open Bindoj_gen_foreign.Foreign_datatype

let annotate_fwrt_decl : bool -> bool -> (unit, unit) fwrt_decl -> (ts_modifier list, [`read_only] list) fwrt_decl =
  fun export read_only (name, env) ->
  (name,
   FwrtTypeEnv.annotate
     name
     (if export then ([`export], []) else ([], []))
     (if read_only then ([`read_only], []) else ([], []))
     env)

let type_of_prim : Coretype.prim -> ts_type_desc = function
  | `unit -> `union [`type_reference "null"; `type_reference "undefined"]
  | `bool -> `type_reference "boolean"
  | `int | `float | `byte -> `type_reference "number"
  | `string | `uchar -> `type_reference "string"
  | `bytes -> `type_reference "string" (* base64 *)

let type_of_coretype : self_type_name:string -> coretype -> ts_type_desc =
  fun ~self_type_name { ct_desc; _ } ->
  let rec go =
    let open Coretype in
    function
    | Prim p -> type_of_prim p
    | Uninhabitable -> `type_reference "never"
    | Ident s -> `type_reference s.id_name
    | Option t -> `union [go t; `type_reference "null"; `type_reference "undefined"]
    | List t -> `array (go t)
    | Tuple ts -> `tuple (ts |> List.map go)
    | Map (k, v) -> `record (go (Coretype.desc_of_map_key k), go v)
    | Self -> `type_reference self_type_name
    | StringEnum cs -> `union (cs |> List.map (fun c -> `literal_type (`string_literal c)))
  in go ct_desc

let rec ts_ast_of_fwrt_decl :
  (ts_modifier list, [`read_only] list) fwrt_decl -> ts_ast =
  fun fwrt_decl ->
  let self_type_name = fst fwrt_decl in
  match FwrtTypeEnv.lookup (fst fwrt_decl) (snd fwrt_decl) with
  | { fd_kind = Fwrt_object { fo_children = _ :: _; _ }; _ } ->
    [ `type_alias_declaration (ts_type_alias_decl_of_fwrt_decl ~self_type_name fwrt_decl);
      `function_declaration (ts_func_decl_of_fwrt_decl ~self_type_name fwrt_decl) ]
  | _ ->
    [ `type_alias_declaration (ts_type_alias_decl_of_fwrt_decl ~self_type_name fwrt_decl) ]

and ts_type_alias_decl_of_fwrt_decl :
  self_type_name:string -> (ts_modifier list, [`read_only] list) fwrt_decl -> ts_type_alias_decl =
  fun ~self_type_name (name, env) ->
  let { fd_name; fd_kind; fd_annot; _ } = FwrtTypeEnv.lookup name env in
  assert (name = fd_name);
  let desc =
    match fd_kind with
    | Fwrt_object { fo_fields; fo_children; fo_configs } ->
      let members =
        fo_fields |&> fun { ff_name; ff_type; ff_annot; _ } ->
          { tsps_modifiers = ff_annot;
            tsps_name = ff_name;
            tsps_type_desc = type_of_coretype ~self_type_name ff_type }
      in
      let discriminator = fo_configs |> Ts_config.get_variant_discriminator in
      let children =
        fo_children |&> fun child ->
          let { tsa_name; tsa_type_desc; _; } =
            ts_type_alias_decl_of_fwrt_decl ~self_type_name (child, env) in
          let kind_field =
            { tsps_modifiers = [];
              tsps_name = discriminator;
              tsps_type_desc = `literal_type (`string_literal tsa_name); } in
          begin match tsa_type_desc with
            | `type_literal fields -> `type_literal (kind_field :: fields)
            | _ -> failwith "tsa_type_desc in children must be type literal"
          end
      in
      let desc = match members, children with
        | _, [] -> `type_literal members
        | [], _ -> `union children
        | _ -> `intersection [`type_literal members; `union children] in
      desc
    | Fwrt_alias { fa_type; _ } -> type_of_coretype ~self_type_name fa_type
    | Fwrt_constructor { fc_args; fc_fields; fc_configs } ->
      let arg_name = Ts_config.get_name Ts_config.default_name_of_variant_arg fc_configs in
      let members =
        let tmp =
          fc_fields |&> fun { ff_name; ff_type; ff_annot; _ } ->
            { tsps_modifiers = ff_annot;
              tsps_name = ff_name;
              tsps_type_desc = type_of_coretype ~self_type_name ff_type }
        in
        match fc_args with
        | [] -> tmp
        | [arg] ->
          { tsps_modifiers = [];
            tsps_name = arg_name;
            tsps_type_desc = type_of_coretype ~self_type_name arg } :: tmp
        | args ->
          let desc =
            `tuple (args |> List.map (type_of_coretype ~self_type_name))
          in
          { tsps_modifiers = [];
            tsps_name = arg_name;
            tsps_type_desc = desc } :: tmp
      in
      `type_literal members
  in
  { tsa_modifiers = fd_annot;
    tsa_name = name;
    tsa_type_parameters = [];
    tsa_type_desc = desc }

and ts_func_decl_of_fwrt_decl :
  self_type_name:string -> (ts_modifier list, [`read_only] list) fwrt_decl -> ts_func_decl =
  fun ~self_type_name (name, env) ->
  let { fd_name; fd_kind; fd_annot; _; } = FwrtTypeEnv.lookup name env in
  assert (name = fd_name);
  match fd_kind with
  | Fwrt_alias _ | Fwrt_constructor _ -> invalid_arg "this fwrt_decl cannot be a parent"
  | Fwrt_object { fo_children; fo_configs; _ } ->
    let name = "analyze_" ^ fd_name in
    let type_param = "__bindoj_ret" in
    let param = "__bindoj_fns" in
    let var_v = "__bindoj_v" in
    let discriminator = Ts_config.get_variant_discriminator fo_configs in
    let param_type =
      `type_literal (List.sort String.compare fo_children |&> fun child ->
        let decl = ts_type_alias_decl_of_fwrt_decl ~self_type_name (child, env) in
        let kind_field =
          { tsps_modifiers = [];
            tsps_name = discriminator;
            tsps_type_desc = `literal_type (`string_literal decl.tsa_name); } in
        let desc =
          match decl.tsa_type_desc with
          | `type_literal fields -> `type_literal (kind_field :: fields)
          | _ -> failwith "tsa_type_desc in children must be type literal"
        in
        { tsps_modifiers = [];
          tsps_name = decl.tsa_name;
          tsps_type_desc =
            (`func_type
              { tsft_parameters = [{ tsp_name = var_v; tsp_type_desc = desc }];
                tsft_type_desc = `type_reference type_param; }); })
    in
    let var_x = "__bindoj_x" in
    let type_desc =
      `func_type
        { tsft_parameters = [{ tsp_name = var_x; tsp_type_desc = `type_reference fd_name; }];
          tsft_type_desc = `type_reference type_param; } in
    let body =
      (`return_statement
        (`arrow_function
            { tsaf_parameters =
                [{ tsp_name = var_x;
                  tsp_type_desc = `type_reference fd_name; }];
              tsaf_body =
                [fo_children |> List.sort String.compare |> List.rev |@>
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
                    let { fd_name; _ } = FwrtTypeEnv.lookup child env in
                    `if_statement
                      ((`binary_expression
                          { tsbe_left =
                              `property_access_expression
                                { tspa_expression = `identifier var_x;
                                  tspa_name = discriminator; };
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
                                          tspa_name = discriminator; }; };
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
    "if " @+
    (rope_of_ts_expression test |> between "(" ")") ++
    (rope_of_ts_statement then_stat |> between "{\n" "\n} else ") ++
    (match else_stat with
     | `if_statement _ ->
       rope_of_ts_statement else_stat
     | _ ->
       rope_of_ts_statement else_stat |> between "{\n" "\n}")
  | `throw_statement expr ->
    "throw " @+ rope_of_ts_expression expr
  | `block body ->
    rope_of_ts_ast body |> between "{\n" "\n}"

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
    match tsf_type_parameters with
    | [] -> rope ""
    | _ ->
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
    type_descs |&> rope_of_ts_type_desc_with_paren
    |> concat_str "\n| "
  | `intersection type_descs ->
    type_descs |&> rope_of_ts_type_desc_with_paren
    |> concat_str "\n& "
  | `array t ->
    rope_of_ts_type_desc_with_paren t +@ "[]"
  | `record (k, v) -> (* Record<K, V> *)
    "Record<" @+ rope_of_ts_type_desc k +@ "," ++ rope_of_ts_type_desc v +@ ">"
  | `func_type { tsft_parameters;
                 tsft_type_desc; } ->
    let parameters =
      (tsft_parameters |&> fun { tsp_name; tsp_type_desc; } ->
          tsp_name @+ " : " @+ rope_of_ts_type_desc tsp_type_desc)
      |> comma_separated_list
      |> between "(" ")" in
    let type_desc = rope_of_ts_type_desc tsft_type_desc in
    parameters +@ " => " ++ type_desc

and rope_of_ts_type_desc_with_paren : ts_type_desc -> Rope.t = fun type_desc ->
  let open RopeUtil in
  match type_desc with
  | `type_reference _ | `literal_type _ | `type_literal _
  | `tuple _ | `array _ | `record _ -> rope_of_ts_type_desc type_desc
  | _ -> rope_of_ts_type_desc type_desc |> between "(" ")"

and rope_of_ts_expression : ts_expression -> Rope.t =
  let open RopeUtil in
  function
  | `identifier id -> rope id
  | `literal_expression lit -> begin match lit with
      | `string_literal s -> between "\"" "\"" (rope s)
    end
  | `call_expression { tsce_expression; tsce_arguments; } ->
    rope_of_ts_expression_with_paren tsce_expression ++
    (tsce_arguments |&> rope_of_ts_expression
     |> comma_separated_list
     |> between "(" ")")
  | `element_access_expression { tsea_expression; tsea_argument; } ->
    rope_of_ts_expression_with_paren tsea_expression
    ++ between "[" "]" (rope_of_ts_expression tsea_argument)
  | `property_access_expression { tspa_expression; tspa_name; } ->
    rope_of_ts_expression_with_paren tspa_expression +@ "." +@ tspa_name
  | `binary_expression { tsbe_left; tsbe_operator_token; tsbe_right; } ->
    rope_of_ts_expression_with_paren tsbe_left
    +@ " " +@ tsbe_operator_token +@ " " ++
    rope_of_ts_expression_with_paren tsbe_right
  | `arrow_function { tsaf_parameters; tsaf_body; } ->
    (tsaf_parameters |&> fun { tsp_name; tsp_type_desc; } ->
        tsp_name @+ " : " @+ rope_of_ts_type_desc tsp_type_desc)
    |> comma_separated_list
    |> between "(" ")"
    |> fun param -> param +@ " => " ++ between "{\n" "\n}" (rope_of_ts_ast tsaf_body)
  | `new_expression { tsne_expression; tsne_arguments; } ->
    "new " @+
    rope_of_ts_expression_with_paren tsne_expression ++
    between "(" ")" (tsne_arguments |&> rope_of_ts_expression |> comma_separated_list)

and rope_of_ts_expression_with_paren : ts_expression -> Rope.t = fun expr ->
  let open RopeUtil in
  match expr with
  | `identifier _ -> rope_of_ts_expression expr
  | _ -> rope_of_ts_expression expr |> between "(" ")"


module Internals = struct
  let rope_of_ts_ast = rope_of_ts_ast
  let rope_of_ts_statement = rope_of_ts_statement
  let rope_of_ts_type_desc = rope_of_ts_type_desc
  let rope_of_ts_expression = rope_of_ts_expression
end

let gen_ts_type : ?export:bool -> type_decl -> string =
  fun ?(export=true) type_decl ->
  let fwrt_decl = annotate_fwrt_decl export false (fwrt_decl_of_type_decl type_decl) in
  let self_type_name = fst fwrt_decl in
  let ts_type_alias_decl = ts_type_alias_decl_of_fwrt_decl ~self_type_name fwrt_decl in
  let rope = rope_of_ts_type_alias_decl ts_type_alias_decl in
  Rope.to_string rope

let gen_ts_case_analyzer : ?export:bool -> ?name:string -> type_decl -> string =
  fun ?(export=true) ?name type_decl ->
  let fwrt_decl = annotate_fwrt_decl export false (fwrt_decl_of_type_decl type_decl) in
  let self_type_name = fst fwrt_decl in
  let ts_func_decl = ts_func_decl_of_fwrt_decl ~self_type_name fwrt_decl in
  let ts_func_decl =
    match name with
    | None -> ts_func_decl
    | Some tsf_name -> { ts_func_decl with tsf_name } in
  let rope = rope_of_ts_func_decl ts_func_decl in
  Rope.to_string rope
