(* Copyright 2022-2023 Kotoi-Xie Consultancy, Inc. This file is a part of the

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
include Bindoj_gen_ts_config
open Bindoj_gen.Json_codec

open Bindoj_typedesc.Type_desc
open Bindoj_gen_foreign.Foreign_datatype

type ts_fwrt_constructor_kind_annot = ts_fwrt_constructor_kind_info option
[@@deriving show, eq]

and ts_fwrt_constructor_kind_info =
  | Tfcki_reused_variant_inline_record of type_decl
[@@deriving show, eq]

let string_of_ts_fwrt_constructor_kind_annot = show_ts_fwrt_constructor_kind_annot
let string_of_ts_fwrt_constructor_kind_info = show_ts_fwrt_constructor_kind_info

let type_of_prim : Coretype.prim -> ts_type_desc = function
  | `unit -> `literal_type (`numeric_literal 1.)
  | `bool -> `type_reference "boolean"
  | `int | `int53p | `float | `byte -> `type_reference "number"
  | `string | `uchar -> `type_reference "string"
  | `bytes -> `type_reference "string" (* base64 *)

let property_type_of_coretype :
      ?definitive:bool
      -> self_json_name:string
      -> Json_config.json_mangling_style
      -> coretype -> [`optional_property of bool]*ts_type_desc =
  fun ?(definitive = false) ~self_json_name base_mangling_style { ct_desc; ct_configs; _ } ->
  let base_mangling_style =
    Json_config.get_mangling_style_opt ct_configs
    |? base_mangling_style
  in
  let rec go =
    let open Coretype in
    function
    | Prim p -> type_of_prim p
    | Uninhabitable -> `type_reference "never"
    | Ident { id_name; _ } ->
      let name =
        ct_configs
        |> Json_config.get_name_opt |? id_name
      in
      `type_reference (Json_config.mangled `type_name base_mangling_style name)
    | Option ((Option _) as t) -> go t
    | Option t -> `union [go t; `type_reference "null"; `type_reference "undefined"]
    | List t -> `array (go t)
    | Tuple ts ->
      begin match Json_config.get_tuple_style ct_configs with
      | `arr -> `tuple (ts |> List.map go)
      | `obj `default ->
        let fields =
          ts |> List.mapi (fun i t ->
            let tsps_optional, tsps_type_desc =
              match go_property t with
              | `optional t -> (true, t)
              | `mandatory t -> (false, t)
            in
            { tsps_modifiers = [];
              tsps_name = Json_config.tuple_index_to_field_name i;
              tsps_optional; tsps_type_desc;
            })
        in
        `type_literal fields
      end
    | Map (k, v) -> `record (go (Coretype.desc_of_map_key k), go v)
    | Self ->
      `type_reference
        (ct_configs
          |> Json_config.get_name_opt
          >? Json_config.mangled `type_name base_mangling_style
          |? self_json_name)
    | StringEnum cs ->
      `union (cs |&> (
          Json_config.get_mangled_name_of_string_enum_case ~inherited:base_mangling_style
          &> (fun c -> `literal_type (`string_literal c))))
  and go_property = function
    | Option t -> `optional (go t)
    | t -> `mandatory (go t)
  in
  let definitive =
    let classify = Coretype.(function
      | Ident _ | Self -> false
      | Prim _ | Uninhabitable
        | Option _ | List _ | Tuple _ | Map _
        | StringEnum _
        -> true
      | _ -> .)
    in
    definitive || (classify ct_desc) in
  (if definitive then
    ct_configs |> Configs.find_foreign_type_expr typescript
    >? (fun t -> `mandatory t)
    |? go_property ct_desc
  else go_property ct_desc)
  |> function
  | `optional t -> (`optional_property true, t)
  | `mandatory t -> (`optional_property false, t)

let type_of_coretype =
  fun ?definitive ~self_json_name base_mangling_style ct ->
    property_type_of_coretype ?definitive ~self_json_name base_mangling_style ct
    |> function
    | `optional_property true, t -> `union [t; `type_reference "null"; `type_reference "undefined"]
    | `optional_property false, t -> t

let get_name_of_fwrt_desc : default:string -> Json_config.json_mangling_style -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_desc -> string * Json_config.json_mangling_style =
  fun ~default base_mangling_style desc ->
    let get_mangled_name kind configs = Json_config.(
      let style = get_mangling_style_opt configs |? base_mangling_style in
      get_name_opt configs |? default
      |> mangled kind style, style
    ) in
    match desc.fd_kind with
    | Fwrt_object { fo_configs; _ } -> get_mangled_name `type_name fo_configs
    | Fwrt_alias { fa_configs; _ } -> get_mangled_name `type_name fa_configs
    | Fwrt_constructor { fc_configs; _ } -> get_mangled_name `discriminator_value fc_configs

let type_of_nested env name : ts_type_desc =
  let codec = FwrtTypeEnv.lookup name env in
  let name, _ = get_name_of_fwrt_desc ~default:name Json_config.default_mangling_style codec in
  match codec.fd_kind with
  | Fwrt_constructor _ -> failwith "Constructor cannot be nested."
  | _ -> `type_reference name

let property_type_of_nested ?definitive ~self_json_name env name : [`optional_property of bool] * ts_type_desc =
  let codec = FwrtTypeEnv.lookup name env in
  let name, mangling_style = get_name_of_fwrt_desc ~default:name Json_config.default_mangling_style codec in
  match codec.fd_kind with
  | Fwrt_constructor _ -> failwith "Constructor cannot be nested."
  | Fwrt_alias { fa_type = { ct_desc = Option (Option _); _}; _ } ->
    failwith "Nested option types cannot be fields."
  | Fwrt_alias { fa_type = ct; _ } when Coretype.is_option ct ->
    property_type_of_coretype ?definitive ~self_json_name mangling_style ct
  | _ -> `optional_property false, `type_reference name

type ('ann_d, 'ann_f, 'ann_va) ts_fwrt_decl = ('ann_d, 'ann_f, 'ann_va, unit*unit*ts_fwrt_constructor_kind_annot) fwrt_decl
type fwrt_decl_of_ts = (ts_modifier list, [`readonly] list, [`readonly] list) ts_fwrt_decl

let ts_fwrt_decl_of_type_decl :
  export:bool
  -> readonly:bool
  -> type_decl
  -> fwrt_decl_of_ts =
  fun ~export ~readonly decl ->
  fwrt_decl_of_type_decl' ~annotator:{
    annotate_decl = (
      if export then (fun _ _ ->  [`export])
      else (fun _ _ -> [])
    );
    annotate_kind_object = (fun ~fields:_ ~children:_ ~configs:_ -> ());
    annotate_kind_alias = (fun ~type_:_ ~configs:_ -> ());
    annotate_kind_constructor = (fun ~param ~configs:_ ->
      match param with
      | `reused_inline_record td ->
        Some (Tfcki_reused_variant_inline_record td)
      | _ -> None
    );
    annotate_field = (
      if readonly then (fun ~name:_ ~type_:_ ~configs:_ -> [`readonly])
      else (fun ~name:_ ~type_:_ ~configs:_ -> [])
    );
    annotate_variant_argument = (
      if readonly then (fun ~type_:_ ~configs:_ -> [`readonly])
      else (fun ~type_:_ ~configs:_ -> [])
    );
  } decl

let add_kind_field kind_field = function
  | `type_literal fields -> `type_literal (kind_field :: fields)
  | `intersection ((`type_literal fields) :: typs) ->
    `intersection (`type_literal (kind_field :: fields) :: typs)
  | `intersection typs -> `intersection (`type_literal [ kind_field ] :: typs)
  | desc -> `intersection [ `type_literal [ kind_field ]; desc]

let rec ts_ast_of_fwrt_decl : fwrt_decl_of_ts -> ts_ast =
  fun fwrt_decl ->
  let type_alias_decl =
    `type_alias_declaration (ts_type_alias_decl_of_fwrt_decl fwrt_decl)
  in
  match FwrtTypeEnv.lookup (fst fwrt_decl) (snd fwrt_decl) with
  | { fd_kind = Fwrt_object { fo_children = _ :: _; _ }; _ } ->
    [ type_alias_decl;
      `function_declaration (ts_func_decl_of_fwrt_decl fwrt_decl) ]
  | _ ->
    [ type_alias_decl ]

and ts_type_alias_decl_of_fwrt_decl' :
  base_mangling_style:Json_config.json_mangling_style
  -> ?parent_configs:[`type_decl] configs
  -> ?self_json_name:string
  -> fwrt_decl_of_ts
  -> ts_type_alias_decl =
  fun ~base_mangling_style ?parent_configs ?self_json_name (name, env) ->
  let { fd_name; fd_kind; fd_annot; _ } as desc = FwrtTypeEnv.lookup name env in
  assert (name = fd_name);
  let (mangled_name, base_mangling_style) = get_name_of_fwrt_desc ~default:name base_mangling_style desc in
  let self_json_name = self_json_name |? mangled_name in
  let ts_props_and_nested_types_of_fields base_mangling_style fields: ts_property_signature ignore_order_list * ts_type_desc ignore_order_list =
    List.fold_right (fun ({ ff_name; ff_type; ff_annot; ff_configs; _}) (members, nested) ->
      let base_mangling_style = Json_config.get_mangling_style_opt ff_configs |? base_mangling_style in
      let nested_style = Json_config.get_nested_field_style ff_configs in
      match nested_style, ff_type with
      | `spreading, `direct _ -> failwith "non-nested argument/field cannot be spread."
      | `spreading, `nested (name, _) ->
        members, type_of_nested env name :: nested
      | `nested, _ ->
        let (`optional_property tsps_optional), tsps_type_desc = match ff_type with
          | `direct ct -> property_type_of_coretype ~self_json_name base_mangling_style ct
          | `nested (name, _) -> property_type_of_nested ~self_json_name env name
        in
        let field_name = Json_config.(
          ff_configs
          |> get_name_opt |? ff_name
          |> mangled `field_name base_mangling_style
        ) in
        let member =
          { tsps_modifiers = ff_annot;
            tsps_name = field_name;
            tsps_optional;
            tsps_type_desc; }
        in
        member :: members, nested
    ) fields ([], [])
  in
  let when_not_empty f = function | [] -> None | xs -> Some (f xs) in
  let desc: ts_type_desc =
    match fd_kind with
    | Fwrt_object { fo_fields; fo_children; fo_configs; fo_annot=() } ->
      let members, nested = ts_props_and_nested_types_of_fields base_mangling_style fo_fields in
      let discriminator_name =
        Json_config.get_mangled_name_of_discriminator_field'
          ~inherited:base_mangling_style fo_configs
      in
      let children: ts_type_desc ignore_order_list =
        fo_children |&> fun child ->
          let { tsa_name = discriminator_value; tsa_type_desc; _; } =
            ts_type_alias_decl_of_fwrt_decl' ~parent_configs:fo_configs ~self_json_name ~base_mangling_style (child, env) in
          let kind_field =
            { tsps_modifiers = [];
              tsps_name = discriminator_name;
              tsps_optional = false;
              tsps_type_desc = `literal_type (`string_literal discriminator_value); } in
          add_kind_field kind_field tsa_type_desc
      in
      ([ when_not_empty (fun x -> `type_literal x) members;
         when_not_empty (fun x -> `union x) children;
      ] @ (nested |&> Option.some))
      |&?> identity
      |> (function | [ t ] -> t | ts -> `intersection ts)
    | Fwrt_alias { fa_type; fa_annot=(); _ } ->
      type_of_coretype ~definitive:true ~self_json_name base_mangling_style fa_type
    | Fwrt_constructor { fc_args; fc_fields; fc_configs; fc_annot } ->
      let parent_configs =
        parent_configs |?! (fun () ->
          failwith "Fwrt_constructor cannot be converted to ts_type_alias_decl at the top level and must be a child of Fwrt_object.")
      in
      let inline_record_style =
        Ts_config.(get_reused_variant_inline_record_style_opt fc_configs |? default_reused_variant_inline_record_style)
      in
      match fc_annot, inline_record_style with
      | Some (Tfcki_reused_variant_inline_record td), `intersection_type ->
        `type_reference (Json_config.get_mangled_name_of_type td |> fst)
      | _ ->
        let base_mangling_style =
          match fc_annot with
          | Some (Tfcki_reused_variant_inline_record { td_configs; _ }) ->
            Json_config.(get_mangling_style_opt td_configs |? default_mangling_style)
          | None -> base_mangling_style
        in
        let arg_name =
          Json_config.get_mangled_name_of_variant_arg'
            ~inherited:base_mangling_style
            parent_configs
            fc_configs
            fd_name
        in
        let members, nested =
          let members, nested =
            ts_props_and_nested_types_of_fields base_mangling_style fc_fields
          in
          let property_type_of_variant_argument, type_of_variant_argument =
            let type_of_variant_argument' type_of_coretype type_of_nested base_mangling_style arg =
              let base_mangling_style = Json_config.get_mangling_style_opt arg.fva_configs |? base_mangling_style in
              match arg.fva_type with
              | `direct ct -> type_of_coretype ?definitive:None ~self_json_name base_mangling_style ct
              | `nested (name, _) -> type_of_nested env name
            in
            type_of_variant_argument' property_type_of_coretype (property_type_of_nested ?definitive:None ~self_json_name),
            type_of_variant_argument' type_of_coretype type_of_nested
          in
          match fc_args, Json_config.get_tuple_style fc_configs with
          | [], _ -> members, nested
          | [arg], _
            when Json_config.get_nested_field_style arg.fva_configs = `spreading ->
              begin match arg.fva_type with
              | `direct _ -> failwith "non-nested argument/field cannot be spread."
              | `nested (name, _) ->
                members, (type_of_nested env name :: nested)
              end
          | [arg], _ ->
            let base_mangling_style = Json_config.get_mangling_style_opt arg.fva_configs |? base_mangling_style in
            let (`optional_property tsps_optional), tsps_type_desc = property_type_of_variant_argument base_mangling_style arg in
            { tsps_modifiers = arg.fva_annot;
              tsps_name = arg_name;
              tsps_optional; tsps_type_desc } :: members, nested
          | args, `arr ->
            let desc =
              `tuple (args |&> type_of_variant_argument base_mangling_style)
            in
            { tsps_modifiers = args |&>> (fun { fva_annot; _ } -> fva_annot);
              tsps_name = arg_name;
              tsps_optional = false;
              tsps_type_desc = desc } :: members, nested
          | args, `obj `default ->
            let fields =
              args |> List.mapi (fun i arg ->
                let (`optional_property tsps_optional), tsps_type_desc = property_type_of_variant_argument base_mangling_style arg in
                { tsps_modifiers = arg.fva_annot;
                  tsps_name = Json_config.tuple_index_to_field_name i;
                  tsps_optional; tsps_type_desc })
            in
            members @ fields, nested
        in
        match members, nested with
        | ps, [] -> `type_literal ps
        | [], [ t ] -> t
        | [], ts -> `intersection ts
        | ps, ts -> `intersection (`type_literal ps :: ts)
  in
  { tsa_modifiers = fd_annot;
    tsa_name = mangled_name;
    tsa_type_parameters = [];
    tsa_type_desc = desc }

and ts_func_decl_of_fwrt_decl : fwrt_decl_of_ts -> ts_func_decl =
  fun (name, env) ->
  let { fd_name; fd_kind; fd_annot; _; } = FwrtTypeEnv.lookup name env in
  assert (name = fd_name);

  match fd_kind with
  | Fwrt_alias _ | Fwrt_constructor _ -> invalid_arg "this fwrt_decl cannot be a parent"
  | Fwrt_object { fo_children; fo_configs; _ } ->
    let base_mangling_style =
      Json_config.(get_mangling_style_opt fo_configs |? default_mangling_style)
    in
    let fd_name = Json_config.get_name_opt fo_configs |? fd_name in
    let self_json_name = Json_config.mangled `type_name base_mangling_style fd_name in
    let name = Json_config.mangled `field_name base_mangling_style ("analyze_" ^ fd_name) in
    let type_param = "__bindoj_ret" in
    let param = "__bindoj_fns" in
    let var_v = "__bindoj_v" in
    let discriminator_name =
      Json_config.get_mangled_name_of_discriminator_field'
        ~inherited:base_mangling_style fo_configs
    in
    let param_type =
      `type_literal (List.sort String.compare fo_children |&> fun child ->
        let { tsa_name = discriminator_value; tsa_type_desc; _ } =
          ts_type_alias_decl_of_fwrt_decl' ~parent_configs:fo_configs ~self_json_name ~base_mangling_style (child, env)
        in
        let kind_field =
          { tsps_modifiers = [];
            tsps_name = discriminator_name;
            tsps_optional = false;
            tsps_type_desc = `literal_type (`string_literal discriminator_value); } in
        let desc = add_kind_field kind_field tsa_type_desc in
        { tsps_modifiers = [];
          tsps_name = discriminator_value;
          tsps_optional = false;
          tsps_type_desc =
            (`func_type
              { tsft_parameters = [{ tsp_name = var_v; tsp_type_desc = desc }];
                tsft_type_desc = `type_reference type_param; }); })
    in
    let var_x = "__bindoj_x" in
    let type_desc =
      `func_type
        { tsft_parameters = [
          { tsp_name = var_x;
            tsp_type_desc = `type_reference self_json_name; }];
          tsft_type_desc = `type_reference type_param; } in
    let body =
      (`return_statement
        (`arrow_function
            { tsaf_parameters =
                [{ tsp_name = var_x;
                  tsp_type_desc = `type_reference self_json_name; }];
              tsaf_body =
                [fo_children |> List.sort String.compare |> List.rev |@>
                (`throw_statement
                    (`new_expression
                      { tsne_expression = `identifier "TypeError";
                        tsne_arguments =
                          [`binary_expression
                              { tsbe_left =
                                  `literal_expression (`string_literal ("panic @"^name^" - unrecognized: "));
                                tsbe_operator_token = "+";
                                tsbe_right = `identifier var_x; }]; }),
                  fun (acc, child) ->
                    let { fd_name; _ } as child_desc = FwrtTypeEnv.lookup child env in
                    let (discriminator_value, _) =
                      child_desc
                      |> get_name_of_fwrt_desc ~default:fd_name base_mangling_style in
                    `if_statement
                      ((`binary_expression
                          { tsbe_left =
                              `property_access_expression
                                { tspa_expression = `identifier var_x;
                                  tspa_name = discriminator_name; };
                            tsbe_operator_token = "===";
                            tsbe_right = `literal_expression (`string_literal discriminator_value); }),
                      (`return_statement
                          (`call_expression
                            { tsce_expression =
                                `element_access_expression
                                  { tsea_expression = `identifier param;
                                    tsea_argument =
                                      `property_access_expression
                                        { tspa_expression = `identifier var_x;
                                          tspa_name = discriminator_name; }; };
                              tsce_arguments = [`identifier var_x]; })),
                      acc))]; })) in
    { tsf_modifiers = fd_annot;
      tsf_name = name;
      tsf_type_parameters = [type_param];
      tsf_parameters = [{ tsp_name = param; tsp_type_desc = param_type; }];
      tsf_type_desc = type_desc;
      tsf_body = [body]; }

and ts_type_alias_decl_of_fwrt_decl : ?self_json_name:string -> fwrt_decl_of_ts -> ts_type_alias_decl =
  fun ?self_json_name fwrt_decl ->
  ts_type_alias_decl_of_fwrt_decl'
    ~base_mangling_style:Json_config.default_mangling_style
    ?self_json_name
    fwrt_decl

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
  let between_double_quotes = between "\"" "\""

  let roprintf fmt = Format.ksprintf Rope.of_string fmt
  (** sprintf のように rope を作れる *)

  let concat = Rope.concat
  let concat_str sep xs = Rope.concat (rope sep) xs
  let comma_separated_list xs = Rope.concat (rope ", ") xs
  let comma_newline_separated_list xs = Rope.concat (rope ",\n") xs
end

let valid_ascii_js_identifier s =
  let at = String.get s in
  let len = String.length s in
  let rec loop n =
    if n >= len then true
    else match at n with
      | '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
          loop (succ n)
      | _ -> false
  in
  if len = 0 then false
  else match at 0 with
    | '_' | 'a' .. 'z' | 'A' .. 'Z' ->
        loop 1
    | _ -> false

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
  | `value_declaration value_decl ->
    rope_of_ts_value_decl value_decl
  | `module_declaration ts_mod_decl ->
    rope_of_ts_mod_decl ts_mod_decl
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
  let modifiers = rope_of_modifiers (tsa_modifiers :> ts_modifier list) in
  let name = rope tsa_name in
  let type_parameters =
    tsa_type_parameters |&> rope
    |> comma_separated_list in
  let type_desc = rope_of_ts_type_desc tsa_type_desc in
  modifiers +@ "type " ++ name +@ " = " ++ type_parameters ++ type_desc

and rope_of_ts_func_decl : ts_func_decl -> Rope.t =
  fun { tsf_modifiers; tsf_name; tsf_type_parameters; tsf_parameters; tsf_type_desc; tsf_body; } ->
  let open RopeUtil in
  let modifiers = rope_of_modifiers (tsf_modifiers :> ts_modifier list) in
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
  modifiers +@ "function " ++ name ++ type_parameters ++ parameters ++ (" : " @+ type_desc +@ "\n") ++ body

and rope_of_ts_value_decl : ts_value_decl -> Rope.t =
  fun { tsv_modifiers; tsv_kind; tsv_name; tsv_type_desc; tsv_value } ->
  let open RopeUtil in
  let modifiers = rope_of_modifiers (tsv_modifiers :> ts_modifier list) in
  let kind =
    (match tsv_kind with
    | `const -> "const "
    | `let_ -> "let "
    ) |> rope in
  let name = rope tsv_name in
  let type_desc =
    match tsv_type_desc >? rope_of_ts_type_desc with
    | None -> rope ""
    | Some r -> rope ": " ++ r in
  let body = rope_of_ts_expression tsv_value in
  modifiers ++ kind ++ name ++ type_desc +@ " = " ++ body

and rope_of_ts_mod_decl : ts_mod_decl -> Rope.t =
  fun { tsm_modifiers; tsm_name; tsm_body; } ->
  let open RopeUtil in
  let modifiers = rope_of_modifiers (tsm_modifiers :> ts_modifier list) in
  let name = rope tsm_name in
  let body = rope_of_ts_ast tsm_body |> between " {\n" "\n}" in
  modifiers +@ "namespace " ++ name ++ body

and rope_of_modifiers : ts_modifier list -> Rope.t = fun modifiers ->
  let open RopeUtil in
  (if List.mem `export modifiers then rope "export " else rope "") ++
  (if List.mem `async modifiers then rope "async " else rope "") ++
  (if List.mem `readonly modifiers then rope "readonly " else rope "")

and rope_of_ts_type_desc : ts_type_desc -> Rope.t =
  let open RopeUtil in
  let kw_of_special = function
    | `void -> "void"
    | `undefined -> "undefined"
    | `null -> "null"
    | `any -> "any"
    | `unknown -> "unknown"
    | `never -> "never"
  in
  function
  | `special s -> rope (kw_of_special s)
  | `type_reference s ->
    rope s
  | `type_construct (constructor, arguments) ->
     (rope constructor)
     ++
     (arguments |&> rope_of_ts_type_desc_with_paren
      |> concat_str ", "
      |> between "<" ">")
  | `type_literal members ->
    (members |&> fun { tsps_modifiers; tsps_name; tsps_optional; tsps_type_desc; } ->
        let readonly =
          if List.exists (( = ) `readonly) tsps_modifiers then
            rope "readonly "
          else
            rope "" in
        let name =
          (if valid_ascii_js_identifier tsps_name then
            tsps_name
          else
            sprintf "\"%s\"" tsps_name)
        in
        let optional = if tsps_optional then "?" else "" in
        let type_desc = rope_of_ts_type_desc tsps_type_desc in
        readonly +@ name +@ optional +@ " : " ++ type_desc)
    |> comma_separated_list
    |> between "{ " " }"
  | `literal_type (`numeric_literal f) ->
    rope (string_of_float f)
  | `literal_type (`string_literal s) ->
    between "\"" "\"" (rope s)
  | `literal_type (`template_literal s) ->
    between "`" "`" (rope s)
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
  | `typeof expr ->
     (rope "typeof ")
     ++
     (rope_of_ts_expression_with_paren expr)
  | `keyof td ->
     (rope "keyof ")
     ++
     (rope_of_ts_type_desc_with_paren td)
  | `type_assertion (body, asserted) -> (
     (rope_of_ts_type_desc_with_paren body)
     +@ " as "
     ++ (rope_of_ts_type_desc_with_paren asserted))
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
      | `numeric_literal f -> rope (string_of_float f)
      | `string_literal s -> between "\"" "\"" (rope s)
      | `template_literal s -> between "`" "`" (rope s)
      | `object_literal fs ->
         (fs |&> fun (prop, value) ->
           (between_double_quotes % rope) prop
           +@ ": " ++ (rope_of_ts_expression_with_paren value)
         )
         |> comma_newline_separated_list
         |> between "{\n" "\n}"
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
    if valid_ascii_js_identifier tspa_name then
      rope_of_ts_expression_with_paren tspa_expression
      +@ "." +@ tspa_name
    else
      rope_of_ts_expression_with_paren tspa_expression
      ++ between "[\"" "\"]" (rope tspa_name)
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
  | `await_expression expr ->
    "await " @+ rope_of_ts_expression expr
  | `casted_expression (expr, td) ->
     rope_of_ts_expression_with_paren expr
     +@ " as " ++ (rope_of_ts_type_desc_with_paren td)
  | `const_assertion expr ->
     (rope_of_ts_expression_with_paren expr)
     +@ " as const"

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
  let fwrt_decl =
    type_decl
    |> ts_fwrt_decl_of_type_decl ~export ~readonly:false
  in
  let ts_type_alias_decl = ts_type_alias_decl_of_fwrt_decl fwrt_decl in
  let rope = rope_of_ts_type_alias_decl ts_type_alias_decl in
  Rope.to_string rope

let gen_ts_case_analyzer : ?export:bool -> ?name:string -> type_decl -> string =
  fun ?(export=true) ?name type_decl ->
  let fwrt_decl =
    type_decl
    |> ts_fwrt_decl_of_type_decl ~export ~readonly:false
  in
  let ts_func_decl = ts_func_decl_of_fwrt_decl fwrt_decl in
  let ts_func_decl =
    match name with
    | None -> ts_func_decl
    | Some tsf_name -> { ts_func_decl with tsf_name } in
  let rope = rope_of_ts_func_decl ts_func_decl in
  Rope.to_string rope
