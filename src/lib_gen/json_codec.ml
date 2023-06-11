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
open Ppxlib
open Ast_builder.Default
open Ast_helper
open Utils
open Bindoj_runtime
open Bindoj_base
open Bindoj_base.Type_desc
open Bindoj_codec

include Bindoj_codec.Json.Config

type json_schema
type ('tag, 'datatype_expr) foreign_language +=
  | Foreign_language_JSON_Schema :
    (json_schema, Bindoj_openapi.V3.Schema_object.t) foreign_language
let json_schema = Foreign_language_JSON_Schema

module Json_config = struct
  include Bindoj_codec.Json.Config.Json_config

  let custom_json_schema schema =
    Configs.Config_foreign_type_expression (json_schema, schema)
end

let get_encoder_name type_name = function
  | `default -> type_name^"_to_json"
  | `open_ m -> sprintf "%s.%s_to_json" m type_name
  | `in_module m -> m^".to_json"
  (* | `codec_val v -> v *)
let get_decoder_name type_name = function
  | `default -> type_name^"_of_json'"
  | `open_ m -> sprintf "%s.%s_of_json'" m type_name
  | `in_module m -> m^".of_json'"
  (* | `codec_val v -> v *)

let get_json_shape_explanation_name type_name = function
  | `default -> type_name^"_json_shape_explanation"
  | `open_ m -> sprintf "%s.%s_json_shape_explanation" m type_name
  | `in_module m -> m^".json_shape_explanation"

type builtin_codec = {
  encoder: expression;
  decoder: expression;
  (* validator stuffs are meant to go here *)
}

module Builtin_codecs = struct
  open struct
    let loc = Location.none
    let error_type_mismatch cty_name  =
      let format_string = sprintf "expecting type '%s' but the given is of type '%%s'" cty_name in
      [%expr
      Error (
        Printf.sprintf [%e estring ~loc format_string]
          Kxclib.Json.(string_of_jv_kind(classify_jv jv)), path)]
  end

  let unit = {
      encoder = [%expr fun () -> (`num 1. : Kxclib.Json.jv)];
      decoder = [%expr fun path -> function
        | (`bool _ | `num _ | `str _ | `arr [] | `obj []) -> Ok ()
        | jv -> [%e error_type_mismatch "unit"]
      ];
    }
  let bool = {
      encoder = [%expr fun (x : bool) -> (`bool x : Kxclib.Json.jv)];
      decoder = [%expr fun path -> function
        | (`bool x : Kxclib.Json.jv) -> Ok x
        | jv -> [%e error_type_mismatch "bool"]
      ];
    }
  let int = {
      encoder = [%expr fun (x : int) -> (`num (float_of_int x) : Kxclib.Json.jv)];
      decoder = [%expr fun path -> function
        | (`num x : Kxclib.Json.jv) ->
          if Float.is_integer x then Ok (int_of_float x)
          else Error (Printf.sprintf "expecting an integer but the given is '%f'" x, path)
        | jv -> [%e error_type_mismatch "int"]
      ];
    }
  let int53p = {
      encoder = [%expr fun (x : Kxclib.int53p) -> (`num (Kxclib.Int53p.to_float x) : Kxclib.Json.jv)];
      decoder = [%expr fun path -> function
        | (`num x : Kxclib.Json.jv) -> Ok (Kxclib.Int53p.of_float x)
        | jv -> [%e error_type_mismatch "int53p"]
      ];
    }
  let float = {
      encoder = [%expr fun (x : float) -> (`num x : Kxclib.Json.jv)];
      decoder = [%expr fun path -> function
        | (`num x : Kxclib.Json.jv) -> Ok x
        | jv -> [%e error_type_mismatch "float"]
      ];
    }
  let string = {
      encoder = [%expr fun (x : string) -> (`str x : Kxclib.Json.jv)];
      decoder = [%expr fun path -> function
        | (`str x : Kxclib.Json.jv) -> Ok x
        | jv -> [%e error_type_mismatch "string"]
      ];
    }
  let uchar = {
      encoder = [%expr fun (x: Uchar.t) -> (`str (String.of_seq (List.to_seq [Uchar.to_char x])) : Kxclib.Json.jv)];
      decoder = [%expr fun path -> function
        | (`str x : Kxclib.Json.jv) ->
            if String.length x = 1 then Ok (Uchar.of_char (String.get x 0))
            else Error (Printf.sprintf "string '%s' is not a valid uchar value" x, path)
        | jv -> [%e error_type_mismatch "uchar"]
      ];
    }
  let byte = {
      encoder = [%expr fun (x: char) -> (`num (float_of_int (int_of_char x)) : Kxclib.Json.jv)];
      decoder = [%expr fun path -> function
        | (`num x : Kxclib.Json.jv) ->
            let x = int_of_float x in
            if 0 <= x && x <= 255 then Ok (char_of_int x)
            else Error (Printf.sprintf "number '%d' is not a valid byte value" x, path)
        | jv -> [%e error_type_mismatch "byte"]
      ];
    }
  let bytes = {
      encoder = [%expr fun (x : Bytes.t) -> (`str (Kxclib.Base64.encode x) : Kxclib.Json.jv)];
      decoder = [%expr fun path -> function
        | (`str x : Kxclib.Json.jv) ->
          (try Ok (Kxclib.Base64.decode x) with Invalid_argument msg -> Error (msg, path))
        | jv -> [%e error_type_mismatch "bytes"]
      ];
    }
  let option = {
      encoder = [%expr fun t_to_json -> function
        | Some x -> t_to_json x
        | None -> (`null : Kxclib.Json.jv)
      ];
      decoder = [%expr fun t_of_json path -> function
        | `null -> Ok None
        | x ->
          match t_of_json path x with
          | Ok x -> Ok (Some x)
          | Error msg -> Error msg
      ];
    }
  let list = {
      encoder = [%expr fun t_to_json xs -> (`arr (List.map t_to_json xs) : Kxclib.Json.jv)];
      decoder = [%expr fun t_of_json path -> function
        | (`arr xs : Kxclib.Json.jv) ->
          let open Kxclib.MonadOps(Kxclib.ResultOf(struct type err = string * Kxclib.Json.jvpath end)) in
          xs |> List.mapi (fun i -> t_of_json (`i i :: path)) |> sequence_list
        | jv -> [%e error_type_mismatch "list"]
      ];
    }
  let uninhabitable = {
      encoder = [%expr fun () -> (`null : Kxclib.Json.jv)];
      decoder = [%expr fun path -> function
        | `null -> Ok ()
        | jv -> [%e error_type_mismatch "**uninhabitable**"]];
    }
  let map = {
      encoder = [%expr fun key_to_string v_to_json fields ->
        let fields =
          fields |> List.map (fun (k, v) -> key_to_string k, v_to_json v)
        in
        (`obj fields : Kxclib.Json.jv)
      ];
      decoder = [%expr fun key_of_string v_of_json path -> function
        | `obj fields ->
          let open Kxclib.MonadOps(Kxclib.ResultOf(struct type err = string * Kxclib.Json.jvpath end)) in
          fields
          |> List.map (fun (k, v) ->
            match key_of_string k, v_of_json (`f k :: path) v with
            | Some k, Ok v -> Ok (k, v)
            | None, _ -> Error (Printf.sprintf "'key_of_string %s' failed" k, path)
            | _, Error x -> Error x
          )
          |> sequence_list
        | jv -> [%e error_type_mismatch "map"]
      ];
    }

  let all = [
      "unit", unit;
      "bool", bool;
      "int", int;
      "int53p", int53p;
      "float", float;
      "string", string;
      "uchar", uchar;
      "byte", byte;
      "bytes", bytes;
      "option", option;
      "list", list;
      "uninhabitable", uninhabitable;
      "map", map;
    ]
end

let builtin_codecs = Builtin_codecs.all

let builtin_codecs_map =
  builtin_codecs |> List.to_seq |> StringMap.of_seq

let codec_of_coretype ~get_custom_codec ~get_name ~map_key_converter ~tuple_case ~string_enum_case ~wrap_ident self_ename (ct: coretype) =
  let loc = Location.none in
  match get_custom_codec ct.ct_configs with
  | Some coder -> evar coder
  | None ->
    let evar_name ?(codec = `default) name =
      get_name name codec |> evar
    in
    let rec go = function
      | Coretype.Prim p -> evar_name (Coretype.string_of_prim p)
      | Uninhabitable -> evar_name "uninhabitable"
      | Ident i -> evar_name ~codec:i.id_codec i.id_name |> wrap_ident
      | Option t -> [%expr [%e evar_name "option"] [%e go t]] (* option_of_json t_of_json *)
      | List t -> [%expr [%e evar_name "list"] [%e go t]] (* list_of_json t_of_json *)
      | Map (k, v) -> [%expr [%e evar_name "map"] [%e map_key_converter k] [%e go v]] (* map_of_json key_of_string t_of_json *)
      | Tuple ts -> tuple_case ct.ct_configs go ts
      | StringEnum cs -> string_enum_case cs
      | Self -> self_ename
    in
    go ct.ct_desc

let collect_builtin_codecs (td: type_decl) =
  let folder state (ct: coretype) =
    Coretype.fold (fun state ->
      let add name = state |> StringMap.add name (builtin_codecs_map |> StringMap.find name) in
      function
      | Prim p -> add (Coretype.string_of_prim p)
      | Uninhabitable -> add "uninhabitable"
      | Option _ -> add "option"
      | List _ -> add "list"
      | Map _ -> add "map"
      | Ident _ | Self | Tuple _ | StringEnum _ -> state
    ) state ct.ct_desc
  in
  fold_coretypes folder StringMap.empty td

let bind_results : (pattern * expression) list -> expression -> expression = fun bindings body ->
  let loc = Location.none in
  [%expr
    let (>>=) = Result.bind in
    [%e List.fold_right (fun (p, e) body ->
          [%expr [%e e] >>= (fun [%p p] -> [%e body])])
        bindings body]]

let opt_to_result : expression -> expression =
  let loc = Location.none in
  fun err -> [%expr function | Some a -> Ok a | None -> Error [%e err]]

let encoder_of_coretype =
  let open Coretype in
  let vari i = "x"^string_of_int i in
  let loc = Location.none in
  let evari i = evar (vari i) in
  let pvari i = pvar (vari i) in
  let tuple_case (configs: [`coretype] configs) (go: desc -> expression) (ts: desc list) =
    let args =
      ts |> List.mapi (fun i _ -> pvari i)
         |> Pat.tuple
    in
    let rec mk_list acc = function
      | [] -> acc
      | x :: xs -> mk_list [%expr [%e x] :: [%e acc]] xs
    in
    let ret =
      let style = Json_config.get_tuple_style configs in
      ts
      |> List.mapi (fun i t ->
        match style with
        | `obj `default ->
          let label = estring ~loc (tuple_index_to_field_name i) in
          [%expr ([%e label], [%e go t] [%e evari i])]
        | `arr -> [%expr [%e go t] [%e evari i]])
      |> List.rev |> mk_list [%expr []]
      |> (fun ret ->
        match style with
        | `obj `default -> [%expr `obj [%e ret]]
        | `arr -> [%expr `arr [%e ret]])
    in
    [%expr fun [%p args] -> ([%e ret] : Kxclib.Json.jv)]
  in
  let map_key_converter (k: map_key) = (* key_to_string *)
    match k with
    | `string -> [%expr fun (k: string) -> k]
  in
  let string_enum_case ct_configs (cs: Coretype.string_enum_case list) =
    let cases =
      cs |> List.map (fun ((name, _, _) as c) ->
        let json_name = Json.Json_config.get_mangled_name_of_string_enum_case ct_configs c in
        let pat = Pat.variant (Utils.escape_as_constructor_name name) None in
        let expr = [%expr `str [%e Exp.constant (Const.string json_name)]] in
        Exp.case pat expr
      )
    in
    Exp.function_ cases
  in
  let wrap_ident = identity in
  fun e ct ->
    codec_of_coretype
      ~get_custom_codec:Json_config.get_custom_encoder
      ~get_name:get_encoder_name
      ~tuple_case ~map_key_converter ~string_enum_case:(string_enum_case ct.ct_configs)
      ~wrap_ident
      e ct

let decoder_of_coretype =
  let open Coretype in
  let vari i = "x"^string_of_int i in
  let loc = Location.none in
  let evari i = evar (vari i) in
  let pvari i = pvar (vari i) in
  let tuple_case (configs: [`coretype] configs) (go: desc -> expression) (ts: desc list) =
    let rec mk_list acc = function
      | [] -> acc
      | x :: xs -> mk_list [%pat? [%p x] :: [%p acc]] xs
    in
    let ret =
      let bindings =
        ts |> List.mapi (fun i t ->
            [%pat? [%p pvari i]], [%expr [%e go t] (`i [%e eint ~loc i] :: path) [%e evari i]]
          )
      in
      let ret =
        ts |> List.mapi (fun i _ -> [%expr [%e evari i]]) |> Exp.tuple
      in
      bind_results bindings [%expr Ok [%e ret]]
    in
    match Json_config.get_tuple_style configs with
    | `arr ->
      let args =
        ts |> List.mapi (fun i _ -> pvari i)
           |> List.rev |> mk_list [%pat? []]
      in
      let tuple_length_error_message =
        sprintf "expecting a tuple of length %d, but the given has a length of %%d" (List.length ts)
      in
      [%expr fun path -> function
        | (`arr [%p args] : Kxclib.Json.jv) -> [%e ret]
        | `arr xs ->
          Error (
            Printf.sprintf
              [%e estring ~loc tuple_length_error_message]
              (List.length xs),
            path)
        | jv ->
          Error (
            Printf.sprintf
              "an array is expected for a tuple value, but the given is of type '%s'"
              Kxclib.Json.(string_of_jv_kind (classify_jv jv)),
            path)
      ]
    | `obj `default ->
      let body =
        ts
        |> List.mapi (fun i _ -> i)
        |> List.foldr (fun i ret ->
          let label_name = tuple_index_to_field_name i in
          let label = estring ~loc label_name in
          let error_message = sprintf "mandatory field '%s' does not exist" label_name in
          [%expr
            Bindoj_runtime.StringMap.find_opt [%e label] fields
            |> [%e opt_to_result [%expr ([%e estring ~loc error_message], path)]]
            >>= fun [%p pvari i] -> [%e ret]]) ret
      in
      [%expr fun path -> function
        | (`obj fields : Kxclib.Json.jv) ->
          let fields = Bindoj_runtime.StringMap.of_list fields in
          [%e body]
        | jv ->
          Error (
            Printf.sprintf
              "an object is expected for a tuple value, but the given is of type '%s'"
              Kxclib.Json.(string_of_jv_kind (classify_jv jv)),
            path)]
  in
  let map_key_converter (k: map_key) = (* key_of_string *)
    match k with
    | `string -> [%expr fun (s: string) -> Some s]
  in
  let string_enum_case ct_configs (cs: Coretype.string_enum_case list) =
    let cases =
      cs |> List.map (fun ((name, _, _) as c) ->
        let json_name = Json.Json_config.get_mangled_name_of_string_enum_case ct_configs c in
        let pat = Pat.constant (Const.string json_name) in
        let expr = Exp.variant (Utils.escape_as_constructor_name name) None in
        Exp.case pat [%expr Ok [%e expr]]
      ) |> fun cases ->
        let error_message =
          (cs |&> (Json.Json_config.get_mangled_name_of_string_enum_case ct_configs &> sprintf "'%s'")
          |> String.concat ", ")
          |> sprintf "given string '%%s' is not one of [ %s ]"
        in
        cases @ [
          Exp.case [%pat? s] [%expr Error (Printf.sprintf [%e estring ~loc error_message] s, path)]
        ]
    in
    [%expr fun path -> function
      | `str s -> [%e Exp.function_ cases] s
      | jv ->
        Error (
          Printf.sprintf
            "expecting type 'string' but the given is of type '%s'"
            Kxclib.Json.(string_of_jv_kind (classify_jv jv)),
          path)
    ]
  in
  let wrap_ident e =
    [%expr (fun path x ->
        [%e e] ~path x |> Result.map_error (fun (msg, path, _) -> (msg, path)))]
  in
  fun e ct ->
  codec_of_coretype
    ~get_custom_codec:Json_config.get_custom_decoder
    ~get_name:get_decoder_name
    ~tuple_case ~map_key_converter ~string_enum_case:(string_enum_case ct.ct_configs)
    ~wrap_ident
    e ct

let gen_builtin_codecs ?attrs ~get_name ~get_codec (td: type_decl) =
  let loc = Location.none in
  let coders = collect_builtin_codecs td in
  let bind str expr = Vb.mk ~loc ?attrs (Pat.var (strloc str)) expr in
  StringMap.fold (fun label coder state ->
    bind (get_name label `default) (get_codec coder) :: state
  ) coders []

let gen_builtin_encoders : ?attrs:attrs -> type_decl -> value_binding list =
  gen_builtin_codecs ~get_name:get_encoder_name ~get_codec:(fun x -> x.encoder)

let gen_builtin_decoders : ?attrs:attrs -> type_decl -> value_binding list =
  gen_builtin_codecs ~get_name:get_decoder_name ~get_codec:(fun x -> x.decoder)

type json_shape_explanation_resolution =
  string -> [
    | `no_resolution
    | `default
    | `open_ of string
    | `in_module of string ]

let rec ejson_shape_explanation ~loc (shape: json_shape_explanation) =
  match shape with
  | `self -> [%expr `self]
  | `named (name, shape) -> [%expr `named([%e estring ~loc name], [%e ejson_shape_explanation ~loc shape])]
  | `special (name, shape) -> [%expr `special([%e estring ~loc name], [%e ejson_shape_explanation ~loc shape])]
  | `with_warning (name, shape) -> [%expr `with_warning([%e estring ~loc name], [%e ejson_shape_explanation ~loc shape])]
  | `exactly jv -> [%expr `exactly([%e ejv ~loc jv])]
  | `any_json_value -> [%expr `any_json_value]
  | `unresolved s -> [%expr `unresolved [%e estring ~loc s]]
  | `anyone_of shapes -> [%expr `anyone_of [%e shapes |&> (ejson_shape_explanation ~loc) |> elist ~loc]]
  | `string_enum shapes -> [%expr `string_enum [%e shapes |&> (estring ~loc) |> elist ~loc]]
  | `nullable shape -> [%expr `nullable([%e ejson_shape_explanation ~loc shape])]
  | `boolean -> [%expr `boolean]
  | `numeric -> [%expr `numeric]
  | `integral -> [%expr `integral]
  | `proper_int53p -> [%expr `proper_int53p]
  | `proper_float -> [%expr `proper_float]
  | `string -> [%expr `string]
  | `base64str -> [%expr `base64str]
  | `array_of shape -> [%expr `array_of [%e ejson_shape_explanation ~loc shape]]
  | `tuple_of shapes -> [%expr `tuple_of [%e shapes |&> (ejson_shape_explanation ~loc) |> elist ~loc]]
  | `record_of shape -> [%expr `record_of [%e ejson_shape_explanation ~loc shape]]
  | `object_of fields -> [%expr `object_of [%e fields |&> (efield_shape_explanation ~loc) |> elist ~loc]]
and efield_shape_explanation ~loc (field: json_field_shape_explanation) =
  match field with
  | `mandatory_field (s, shape) -> [%expr `mandatory_field([%e estring ~loc s], [%e ejson_shape_explanation ~loc shape])]
  | `optional_field (s, shape) -> [%expr `optional_field([%e estring ~loc s], [%e ejson_shape_explanation ~loc shape])]
and ejv ~loc (jv: Kxclib.Json.jv) =
  match jv with
  | `null -> [%expr `null]
  | `bool x -> [%expr `bool [%e ebool ~loc x]]
  | `num x -> [%expr `num [%e pexp_constant ~loc (Pconst_float (Float.to_string x, None))]]
  | `str x -> [%expr `str [%e estring ~loc x ]]
  | `arr xs -> [%expr `arr [%e xs |&> (ejv ~loc) |> elist ~loc ]]
  | `obj xs -> [%expr
    `obj [%e xs
      |&> (fun (s, jv) ->
        [%expr ([%e estring ~loc s], [%e ejv ~loc jv])])
      |> elist ~loc ]]

let explain_encoded_json_shape :
  ?json_shape_explanation_resolution:json_shape_explanation_resolution
  -> type_decl
  -> expression =
  fun ?json_shape_explanation_resolution td ->
  let loc = Location.none in
  let json_shape_explanation_resolution =
    json_shape_explanation_resolution |? (constant `default)
  in
  let rec process_td td : expression =
    let json_type_name = Json_config.get_mangled_name_of_type td in
    [%expr `named ([%e estring ~loc json_type_name], [%e process_kind td])]
  and process_kind { td_kind; td_configs; _ } : expression =
    match td_kind with
    | Alias_decl ct -> [%expr [%e process_coretype' td_configs ct ]]
    | Record_decl fields ->
       [%expr `object_of [%e fields |&> process_field td_configs |> elist ~loc]]
    | Variant_decl branches ->
      [%expr `anyone_of [%e
        branches |&> (fun ctor ->
            let { vc_name; vc_param; vc_configs; _ } = ctor in
            let discriminator_fname =
              Json_config.get_variant_discriminator td_configs
              |> Json_config.mangled `field_name td_configs
            in
            let discriminator_value = Json_config.get_mangled_name_of_discriminator td_configs ctor in
            match vc_param with
            | `no_param ->
              process_branch discriminator_value discriminator_fname []
            | `tuple_like cts ->
              let arg_fname =
                Json_config.(get_name_of_variant_arg default_name_of_variant_arg) vc_configs
                |> Json_config.mangled `field_name vc_configs
              in
              let arg_shape = [%expr `tuple_of [%e cts |&> process_coretype' td_configs |> elist ~loc]] in
              process_branch discriminator_value discriminator_fname [
                [%expr `mandatory_field ([%e estring ~loc arg_fname], [%e arg_shape])]]
            | `inline_record fields ->
              process_branch discriminator_value discriminator_fname
                (fields |&> process_field td_configs)
            | `reused_inline_record decl ->
              let fields = decl.td_kind |> function
                | Record_decl fields -> fields
                | _ -> failwith' "panic - type decl of reused inline record '%s' must be record decl." vc_name
              in
              process_branch discriminator_value discriminator_fname
                (fields |&> process_field td_configs))
        |> elist ~loc ]]
  and process_field td_configs ({ rf_type; _ } as field): expression =
    let optional, desc =
      rf_type.ct_desc
      |> (function Option desc -> true, desc | desc -> false, desc)
    in
    let json_field_name =
      Json_config.get_mangled_name_of_field td_configs field
      |> estring ~loc
    in
    let inner = process_coretype td_configs rf_type.ct_configs desc in
    match optional with
    | true -> [%expr `optional_field ([%e json_field_name], [%e inner])]
    | false -> [%expr `mandatory_field ([%e json_field_name], [%e inner])]
  and process_branch kind_name discriminator_field_name proper_fields =
    (* asuming flat_kind for now *)
    let kind_field = [%expr
      `mandatory_field (
          [%e estring ~loc discriminator_field_name],
          `exactly (`str [%e estring ~loc kind_name]))] in
    [%expr `object_of ([%e kind_field] :: [%e proper_fields |> elist ~loc])]
  and process_coretype' td_configs ({ ct_desc; ct_configs }: Coretype.t) : expression = process_coretype td_configs ct_configs ct_desc
  and process_coretype td_configs (configs: [`coretype] configs) (desc: Coretype.desc) : expression = match desc with
    | Prim `unit -> [%expr `special ("unit", `exactly `null)]
    | Prim `bool -> [%expr `boolean]
    | Prim `int -> [%expr `integral]
    | Prim `int53p -> [%expr `proper_int53p]
    | Prim `float -> [%expr `proper_float]
    | Prim `string -> [%expr `string]
    | Prim `uchar -> [%expr `special ("uchar", `string)]
    | Prim `byte -> [%expr `special ("byte", `string)]
    | Prim `bytes -> [%expr `base64str]
    | Uninhabitable -> [%expr `special ("uninhabitable", `exactly `null)]
    | Ident { id_name=ident; id_codec; _ } ->
      let ident_json_name =
        configs
        |> Json.Json_config.get_name_opt |? ident
        |> Json.Json_config.mangled `type_name configs
      in
      Json.Json_config.get_custom_shape_explanation configs
      >? (fun shape -> `named (ident_json_name, shape) |> ejson_shape_explanation ~loc)
      |?! (fun () ->
        let trim eshape = [%expr
          match [%e eshape] with
          | `with_warning (_, (`named _ as s)) -> s
          | `with_warning (_, s) | s -> `named ([%e estring ~loc ident_json_name], s)
        ] in
        begin match json_shape_explanation_resolution ident with
        | `no_resolution -> [%expr `unresolved [%e estring ~loc ("alias: "^ident)]]
        | `default ->
          let json_shape_explanation_name = get_json_shape_explanation_name ident id_codec in
          evar json_shape_explanation_name |> trim
        | (`in_module _  | `open_ _) as resolution ->
          let json_shape_explanation_name = get_json_shape_explanation_name ident resolution in
          evar json_shape_explanation_name |> trim
        end)
    | Option d -> [%expr `nullable [%e process_coretype td_configs configs d]]
    | Tuple ds -> [%expr `tuple_of [%e ds |&> process_coretype td_configs configs |> elist ~loc]]
    | List desc -> [%expr `array_of [%e process_coretype td_configs configs desc]]
    | Map (`string, d) -> [%expr `record_of [%e process_coretype td_configs configs d]]
    | StringEnum xs -> [%expr
      `string_enum [%e
        xs
        |&> (Json.Json_config.get_mangled_name_of_string_enum_case configs &> estring ~loc)
        |> elist ~loc
      ]]
    | Self -> [%expr `self]
    | _ -> .
  in
  [%expr `with_warning ("not considering any config if exists", [%e process_td td])]

let name_with_codec : ?codec:Coretype.codec -> string -> string -> string =
  fun ?(codec=`default) name suffix ->
  match codec with
  | `default -> sprintf "%s_%s" name suffix
  | `open_ m -> sprintf "%s.%s_%s" m name suffix
  | `in_module _ -> suffix

let json_encoder_name ?(codec=`default) td =
  name_with_codec ~codec td.td_name "to_json"

let json_decoder_name ?(codec=`default) td =
  name_with_codec ~codec td.td_name "of_json"

let json_shape_explanation_name ?(codec=`default) td =
  name_with_codec ~codec td.td_name "json_shape_explanation"

let gen_discriminator_value_accessor_name ?(codec=`default) td =
  name_with_codec ~codec td.td_name "json_discriminator_value"

let gen_json_encoder :
      ?self_contained:bool
      -> ?codec:Coretype.codec
      -> type_decl
      -> value_binding =
  fun ?(self_contained=false) ?(codec=`default) td ->
  let { td_name; td_kind=kind; td_configs; _ } = td in
  let loc = Location.none in
  let self_name = json_encoder_name ~codec td in
  let self_pname = pvar self_name in
  let self_ename = evar self_name in
  let vari i = "x"^string_of_int i in
  let evari i = evar ~loc (vari i) in
  let pvari i = pvar ~loc (vari i) in
  let wrap_self_contained e =
    if self_contained then
      match gen_builtin_encoders td with
      | [] -> e
      | es ->
         pexp_let ~loc Nonrecursive
           es e
    else e
  in
  let record_params : record_field list -> pattern = fun fields ->
    ppat_record ~loc
      (List.mapi (fun i { rf_name; _; } ->
           (lidloc ~loc rf_name, pvari i))
         fields)
      Closed
  in
  let member_of_field : int -> record_field -> expression =
    fun i field ->
    let json_field_name = Json_config.get_mangled_name_of_field td_configs field in
    [%expr ([%e estring ~loc json_field_name],
            [%e encoder_of_coretype self_ename field.rf_type] [%e evari i])]
  in
  let record_body : record_field list -> expression = fun fields ->
    let members = List.mapi member_of_field fields in
    [%expr `obj [%e elist ~loc members]]
  in
  let variant_params : variant_constructor list -> pattern list = fun constrs ->
    constrs |&> fun { vc_name; vc_param; _ } ->
      let of_record_fields ~label fields =
        match Caml_config.get_variant_type td_configs with
        | `regular ->
          ppat_construct ~loc
            (lidloc ~loc vc_name)
            (Some (record_params fields))
        | `polymorphic ->
          failwith' "case '%s' with an %s cannot be used in a polymorphic variant" vc_name label
      in
      match vc_param with
      | `no_param ->
        begin match Caml_config.get_variant_type td_configs with
        | `regular -> Pat.construct (lidloc vc_name) None
        | `polymorphic -> Pat.variant vc_name None
      end
      | `tuple_like args ->
        let inner = Some (Pat.tuple (List.mapi (fun i _ -> pvari i) args)) in
        begin match Caml_config.get_variant_type td_configs with
        | `regular -> Pat.construct (lidloc vc_name) inner
        | `polymorphic -> Pat.variant vc_name inner
        end
      | `inline_record fields -> of_record_fields ~label:"inline record" fields
      | `reused_inline_record decl ->
        let fields = decl.td_kind |> function
          | Record_decl fields -> fields
          | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." vc_name
        in
        of_record_fields ~label:"reused inline record" fields
  in
  let variant_body : variant_constructor list -> expression list = fun cnstrs ->
    cnstrs |&> fun ({ vc_name; vc_param; vc_configs; _ } as ctor) ->
      let discriminator_fname =
        Json_config.get_variant_discriminator td_configs
        |> Json.Json_config.mangled `field_name td_configs
      in
      let discriminator_value = Json_config.get_mangled_name_of_discriminator td_configs ctor in
      let arg_fname =
        Json_config.(get_name_of_variant_arg default_name_of_variant_arg) vc_configs
        |> Json.Json_config.mangled `field_name vc_configs
      in
      let of_record_fields fields =
        let discriminator_fname = estring ~loc discriminator_fname in
        let cstr = [%expr ([%e discriminator_fname], `str [%e estring ~loc discriminator_value])] in
        let args = List.mapi (fun i field -> member_of_field i field) fields in
        [%expr `obj [%e elist ~loc (cstr :: args)]]
      in
      match Json_config.get_variant_style vc_configs with
      | `flatten -> begin
        match vc_param with
        | `no_param ->
          let cstr = [%expr ([%e estring ~loc discriminator_fname], `str [%e estring ~loc discriminator_value])] in
          [%expr `obj [[%e cstr]]]
        | `tuple_like args ->
          let discriminator_fname = estring ~loc discriminator_fname in
          let arg_fname = estring ~loc arg_fname in
          let cstr = [%expr ([%e discriminator_fname], `str [%e estring ~loc discriminator_value])] in
          let args =
            List.mapi (fun i typ ->
                [%expr [%e encoder_of_coretype self_ename typ] [%e evari i]])
              args in
          begin match args, Json_config.get_tuple_style vc_configs with
          | [], _ -> [%expr `obj [[%e cstr]]]
          | [arg], _ -> [%expr `obj [[%e cstr]; ([%e arg_fname], [%e arg])]]
          | _, `arr -> [%expr `obj [[%e cstr]; ([%e arg_fname], `arr [%e elist ~loc args])]]
          | _, `obj `default ->
            let fields =
              args |> List.mapi (fun i arg ->
                let label = estring ~loc (tuple_index_to_field_name i) in
                [%expr ([%e label], [%e arg])])
            in
            [%expr `obj ([%e cstr] :: [%e elist ~loc fields])]
          end
        | `inline_record fields -> of_record_fields fields
        | `reused_inline_record decl ->
          let fields = decl.td_kind |> function
            | Record_decl fields -> fields
            | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." vc_name
          in
          of_record_fields fields
      end
  in
  match kind with
  | Alias_decl cty ->
    Vb.mk
      ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
      self_pname
      (pexp_constraint ~loc
         (wrap_self_contained (encoder_of_coretype self_ename cty))
         [%type: [%t typcons ~loc td_name] -> Kxclib.Json.jv])
  | Record_decl fields ->
    let params = record_params fields in
    let body = record_body fields in
    Vb.mk
      ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
      self_pname
      (pexp_constraint ~loc
        (wrap_self_contained [%expr fun [%p params] -> [%e body]])
        [%type: [%t typcons ~loc td_name] -> Kxclib.Json.jv])
  | Variant_decl ctors ->
    let params = variant_params ctors in
    let body = variant_body ctors in
    let cases =
      List.map2
        (fun p b -> case ~lhs:p ~rhs:b ~guard:None)
        params body
    in
    Vb.mk
      ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
      self_pname
      (pexp_constraint ~loc
        (wrap_self_contained (pexp_function ~loc cases))
        [%type: [%t typcons ~loc td_name] -> Kxclib.Json.jv])

let gen_json_decoder_result :
      ?self_contained:bool
      -> ?json_shape_explanation_style:[
        | `inline of json_shape_explanation_resolution option
        | `reference ]
      -> ?codec:Coretype.codec
      -> type_decl
      -> value_binding =
  fun ?(self_contained=false) ?(json_shape_explanation_style = `inline None) ?(codec=`default) td ->
  let { td_name; td_kind=kind; td_configs; _ } = td in
  let loc = Location.none in
  let impl_name = "of_json_impl" in
  let impl_pname = pvar impl_name in
  let impl_ename = evar impl_name in
  let self_name = (json_decoder_name ~codec td) ^ "'" in
  let self_pname = pvar self_name in
  let vari i = "x"^string_of_int i in
  let evari i = evar ~loc (vari i) in
  let pvari i = pvar ~loc (vari i) in
  let param_e = evar ~loc "param" in
  let param_p = pvar ~loc "param" in
  let wrap_self_contained e =
    if self_contained then
      match gen_builtin_decoders td with
      | [] -> e
      | es ->
         pexp_let ~loc Nonrecursive
           es e
    else e
  in
  let record_bindings : record_field list -> (pattern * expression) list = fun fields ->
    List.mapi (fun i field ->
      let json_field_name = Json_config.get_mangled_name_of_field td_configs field in
      let json_field = estring ~loc json_field_name in
      let expr =
        if Coretype.is_option field.rf_type then
          [%expr
            List.assoc_opt [%e json_field] [%e param_e]
            |> Option.value ~default:`null
            |> [%e decoder_of_coretype impl_ename field.rf_type] (`f [%e json_field] :: path)]
        else
          let error_message = sprintf "mandatory field '%s' does not exist" json_field_name in
          [%expr
            List.assoc_opt [%e json_field] [%e param_e]
            |> [%e opt_to_result [%expr ([%e estring ~loc error_message], path)]]
            >>= [%e decoder_of_coretype impl_ename field.rf_type] (`f [%e json_field] :: path)
          ]
        in
      pvari i, expr)
    fields
  in
  let record_body : record_field list -> expression = fun fields ->
    pexp_record ~loc
      (List.mapi (fun i { rf_name; _; } ->
          (lidloc ~loc rf_name, [%expr [%e evari i]]))
         fields)
      None
  in
  let object_is_expected_error_record, object_is_expected_error_variant =
    let object_is_expected_error label jv =
      let error_message =
        sprintf "an object is expected for a %s value, but the given is of type '%%s'" label
      in
      [%expr
        Error (
          Printf.sprintf
            [%e estring ~loc error_message]
            Kxclib.Json.(string_of_jv_kind (classify_jv [%e jv])),
          path)
      ]
    in
    object_is_expected_error "record", object_is_expected_error "variant"
  in
  let variant_body : variant_constructor list -> (pattern * expression) list = fun cstrs ->
    let discriminator_fname =
      Json_config.get_variant_discriminator td_configs
      |> Json.Json_config.mangled `field_name td_configs
    in
    let discriminator_fname_p = pstring ~loc discriminator_fname in
    cstrs
    |&> (fun ({ vc_name; vc_param; vc_configs; _ } as ctor) ->
      let discriminator_value = Json_config.get_mangled_name_of_discriminator td_configs ctor in
      let arg_fname =
        Json_config.(get_name_of_variant_arg default_name_of_variant_arg) vc_configs
        |> Json.Json_config.mangled `field_name vc_configs
      in
      let cstr_p tail = [%pat? `obj (([%p discriminator_fname_p], `str [%p pstring ~loc discriminator_value])::[%p tail])] in
      let construct name args =
        match Caml_config.get_variant_type td_configs with
        | `regular -> Exp.construct (lidloc name) args
        | `polymorphic -> Exp.variant name args
      in
      match Json_config.get_variant_style vc_configs with
      | `flatten ->
        let of_record_fields ~label fields =
          match fields with
          | [] -> construct vc_name None
          | _ ->
            let bindings = record_bindings fields in
            let body =
              match Caml_config.get_variant_type td_configs with
              | `regular -> record_body fields
              | `polymorphic -> failwith' "case '%s' with an %s cannot be used in a polymorphic variant" vc_name label
            in
            bind_results bindings [%expr Ok [%e (construct vc_name (Some body))]]
        in
        begin match vc_param with
          | `no_param -> cstr_p [%pat? _], [%expr Ok [%e construct vc_name None]]
          | `tuple_like ts ->
            let body : expression =
              [%expr Ok
                [%e construct
                  vc_name
                  (Some (pexp_tuple ~loc (List.mapi (fun i _ -> evari i) ts)))]] in
            begin match Json_config.get_tuple_style vc_configs, ts with
              | _, [] -> cstr_p [%pat? _], [%expr Ok [%e construct vc_name None]]
              | `obj `default, _ :: _ :: _ ->
                let bindings =
                  ts |> List.mapi (fun i arg ->
                    let label_name = tuple_index_to_field_name i in
                    let label_name_e = estring ~loc label_name in
                    let error_message = sprintf "mandatory field '%s' does not exist" label_name in
                    pvari i, [%expr
                      List.assoc_opt [%e label_name_e] [%e param_e]
                      |> [%e opt_to_result [%expr ([%e estring ~loc error_message], path)]]
                      >>= ([%e decoder_of_coretype impl_ename arg] (`f [%e label_name_e] :: path))
                    ])
                in
                cstr_p param_p, [%expr [%e bind_results bindings body]]
              | _, _ ->
                cstr_p param_p, (
                  let path_arg = [%expr `f [%e estring ~loc arg_fname] :: path ] in
                  match ts with
                  | [t] ->
                    [ [%pat? Some arg],
                      bind_results
                        [ pvari 0, [%expr [%e decoder_of_coretype impl_ename t] [%e path_arg] arg]]
                        body; ]
                  | ts ->
                    [ [%pat? Some (`arr [%p plist ~loc (List.mapi (fun i _ -> pvari i) ts)])],
                      bind_results
                        (List.mapi (fun i arg -> pvari i, [%expr
                            [%e decoder_of_coretype impl_ename arg]
                              (`i [%e eint ~loc i] :: [%e path_arg])
                              [%e evari i]]) ts)
                        body;

                      [%pat? Some (`arr xs)],
                      [%expr Error (
                        Printf.sprintf
                          [%e sprintf "expecting an array of length %d, but the given has a length of %%d" (List.length ts) |> estring ~loc ]
                          (List.length xs),
                        [%e path_arg])];

                      [%pat? Some jv],
                      [%expr Error (
                        Printf.sprintf "an array is expected for a tuple value, but the given is of type '%s'"
                          Kxclib.Json.(string_of_jv_kind (classify_jv jv)),
                        [%e path_arg])];
                    ]
                ) @ [
                  [%pat? None],
                  [%expr Error([%e estring ~loc (sprintf "mandatory field '%s' does not exist" arg_fname)], path)];
                ]
                |&> (fun (pat, body) -> case ~lhs:pat ~rhs:body ~guard:None)
                |> pexp_match ~loc [%expr
                  List.assoc_opt [%e estring ~loc arg_fname] [%e param_e]
                ]
            end
          | `inline_record fields ->
            cstr_p param_p, of_record_fields ~label:"inline record" fields
          | `reused_inline_record decl ->
            let fields =
              decl.td_kind |> function
              | Record_decl fields -> fields
              | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." vc_name
            in
            cstr_p param_p, of_record_fields ~label:"reused inline record" fields
        end)
    |> fun cases ->
      let unexpected_discriminator_error_message_format =
        sprintf "given discriminator field value '%%s' is not one of [ %s ]"
          (cstrs
            |&> (fun ctor ->
              Json_config.get_mangled_name_of_discriminator td_configs ctor
              |> sprintf "'%s'")
            |> String.concat ", ")
        |> estring ~loc
      in
      cases @ [
        [%pat? `obj (([%p discriminator_fname_p], `str discriminator_value)::_)],
        [%expr
          Error (
            Printf.sprintf [%e unexpected_discriminator_error_message_format] discriminator_value,
            `f [%e estring ~loc discriminator_fname] :: path)];
        [%pat? `obj (([%p discriminator_fname_p], jv)::_)],
        [%expr
          Error (
            Printf.sprintf "a string is expected for a variant discriminator, but the given is of type '%s'"
              Kxclib.Json.(string_of_jv_kind (classify_jv jv)),
            `f [%e estring ~loc discriminator_fname] :: path)];
        [%pat? `obj _],
        [%expr
          Error ([%e
            sprintf "discriminator field '%s' does not exist" discriminator_fname
            |> estring ~loc],
            path)];
        [%pat? jv], object_is_expected_error_variant [%expr jv];
      ]
  in
  let function_body =
    match kind with
    | Alias_decl cty ->
      (wrap_self_contained
         [%expr [%e (decoder_of_coretype impl_ename cty)] path])
    | Record_decl fields ->
      let bindings = record_bindings fields in
      let body = record_body fields in
      bind_results bindings [%expr Ok [%e body]]
      |> fun body -> [%expr function
        | `obj [%p param_p] -> [%e body]
        | jv -> [%e object_is_expected_error_record [%expr jv]]
      ]
      |> wrap_self_contained
    | Variant_decl ctors ->
      let discriminator_fname = Json_config.get_variant_discriminator td_configs in
      [%expr fun __bindoj_orig ->
        __bindoj_orig
        |> Kxclib.Jv.pump_field [%e estring ~loc discriminator_fname]
        |> [%e
          variant_body ctors
          |&> (fun (pat, body) -> case ~lhs:pat ~rhs:body ~guard:None)
          |> pexp_function ~loc
          |> wrap_self_contained ]]
  in
  Vb.mk
    ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
    self_pname
    (pexp_constraint ~loc
      [%expr fun ?(path = []) x ->
        let rec [%p impl_pname] = fun path -> [%e function_body] in
        [%e impl_ename] path x
        |> Result.map_error (fun (msg, path) ->
          (msg, path, [%e
            match json_shape_explanation_style with
            | `inline json_shape_explanation_resolution ->
              explain_encoded_json_shape ?json_shape_explanation_resolution td
            | `reference ->
              let self_json_shape_explanation_name = match codec with
                | `default | `open_ _ -> td_name ^ "_json_shape_explanation"
                | `in_module _ -> "json_shape_explanation"
              in
              evar self_json_shape_explanation_name
          ]))
      ]
      [%type: ?path:Kxclib.Json.jvpath -> Kxclib.Json.jv -> [%t typcons ~loc td_name] Bindoj_runtime.OfJsonResult.t])

let gen_json_decoder_option :
    ?codec:Coretype.codec
      -> type_decl
      -> value_binding =
  fun ?(codec=`default) td ->
  let { td_name; _ } = td in
  let loc = Location.none in
  let self_name = json_decoder_name ~codec td in
  let self_pname = pvar self_name in
  let decoder_result_name = evar (self_name ^ "'") in
  Vb.mk
    ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
    self_pname
    (pexp_constraint ~loc
      [%expr fun x -> [%e decoder_result_name] x |> Result.to_option ]
      [%type: Kxclib.Json.jv -> [%t typcons ~loc td_name] option])

let gen_json_shape_explanation :
  ?json_shape_explanation_resolution:json_shape_explanation_resolution
  -> ?codec:Coretype.codec
  -> type_decl -> value_binding =
  fun ?json_shape_explanation_resolution ?(codec=`default) td ->
  let loc = Location.none in
  let self_name = json_shape_explanation_name ~codec td in
  let self_pname = pvar self_name in
  let json_shape_expression = explain_encoded_json_shape ?json_shape_explanation_resolution td in
  Vb.mk
    ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
    self_pname
    (pexp_constraint ~loc json_shape_expression
      [%type: Bindoj_runtime.json_shape_explanation])

let gen_discriminator_value_accessor : ?codec:Coretype.codec -> type_decl -> value_binding =
  fun ?(codec=`default) td ->
    let loc = Location.none in
    let self_name = gen_discriminator_value_accessor_name ~codec td in
    let poly = (Caml_config.get_variant_type td.td_configs) = `polymorphic in
    let body =
      td.td_kind
      |> begin function
        | Variant_decl ctors ->
          ctors
          |&> (fun ({ vc_name; vc_param; _ } as ctor) ->
            let arg = if vc_param = `no_param then None else Some(Pat.any ()) in
            let pat =
              if poly then Pat.variant vc_name arg
              else Pat.construct (lidloc vc_name) arg
            in
            let discriminator_value =
              Json.Json_config.get_mangled_name_of_discriminator td.td_configs ctor
              |> estring ~loc
            in
            case ~lhs:pat ~rhs:discriminator_value ~guard:None)
        | _ -> invalid_arg' "'%s' is not a variant decl" td.td_name
      end
      |> pexp_function ~loc
    in
    Vb.mk
      ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
      (pvar self_name)
      (pexp_constraint ~loc body [%type: [%t typcons ~loc td.td_name] -> string])

let gen_json_codec =
  let gen_json_shape_explanation' = gen_json_shape_explanation in

  fun ?self_contained ?(gen_json_shape_explanation=true) ?(discriminator_value_accessor=true) ?json_shape_explanation_resolution ?codec td ->
  let add_item_when cond f xs = if cond then (f ()) :: xs else xs in
  let bindings =
    [ gen_json_encoder ?self_contained ?codec td;
      gen_json_decoder_result
        ?self_contained
        ~json_shape_explanation_style:
          ( if gen_json_shape_explanation then `reference
            else `inline json_shape_explanation_resolution )
        ?codec
        td;
      gen_json_decoder_option ?codec td; ]
  in
  [ Str.value Recursive bindings ]
  |> add_item_when
      gen_json_shape_explanation
      (fun () ->
        gen_json_shape_explanation' ?json_shape_explanation_resolution ?codec td
        |> List.return |> Str.value Nonrecursive)
  |> add_item_when
      (discriminator_value_accessor && match td.td_kind with | Variant_decl _ -> true | _ -> false)
      (fun () ->
        (gen_discriminator_value_accessor ?codec td)
        |> List.return |> Str.value Nonrecursive)

let gen_json_encoder_signature :
  ?codec:Coretype.codec
  -> type_decl
  -> Ppxlib.value_description =
  fun ?(codec=`default) td ->
    let loc = Location.none in
    let self_name = json_encoder_name ~codec td in
    Val.mk ~loc (strloc ~loc self_name)
      [%type: [%t typcons ~loc td.td_name] -> Kxclib.Json.jv]

let gen_json_decoder_result_signature :
  ?codec:Coretype.codec
  -> type_decl
  -> Ppxlib.value_description =
  fun ?(codec=`default) td ->
    let loc = Location.none in
    let self_name = (json_decoder_name ~codec td) ^ "'" in
    Val.mk ~loc (strloc ~loc self_name)
      [%type: [%t typcons ~loc td.td_name] Bindoj_runtime.json_full_decoder]

let gen_json_decoder_option_signature :
  ?codec:Coretype.codec
  -> type_decl
  -> Ppxlib.value_description =
  fun ?(codec=`default) td ->
    let loc = Location.none in
    let self_name = json_decoder_name ~codec td in
    Val.mk ~loc (strloc ~loc self_name)
      [%type: Kxclib.Json.jv -> [%t typcons ~loc td.td_name] option]

let gen_json_shape_explanation_signature :
  ?codec:Coretype.codec
  -> type_decl
  -> Ppxlib.value_description =
  fun ?(codec=`default) td ->
    let loc = Location.none in
    let self_name = json_shape_explanation_name ~codec td in
    Val.mk ~loc (strloc ~loc self_name)
      [%type: Bindoj_runtime.json_shape_explanation]

let gen_discriminator_value_accessor_signature :
  ?codec:Coretype.codec -> type_decl -> Ppxlib.value_description =
  fun ?(codec=`default) td ->
    let loc = Location.none in
    let self_name = gen_discriminator_value_accessor_name ~codec td in
    Val.mk ~loc (strloc ~loc self_name)
      [%type: [%t typcons ~loc td.td_name] -> string]

let gen_json_codec_signature :
  ?gen_json_shape_explanation:bool
  -> ?discriminator_value_accessor:bool
  -> ?codec:Coretype.codec
  -> type_decl
  -> Ppxlib.signature =
  fun ?(gen_json_shape_explanation=true) ?(discriminator_value_accessor=true) ?codec td ->
    let add_item_when cond f xs = if cond then (f ()) :: xs else xs in
    let bindings =
      [ gen_json_encoder_signature ?codec td;
        gen_json_decoder_result_signature ?codec td;
        gen_json_decoder_option_signature ?codec td; ]
      |> (add_item_when
            gen_json_shape_explanation
            (constant @@ gen_json_shape_explanation_signature ?codec td))
      |> (add_item_when
            (discriminator_value_accessor && match td.td_kind with | Variant_decl _ -> true | _ -> false)
            (fun () -> gen_discriminator_value_accessor_signature ?codec td))
    in
    List.map Sig.value bindings

open Bindoj_openapi.V3

exception Incompatible_with_openapi_v3 of string

let base64_regex = {|^(?:[A-Za-z0-9+\/]{4})*(?:[A-Za-z0-9+\/][AQgw]==|[A-Za-z0-9+\/]{2}[AEIMQUYcgkosw048]=)?$|}

let gen_json_schema : ?openapi:bool -> type_decl -> Schema_object.t =
  fun ?(openapi=false) ->

  let docopt = function `docstr s -> Some s | `nodoc -> None in

  let convert_coretype ~self_name ?description (ct: coretype) =
    let tuple_style = Json_config.get_tuple_style ct.ct_configs in
    let rec go =
      let open Coretype in
      function
      | Prim `unit -> Schema_object.integer ~minimum:1 ~maximum:1 ?description ()
      | Prim `bool -> Schema_object.boolean ?description ()
      | Prim `int -> Schema_object.integer ?description ()
      | Prim `int53p -> Schema_object.integer ?description ()
      | Prim `float -> Schema_object.number ?description ()
      | Prim `string -> Schema_object.string ?description ()
      | Prim `uchar -> Schema_object.string ~minLength:1 ~maxLength:1 ?description ()
      | Prim `byte -> Schema_object.integer ~minimum:0 ~maximum:255 ?description ()
      | Prim `bytes -> Schema_object.string ~format:`byte ~pattern:base64_regex ?description ()
      | Uninhabitable -> Schema_object.null ?description ()
      | Ident id ->
        if openapi then (* in OpenAPI, types live in #/components/schemas/ *)
          Schema_object.ref ("#/components/schemas/" ^ id.id_name)
        else
          Schema_object.ref ("#" ^ id.id_name)
      | Option t ->
        Schema_object.option (go t)
      | Tuple ts ->
        begin match tuple_style with
        | `arr ->
          if openapi then
            raise (Incompatible_with_openapi_v3 (
              sprintf "OpenAPI v3 does not support tuple validation (in type '%s')" self_name))
          else
            Schema_object.tuple ?description (ts |> List.map go)
        | `obj `default ->
          Schema_object.record ?description (ts |> List.mapi (fun i x ->
            tuple_index_to_field_name i, go x
          ))
        end
      | List t ->
        Schema_object.array ?description ~items:(`T (go t)) ()
      | Map (`string, t) ->
        Schema_object.obj ?description ~additionalProperties:(`T (go t)) ()
      | StringEnum cases ->
        let enum = cases |> List.map (
          Json_config.get_mangled_name_of_string_enum_case ct.ct_configs
          &> (fun case -> `str case)) in
        Schema_object.string ~enum ()
      | Self ->
        if openapi then (* in OpenAPI, types live in #/components/schemas/ *)
          Schema_object.ref ("#/components/schemas/" ^ self_name)
        else
          Schema_object.ref ("#" ^ self_name)
    in
    match ct.ct_configs |> Configs.find_foreign_type_expr json_schema with
    | Some schema -> schema
    | None -> go ct.ct_desc
  in

  fun { td_name = name; td_kind; td_doc = doc; td_configs } ->
    let self_name = name in

    let schema =
      if openapi then None (* OpenAPI v3 does not support `$schema` *)
      else Some Schema_object.schema in

    let id =
      if openapi then None (* OpenAPI v3 does not support `id` *)
      else Some ("#" ^ name) in

    let record_to_t ?schema ?id ?(additional_fields = []) ~name ~self_name ~doc fields =
      let field_to_t ({ rf_type; rf_doc; _ } as field) =
        (Json_config.get_mangled_name_of_field td_configs field),
        convert_coretype ~self_name ?description:(docopt rf_doc) rf_type
      in
      let fields = fields |> List.map field_to_t in
      Schema_object.record
        ?schema
        ~title:name
        ?description:(docopt doc)
        ?id
        (fields @ additional_fields)
    in

    match td_kind with
    | Record_decl fields ->
      record_to_t ?schema ?id ~name ~self_name ~doc fields
    | Variant_decl ctors ->
      let discriminator_fname =
        Json_config.get_variant_discriminator td_configs
        |> Json.Json_config.mangled `field_name td_configs
      in
      let ctor_to_t ({ vc_name; vc_param; vc_doc = doc; vc_configs; _ } as ctor) =
        let discriminator_value = Json_config.get_mangled_name_of_discriminator td_configs ctor in
        let discriminator_field =
          let enum = [`str discriminator_value] in
          [discriminator_fname, Schema_object.string ~enum ()]
        in
        match Json_config.get_variant_style vc_configs with
        | `flatten ->
          begin match vc_param with
            | `no_param | `tuple_like [] ->
              Schema_object.record ?description:(docopt doc) ~title:discriminator_value discriminator_field
            | `tuple_like (t :: ts) ->
              let arg_name =
                vc_configs
                |> Json_config.(get_name_of_variant_arg default_name_of_variant_arg)
                |> Json.Json_config.mangled `field_name vc_configs
              in
              let arg_field =
                match ts, Json_config.get_tuple_style vc_configs with
                | [], _ -> [arg_name, convert_coretype ~self_name t]
                | _, `arr ->
                  if openapi then
                    raise (Incompatible_with_openapi_v3 (
                      sprintf "OpenAPI v3 does not support tuple validation (in type '%s')" self_name))
                  else
                    let ts = t :: ts |> List.map (convert_coretype ~self_name) in
                    [arg_name, Schema_object.tuple ts]
                | _, `obj `default ->
                  t :: ts |> List.mapi (fun i t ->
                    tuple_index_to_field_name i, convert_coretype ~self_name t
                  )
              in
              let fields = discriminator_field @ arg_field in
              Schema_object.record ?description:(docopt doc) ~title:discriminator_value fields
            | `inline_record fields ->
              record_to_t ~additional_fields:discriminator_field ~name:discriminator_value ~self_name ~doc fields
            | `reused_inline_record decl ->
              let fields = decl.td_kind |> function
                | Record_decl fields -> fields
                | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." vc_name
              in
              record_to_t ~additional_fields:discriminator_field ~name:discriminator_value ~self_name ~doc fields

          end
      in
      Schema_object.oneOf
        ?schema
        ~title:name
        ?description:(docopt doc)
        ?id
        (ctors |> List.map ctor_to_t)
    | Alias_decl cty ->
      convert_coretype ~self_name ?description:(docopt doc) cty

let gen_openapi_schema : type_decl -> Schema_object.t = gen_json_schema ~openapi:true
