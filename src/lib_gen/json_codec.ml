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
open Ppxlib
open Ast_builder.Default
open Ast_helper
open Utils
open Bindoj_runtime
open Bindoj_base
open Bindoj_base.Type_desc

module Json_config = Bindoj_gen_config.Json_config

let tuple_index_to_field_name = Json_config.tuple_index_to_field_name

let get_json_shape_explanation_name type_name = function
  | `default -> None, type_name^"_json_shape_explanation"
  | `open_ m -> Some m, type_name^"_json_shape_explanation"
  | `in_module m -> None, m^".json_shape_explanation"

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

let codec_of_coretype ~inherited_codec ~get_custom_codec ~get_name ~map_key_converter ~tuple_case ~string_enum_case ~wrap_ident self_ename (ct: coretype) =
  let loc = Location.none in
  match get_custom_codec ct.ct_configs with
  | Some coder -> evar coder
  | None ->
    let evar_name ?codec name =
      let open_, name = get_name ?resolution_strategy:codec name in
      evar ?open_ name
    in
    let rec go = function
      | Coretype.Prim p -> evar_name (Coretype.string_of_prim p)
      | Uninhabitable -> evar_name "uninhabitable"
      | Ident i ->
        let codec = match i.id_codec with | `default -> inherited_codec | c -> c in
        evar_name ~codec i.id_name |> wrap_ident
      | Option t -> [%expr [%e evar_name "option"] [%e go t]] (* option_of_json t_of_json *)
      | List t -> [%expr [%e evar_name "list"] [%e go t]] (* list_of_json t_of_json *)
      | Map (k, v) -> [%expr [%e evar_name "map"] [%e map_key_converter k] [%e go v]] (* map_of_json key_of_string t_of_json *)
      | Tuple ts -> tuple_case ct.ct_configs go ts
      | StringEnum cs -> string_enum_case cs
      | Self -> self_ename
    in
    go ct.ct_desc

let collect_builtin_codecs ~including_optional_fields (td: type_decl) =
  let folder state =
    let folder configs =
      let json_tuple_style = Json_config.get_tuple_style configs in
      Json_config.Bindoj_private.collect_coretypes_folder ~json_tuple_style ~including_optional_fields
        ((function
          | `prim s -> Some s
          | `uninhabitable -> Some "uninhabitable"
          | `option -> Some "option"
          | `list -> Some "list"
          | `map -> Some "map"
          | `ident _ | `string_enum | `self -> None)
          &> (Fn.flip Option.bind (Fn.flip StringMap.find_opt builtin_codecs_map)))
    in
    function
    | { Coretype.ct_desc; ct_configs }, `alias _ :: [] ->
      folder ct_configs state ct_desc
    | { ct_desc = Option desc; ct_configs },
      ( (`record_field _ | `variant_field _ | `variant_reused_field _ | `variant_argument (1, _, _, _))::_
      | `alias _ :: (`record_field _ | `variant_field _ | `variant_reused_field _ | `variant_argument (1, _, _, _))::_)
      when not including_optional_fields ->
      folder ct_configs state desc
    | { ct_desc; ct_configs },
      ( (`variant_argument (_, _, vc_configs, _))::_
      | `alias _::(`variant_argument (_, _, vc_configs, _))::_) ->
      begin match Json_config.get_tuple_style vc_configs, ct_desc with
      | `obj `default, Option desc when not including_optional_fields ->
        folder ct_configs state desc
      | _, desc -> folder ct_configs state desc
      end
    | { ct_desc; ct_configs }, _ -> folder ct_configs state ct_desc
  in
  fold_coretypes' folder StringMap.empty td

let bind_results'
  : (pattern * expression * [ `bind | `pipe ]) list
  -> expression -> expression =
  fun bindings body ->
  let loc = Location.none in
  let expr, has_bind =
    List.fold_right (fun (p, e, kind) (body, has_bind) ->
      match kind with
      | `bind -> [%expr [%e e] >>= (fun [%p p] -> [%e body])], true
      | `pipe -> [%expr [%e e] |> (fun [%p p] -> [%e body])], has_bind
    ) bindings (body, false)
  in
  if has_bind then
    [%expr let (>>=) = Result.bind in [%e expr]]
  else
    expr

let bind_results : (pattern * expression) list -> expression -> expression = fun bindings body ->
  bindings
  |&> (fun (p, e) -> (p, e, `bind))
  |> Fn.flip bind_results' body

let opt_to_result : loc:location -> expression -> expression =
  fun ~loc err ->
    [%expr Option.to_result ~none:[%e err]]

let encoder_of_objtuple ?additional_field ~loc to_expr = function
  | [] -> additional_field |?! (fun () -> [%expr []])
  | ts ->
    let es = ts |> List.mapi (fun i t ->
      let label = estring ~loc (tuple_index_to_field_name i) in
      let encoded, is_optional = to_expr i t in
      let efield = [%expr ([%e label], [%e encoded])] in
      efield, is_optional)
    in
    let has_optional = List.exists snd es in
    let fields =
      es |> List.mapi (fun i (efield, is_optional) ->
        if is_optional then
          let v = "x"^string_of_int i in
          [%expr Option.map (fun [%p pvar v] -> [%e efield]) [%e evar v]]
        else if has_optional then
          [%expr Some [%e efield]]
        else efield
      )
      |> elist ~loc
    in
    let e =
      if has_optional then
        [%expr List.filter_map Kxclib.identity [%e fields]]
      else fields in
    begin match additional_field with
    | None -> e
    | Some f ->  [%expr [%e f] :: [%e e]]
    end

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
    let ret =
      match Json_config.get_tuple_style configs with
      | `obj `default ->
        let fields =
          encoder_of_objtuple ~loc (fun i -> function
            | Option (Option _) -> failwith "Nested option cannot be json fields."
            | Option t ->
              [%expr [%e go t] [%e evari i]], true
            | t ->
              [%expr [%e go t] [%e evari i]], false
          ) ts
        in
        [%expr `obj [%e fields] ]
      | `arr ->
        [%expr `arr [%e
          ts |> List.mapi (fun i t -> [%expr [%e go t] [%e evari i]])
             |> elist ~loc ]]
    in
    [%expr fun [%p args] -> ([%e ret] : Kxclib.Json.jv)]
  in
  let map_key_converter (k: map_key) = (* key_to_string *)
    match k with
    | `string -> [%expr fun (k: string) -> k]
  in
  let string_enum_case base_mangling_style (cs: Coretype.string_enum_case list) =
    let cases =
      cs |> List.map (fun ((name, _, _) as c) ->
        let json_name = Json_config.get_mangled_name_of_string_enum_case ~inherited:base_mangling_style c in
        let pat = Pat.variant (Utils.escape_as_constructor_name name) None in
        let expr = [%expr `str [%e Exp.constant (Const.string json_name)]] in
        Exp.case pat expr
      )
    in
    Exp.function_ cases
  in
  let wrap_ident = identity in
  fun inherited_codec base_mangling_style e ct ->
    let base_mangling_style =
      Json_config.get_mangling_style_opt ct.ct_configs
      |? base_mangling_style
    in
    codec_of_coretype
      ~inherited_codec
      ~get_custom_codec:Json_config.get_custom_encoder
      ~get_name:Json_config.get_encoder_name
      ~tuple_case ~map_key_converter ~string_enum_case:(string_enum_case base_mangling_style)
      ~wrap_ident
      e ct

let decoder_of_coretype =
  let open Coretype in
  let vari i = "x"^string_of_int i in
  let loc = Location.none in
  let evari i = evar (vari i) in
  let pvari i = pvar (vari i) in
  let tuple_case (configs: [`coretype] configs) (go: desc -> expression) (ts: desc list) =
    let ret =
      ts |> List.mapi (fun i _ -> [%expr [%e evari i]])
         |> Exp.tuple
         |> fun e -> [%expr Ok [%e e]]
    in
    match Json_config.get_tuple_style configs with
    | `arr ->
      let args =
        ts |> List.mapi (fun i _ -> pvari i)
           |> plist ~loc
      in
      let tuple_length_error_message =
        sprintf "expecting a tuple of length %d, but the given has a length of %%d" (List.length ts)
      in
      [%expr fun path -> function
        | (`arr [%p args] : Kxclib.Json.jv) -> [%e
          let bindings =
            ts |> List.mapi (fun i t ->
                pvari i, [%expr [%e go t] (`i [%e eint ~loc i] :: path) [%e evari i]]
              )
          in

          bind_results bindings ret]
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
      [%expr fun path -> function
        | (`obj fields : Kxclib.Json.jv) ->
          [%e
            ts
            |> List.mapi (fun i t ->
              let label_name = tuple_index_to_field_name i in
              let label = estring ~loc label_name in
              let error_message = sprintf "mandatory field '%s' does not exist" label_name in
                match t with
                | Option _ -> pvari i, [%expr
                  List.assoc_opt [%e label] fields
                  |> Option.value ~default:`null
                  |> [%e go t] (`f [%e label] :: path)
                ]
                | _ -> pvari i, [%expr
                  List.assoc_opt [%e label] fields
                  |> [%e opt_to_result ~loc [%expr ([%e estring ~loc error_message], path)]]
                  >>= [%e go t] (`f [%e label] :: path)
                ]
              )
            |> Fn.flip bind_results ret
          ]
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
  let string_enum_case base_mangling_style (cs: Coretype.string_enum_case list) =
    let cases =
      cs |> List.map (fun ((name, _, _) as c) ->
        let json_name = Json_config.get_mangled_name_of_string_enum_case ~inherited:base_mangling_style c in
        let pat = Pat.constant (Const.string json_name) in
        let expr = Exp.variant (Utils.escape_as_constructor_name name) None in
        Exp.case pat [%expr Ok [%e expr]]
      ) |> fun cases ->
        let error_message =
          (cs |&> (Json_config.get_mangled_name_of_string_enum_case ~inherited:base_mangling_style &> sprintf "'%s'")
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
  fun inherited_codec base_mangling_style e ct ->
  let base_mangling_style =
    Json_config.get_mangling_style_opt ct.ct_configs
    |? base_mangling_style
  in
  codec_of_coretype
    ~inherited_codec
    ~get_custom_codec:Json_config.get_custom_decoder
    ~get_name:Json_config.get_decoder_name
    ~tuple_case ~map_key_converter ~string_enum_case:(string_enum_case base_mangling_style)
    ~wrap_ident
    e ct

let gen_builtin_codecs ?attrs ~including_optional_fields
  ~(get_name: ?resolution_strategy:[ `default | `in_module of label | `open_ of label ] -> label -> label option * label)
  ~get_codec (td: type_decl) =
  let loc = Location.none in
  let coders = collect_builtin_codecs ~including_optional_fields td in
  let bind (_, name) expr = Vb.mk ~loc ?attrs (Pat.var (strloc name)) expr in
  StringMap.fold (fun label coder state ->
    bind (get_name ~resolution_strategy:`default label) (get_codec coder) :: state
  ) coders []

let gen_builtin_encoders : ?attrs:attrs -> type_decl -> value_binding list =
  gen_builtin_codecs
    ~including_optional_fields:false
    ~get_name:Json_config.get_encoder_name ~get_codec:(fun x -> x.encoder)

let gen_builtin_decoders : ?attrs:attrs -> type_decl -> value_binding list =
  gen_builtin_codecs
    ~including_optional_fields:true
    ~get_name:Json_config.get_decoder_name ~get_codec:(fun x -> x.decoder)

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

let explain_encoded_json_shape :
  ?json_shape_explanation_resolution:json_shape_explanation_resolution
  -> type_decl
  -> expression =
  let loc = Location.none in
  let jse: (expression, expression) json_shape_explaner = (module struct
    type shape = expression
    type field_shape = expression
    let shape_of_json_shape_explanation : json_shape_explanation -> shape = ejson_shape_explanation ~loc
    let self : shape = [%expr `self ]
    let named : string * shape -> shape =
      fun (a, b) -> [%expr `named ([%e estring ~loc a], [%e b])]
    let special : string * shape -> shape =
      fun (a, b) -> [%expr `special ([%e estring ~loc a], [%e b])]
    let with_warning : string * shape -> shape =
      fun (a, b) -> [%expr `with_warning ([%e estring ~loc a], [%e b])]
    let exactly : Json.jv -> shape = fun x -> [%expr `exactly [%e ejv ~loc x]]
    let any_json_value : shape = [%expr `any_json_value ]
    let unresolved : string -> shape = fun x -> [%expr `unresolved [%e estring ~loc x]]
    let anyone_of : shape list -> shape = fun x -> [%expr `anyone_of [%e elist ~loc x]]
    let string_enum : string list -> shape = fun x -> [%expr `string_enum [%e x |&> estring ~loc |> elist ~loc]]
    let nullable : shape -> shape = fun x -> [%expr `nullable [%e x]]
    let boolean : shape = [%expr `boolean]
    let numeric : shape = [%expr `numeric]
    let integral : shape = [%expr `integral]
    let proper_int53p : shape = [%expr `proper_int53p]
    let proper_float : shape = [%expr `proper_float]
    let string : shape = [%expr `string]
    let base64str : shape = [%expr `base64str]
    let array_of : shape -> shape = fun x -> [%expr `array_of [%e x]]
    let tuple_of : shape list -> shape = fun x -> [%expr `tuple_of [%e elist ~loc x]]
    let record_of : shape -> shape = fun x -> [%expr `record_of [%e x]]
    let object_of : field_shape list -> shape = fun x ->  [%expr `object_of [%e elist ~loc x]]
    let mandatory_field : string * shape -> field_shape =
      fun (a, b) -> [%expr `mandatory_field ([%e estring ~loc a], [%e b])]
    let optional_field : string * shape -> field_shape =
      fun (a, b) -> [%expr `optional_field ([%e estring ~loc a], [%e b])]
  end) in
  fun ?json_shape_explanation_resolution td ->
  let json_shape_explanation_resolution =
    json_shape_explanation_resolution |? (constant `default)
  in
  Bindoj_codec.Json.explain_encoded_json_shape' jse
    (fun _ base_codec { id_name; id_codec; } ident_json_name ->
      let trim eshape = [%expr
        match [%e eshape] with
        | `with_warning (_, (`named _ as s)) -> s
        | `with_warning (_, s) | s -> `named ([%e estring ~loc ident_json_name], s)
      ] in
      begin match json_shape_explanation_resolution id_name with
      | `no_resolution -> [%expr `unresolved [%e estring ~loc ("alias: "^id_name)]]
      | `default ->
        let open_, name =
          (match id_codec with | `default -> base_codec | _ -> id_codec)
          |> get_json_shape_explanation_name id_name
        in
        trim (evar ?open_ name)
      | (`in_module _  | `open_ _) as resolution ->
        let open_, name = get_json_shape_explanation_name id_name resolution in
        trim (evar ?open_ name)
      end)
    td

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

let inherit_codec base_codec = function
  | `default -> base_codec
  | codec -> codec

let collect_nested_types_uniq f =
  let rec go =
    let go_nested_type_decl ~base_codec state = function
      | `direct _ -> state
      | `nested (td, codec) ->
        let inherited_codec = inherit_codec base_codec codec in
        go ~base_codec:inherited_codec ((inherited_codec, (td, codec)) :: state) td
    in
    let fold_record_fields ~base_codec =
      List.fold_left
        (fun state field ->
          go_nested_type_decl ~base_codec state field.rf_type)
    in
    fun ~base_codec state td ->
    match td with
    | { td_kind = Alias_decl _; _ } -> state
    | { td_kind = Record_decl fields; _ } ->
      fold_record_fields ~base_codec state fields
    | { td_kind = Variant_decl ctors; _ } ->
      List.fold_left (fun state ctor ->
        match ctor.vc_param with
        | `no_param -> state
        | `tuple_like ts ->
          ts |> List.fold_left (fun state va ->
            go_nested_type_decl ~base_codec state va.va_type) state
        | `inline_record fields
        | `reused_inline_record { td_kind = Record_decl fields; _ } ->
          fold_record_fields ~base_codec state fields
        | `reused_inline_record _ -> failwith' "panic - type decl of reused inline record '%s' must be record decl." ctor.vc_name
      ) state ctors
  in
  fun td ->
    go ~base_codec:`default [] td
    |> List.sort_uniq (fun (codec1, (td1, _)) (codec2, (td2, _)) ->
      String.compare
        (type_name_with_codec ~codec:codec1 td1.td_name)
        (type_name_with_codec ~codec:codec2 td2.td_name))
    |&?> (!! f)

let validate_spreading_type = Bindoj_codec.Json.validate_spreading_type

let gen_json_encoder' :
      ?self_contained:bool
      -> ?codec:Coretype.codec
      -> type_decl
      -> string * expression =
  fun ?(self_contained=false) ?(codec=`default) td ->
  let loc = Location.none in
  let vari i = "x"^string_of_int i in
  let evari i = evar ~loc (vari i) in
  let pvari i = pvar ~loc (vari i) in

  let nested_encoder_name codec td =
    (match codec with
    | `default -> ""
    | `open_ m | `in_module m -> String.lowercase_ascii m ^ "__")
    ^ json_encoder_name td ^ "_nested"
  in
  let get_encoder ?(is_field=false) ~inherited_codec ~nested ~spreading base_mangling_style self_ename ty arg =
    let wrap_obj cond =
      if cond then
        (fun e -> [%expr `obj ([%e e] [%e arg])])
      else
        (fun e -> [%expr ([%e e] [%e arg])])
    in
    let unwrap_option (ct: coretype) =
      if is_field then
        (match ct.ct_desc with
        | Option (Option _) when is_field -> failwith "Nested option types cannot be json fields."
        | Option d -> { ct with ct_desc = d }, true
        | _ -> ct, false)
      else ct, false
    in
    match ty with
    | `direct ct ->
      let ct, is_option = unwrap_option ct in
      let e = [%expr [%e encoder_of_coretype inherited_codec base_mangling_style self_ename ct]] in
      wrap_obj (nested && ct.ct_desc = Coretype.Self) e, is_option
    | `nested ({ td_kind = Alias_decl ct; _ }, _) ->
      let ct, is_option = unwrap_option ct in
      let inherited_codec = inherit_codec inherited_codec codec in
      let e = [%expr [%e encoder_of_coretype inherited_codec base_mangling_style self_ename ct]] in
      wrap_obj (nested && ct.ct_desc = Coretype.Self) e, is_option
    | `nested (td, codec) ->
      let inherited_codec = inherit_codec inherited_codec codec in
      let e = evar ~loc (nested_encoder_name inherited_codec td) in
      wrap_obj (not spreading) e, false
  in
  let record_params : record_field list -> pattern = fun fields ->
    ppat_record ~loc
      (List.mapi (fun i { rf_name; _; } ->
           (lidloc ~loc rf_name, pvari i))
         fields)
      Closed
  in
  let record_to_json_fields ~inherited_codec ~nested ~self_ename base_mangling_style fields =
    let rec go i (current, state) =
      let update_state () =
        match current with
        | `mandatory [] | `optional [] -> state
        | `mandatory current ->
          (elist ~loc @@ List.rev current) :: state
        | `optional current ->
          [%expr
            List.filter_map (fun x -> x) [%e
              elist ~loc @@ List.rev current
          ]] :: state
      in
      function
      | [] -> update_state ()
      | field :: fields ->
        let (json_field_name, base_mangling_style) =
          Json_config.get_mangled_name_of_field ~inherited:base_mangling_style field
        in
        let json_field_name = estring ~loc json_field_name in
        let nested_style = Json_config.get_nested_field_style field.rf_configs in
        let get_encoder ?is_field ~spreading = get_encoder ?is_field ~inherited_codec ~nested ~spreading base_mangling_style self_ename in
        match nested_style with
        | `spreading ->
          begin match validate_spreading_type field.rf_type with
          | `record_decl _ | `variant_decl _ ->
            let efields, _ = get_encoder ~spreading:true field.rf_type (evari i) in
            go (i + 1) (`mandatory [], efields :: update_state ()) fields
          end
        | `nested ->
          let encoded, is_optional = get_encoder ~is_field:true ~spreading:false field.rf_type (evari i) in
          let efield = [%expr ([%e json_field_name], [%e encoded])] in
          let efield =
            if is_optional then
              [%expr Option.map (fun [%p pvari i] -> [%e efield]) [%e evari i] ]
            else efield
          in
          fields |> go (i + 1) (match is_optional, current with
          | false, `mandatory current ->
            (`mandatory (efield::current), state)
          | false, `optional _ ->
            (`mandatory [ efield ], update_state ())
          | true, `optional current ->
            (`optional (efield::current), state)
          | true, `mandatory _ ->
            (`optional [ efield ], update_state ())
          )
    in
    go 0 (`mandatory [], []) fields
    |> function
    | [] -> failwith "A record must have at least one field."
    | hd :: tl ->
      List.fold_left (fun es e -> [%expr [%e e] @ [%e es]]) hd tl
  in
  let variant_params : [`type_decl] configs -> variant_constructor list -> pattern list = fun td_configs constrs ->
    constrs |&>> fun { vc_name; vc_param; _ } ->
      let of_record_fields ~label fields =
        match Caml_config.get_variant_type td_configs with
        | `regular ->
          ppat_construct ~loc
            (lidloc ~loc vc_name)
            (Some (record_params fields))
        | `polymorphic ->
          failwith' "case '%s' with an %s cannot be used in a polymorphic variant" vc_name label
      in
      let construct inner =
        begin match Caml_config.get_variant_type td_configs with
        | `regular -> Pat.construct (lidloc vc_name) inner
        | `polymorphic -> Pat.variant vc_name inner
        end
      in
      match vc_param with
      | `no_param -> [ construct None ]
      | `tuple_like [ arg ] ->
        let nested_style = Json_config.get_nested_field_style arg.va_configs in
        begin match nested_style, arg.va_type with
        | `nested, (`direct ct | `nested({ td_kind = Alias_decl ct; _}, _)) when Coretype.is_option ct ->
          [ construct (Some [%pat? None]);
            construct (Some [%pat? Some [%p pvari 0]]);
          ]
        | _ -> construct (Some (pvari 0)) |> List.return
        end
      | `tuple_like args ->
        Some (Pat.tuple (List.mapi (fun i _ -> pvari i) args))
        |> construct
        |> List.return
      | `inline_record fields ->
        of_record_fields ~label:"inline record" fields
        |> List.return
      | `reused_inline_record decl ->
        let fields = decl.td_kind |> function
          | Record_decl fields -> fields
          | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." vc_name
        in
        of_record_fields ~label:"reused inline record" fields
        |> List.return
  in
  let variant_body : nested:bool -> self_ename:expression -> inherited_codec:Coretype.codec -> type_decl -> Json_config.json_mangling_style -> variant_constructor list -> expression list =
    fun ~nested ~self_ename ~inherited_codec td base_mangling_style cnstrs ->
    let discriminator_fname = Json_config.get_mangled_name_of_discriminator_field ~inherited:base_mangling_style td in
    cnstrs |&>> fun ({ vc_name; vc_param; vc_configs; _ } as ctor) ->
      let (discriminator_value, base_mangling_style) =
        Json_config.get_mangled_name_of_discriminator ~inherited:base_mangling_style ctor
      in
      let arg_fname = Json_config.get_mangled_name_of_variant_arg ~inherited:base_mangling_style td.td_configs ctor in
      let cstr = [%expr ([%e estring ~loc discriminator_fname], `str [%e estring ~loc discriminator_value])] in
      let of_record_fields base_mangling_style fields =
        let args = record_to_json_fields ~inherited_codec ~nested ~self_ename base_mangling_style fields in
        [%expr ([%e cstr] :: [%e args])]
      in
      match Json_config.get_variant_style vc_configs with
      | `flatten -> begin
        match vc_param with
        | `no_param ->
          List.return [%expr [ [%e cstr]]]
        | `tuple_like [ va ]
          when Json_config.get_nested_field_style va.va_configs = `spreading ->
            let base_mangling_style = Json_config.get_mangling_style_opt va.va_configs |? base_mangling_style in
            begin match validate_spreading_type va.va_type with
            | `record_decl _ | `variant_decl _->
              let encoder expr =
                expr
                |> get_encoder
                  ~inherited_codec ~nested ~spreading:true
                  base_mangling_style self_ename va.va_type
                |> fst
              in
              List.return [%expr ([%e cstr] :: [%e encoder (evari 0)])]
            end
        | `tuple_like args ->
          let arg_fname = estring ~loc arg_fname in
          let expr_of_arg ?is_field i va =
            let base_mangling_style = Json_config.get_mangling_style_opt va.va_configs |? base_mangling_style in
            let encoder = get_encoder ?is_field ~inherited_codec ~nested ~spreading:false base_mangling_style self_ename va.va_type in
            encoder (evari i)
          in
          begin match args, Json_config.get_tuple_style vc_configs with
          | [], _ -> List.return [%expr [[%e cstr]]]
          | [arg], _ ->
            let expr, is_optional = expr_of_arg ~is_field:true 0 arg in
            if is_optional then
              [ [%expr [[%e cstr]]]; [%expr [[%e cstr]; ([%e arg_fname], [%e expr])]] ]
            else
              [ [%expr [[%e cstr]; ([%e arg_fname], [%e expr])]] ]

          | _, `arr ->
            let args = args |> List.mapi (fun i arg -> expr_of_arg i arg |> fst) in
            List.return [%expr
              [[%e cstr]; ([%e arg_fname], `arr [%e elist ~loc args])]
            ]
          | _, `obj `default ->
            args
            |> encoder_of_objtuple ~loc ~additional_field:cstr (expr_of_arg ~is_field:true)
            |> List.return
          end
        | `inline_record fields ->
          of_record_fields base_mangling_style fields
          |> List.return
        | `reused_inline_record { td_kind; td_configs; _ } ->
          let base_mangling_style =
            Json_config.(get_mangling_style_opt td_configs |? default_mangling_style)
          in
          let fields = td_kind |> function
            | Record_decl fields -> fields
            | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." vc_name
          in
          of_record_fields base_mangling_style fields
          |> List.return
      end
  in
  let encoder_of_type_decl =
    fun
      ~(encoder_kind: [`nested of Coretype.codec | `root])
      codec
      ({ td_kind; td_configs; _ } as td) ->
    let base_mangling_style = Json_config.(get_mangling_style_opt td_configs |? default_mangling_style) in
    let nested, inherited_codec, self_name, map_body =
      match encoder_kind with
      | `nested inherited_codec -> true, inherited_codec, nested_encoder_name inherited_codec td, identity
      | `root -> false, codec, json_encoder_name ~codec td, fun e -> [%expr `obj [%e e]]
    in
    let self_ename = evar ~loc self_name in
    self_name, (match td_kind with
    | Alias_decl ct ->
      encoder_of_coretype codec base_mangling_style self_ename ct
    | Record_decl fields -> [%expr fun [%p record_params fields] ->
      ([%e map_body @@ record_to_json_fields ~inherited_codec ~nested ~self_ename base_mangling_style fields])]
    | Variant_decl ctors ->
      let params = variant_params td_configs ctors in
      let body = variant_body ~inherited_codec ~nested ~self_ename td base_mangling_style ctors in
      List.map2
        (fun p b -> case ~lhs:p ~rhs:(map_body b) ~guard:None)
        params body
      |> pexp_function ~loc)
  in
  let wrap_self_contained e =
    if self_contained then
      match gen_builtin_encoders td with
      | [] -> e
      | es ->
         pexp_let ~loc Nonrecursive
           es e
    else e
  in
  let wrap_nested_type_decls e =
    td |> collect_nested_types_uniq (fun inherited_codec -> function
      | ({ td_kind = Alias_decl _; _ }, _) -> None
      | (td, codec) -> Some (
        encoder_of_type_decl ~encoder_kind:(`nested inherited_codec) codec td
        |> (fun (name, e) ->
          let type_name = Utils.type_name_with_codec ~codec:inherited_codec td.td_name in
          pvar ~loc name, pexp_constraint ~loc e
            [%type: [%t typcons ~loc type_name] -> (string * Kxclib.Json.jv) list])
        |> !! Vb.mk)
    )
    |> function
    | [] -> e
    | es -> pexp_let ~loc Recursive es e
  in
  let (name, encoder_body) =
    encoder_of_type_decl
      ~encoder_kind:`root
      codec
      td
  in
  name, encoder_body |> wrap_nested_type_decls |> wrap_self_contained

let gen_json_encoder :
      ?self_contained:bool
      -> ?codec:Coretype.codec
      -> type_decl
      -> value_binding =
  fun ?self_contained ?codec td ->
    let loc = Location.none in
    let name, body = gen_json_encoder' ?self_contained ?codec td in
    Vb.mk
      ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
      (pvar ~loc name)
      (pexp_constraint ~loc body
        [%type: [%t typcons ~loc td.td_name] -> Kxclib.Json.jv])

let gen_json_decoder_impl :
      ?self_contained:bool
      -> type_decl
      -> expression =
  fun ?(self_contained=false) td ->
  let loc = Location.none in
  let vari i = "x"^string_of_int i in
  let evari i = evar ~loc (vari i) in
  let pvari i = pvar ~loc (vari i) in
  let param_e = evar ~loc "param" in
  let param_p = pvar ~loc "param" in

  let bindoj_orig = "__bindoj_orig" in
  let ebindoj_orig = evar ~loc bindoj_orig in
  let pbindoj_orig = pvar ~loc bindoj_orig in

  let nested_decoder_name codec td =
    (match codec with
    | `default -> ""
    | `open_ m | `in_module m -> String.lowercase_ascii m ^ "__")
    ^ json_decoder_name td ^ "_nested"
  in
  let get_decoder ~inherited_codec base_mangling_style self_ename = function
    | `direct ct -> decoder_of_coretype inherited_codec base_mangling_style self_ename ct
    | `nested ({ td_kind = Alias_decl ct; _ }, codec) ->
      let inherited_codec = inherit_codec inherited_codec codec in
      decoder_of_coretype inherited_codec base_mangling_style self_ename ct
    | `nested (td, codec) ->
      let inherited_codec = inherit_codec inherited_codec codec in
      evar ~loc (nested_decoder_name inherited_codec td)
  in
  let record_bindings : self_ename:expression -> inherited_codec:Coretype.codec -> Json_config.json_mangling_style -> record_field list -> (pattern * expression) list =
    fun ~self_ename ~inherited_codec base_mangling_style -> List.mapi (fun i field ->
      pvari i, (
        let (json_field_name, base_mangling_style) = Json_config.get_mangled_name_of_field ~inherited:base_mangling_style field in
        let json_field = estring ~loc json_field_name in
        let nested_style = Json_config.get_nested_field_style field.rf_configs in
        let decoder = get_decoder ~inherited_codec base_mangling_style self_ename field.rf_type in
        match nested_style, field.rf_type with
        | `spreading, _ ->
          begin match validate_spreading_type field.rf_type with
          | `record_decl _ | `variant_decl _ ->
            [%expr [%e decoder] path [%e ebindoj_orig]]
          end
        | `nested, (`direct ct | `nested ({ td_kind = Alias_decl ct; _ }, _)) when Coretype.is_option ct ->
          [%expr
            List.assoc_opt [%e json_field] [%e param_e]
            |> Option.value ~default:`null
            |> [%e decoder] (`f [%e json_field] :: path)]
        | `nested, _ ->
          let error_message = sprintf "mandatory field '%s' does not exist" json_field_name in
          [%expr
            List.assoc_opt [%e json_field] [%e param_e]
            |> [%e opt_to_result ~loc [%expr ([%e estring ~loc error_message], path)]]
            >>= [%e decoder] (`f [%e json_field] :: path)
          ]))
  in
  let record_body : gen_typcons:bool -> inherited_codec:Coretype.codec -> type_decl -> record_field list -> expression =
    fun ~gen_typcons ~inherited_codec { td_name; _ } fields ->
    pexp_record ~loc
      (fields |> List.mapi (fun i { rf_name; _; } ->
        (lidloc ~loc rf_name, [%expr [%e evari i]])))
      None
    |> fun e -> (
      if gen_typcons then
        let type_name = type_name_with_codec ~codec:inherited_codec td_name in
        pexp_constraint ~loc e (typcons ~loc type_name)
      else e)
  in
  let object_is_expected_error_record, object_is_expected_error_variant =
    let object_is_expected_error label jv =
      let error_message = sprintf "an object is expected for a %s value, but the given is of type '%%s'" label in
      [%expr Error ( Printf.sprintf [%e estring ~loc error_message] Kxclib.Json.(string_of_jv_kind (classify_jv [%e jv])), path)]
    in
    object_is_expected_error "record", object_is_expected_error "variant"
  in
  let variant_body : self_ename:expression -> inherited_codec:Coretype.codec -> gen_typcons:bool -> type_decl -> Json_config.json_mangling_style -> variant_constructor list -> (pattern * expression) list =
    fun ~self_ename ~inherited_codec ~gen_typcons ({ td_name; td_configs; _ } as td) base_mangling_style cstrs ->
    let discriminator_fname = Json_config.get_mangled_name_of_discriminator_field ~inherited:base_mangling_style td in
    let discriminator_fname_p = pstring ~loc discriminator_fname in
    cstrs |&> (fun ({ vc_name; vc_param; vc_configs; _ } as ctor) ->
      let (discriminator_value, base_mangling_style) =
        Json_config.get_mangled_name_of_discriminator ~inherited:base_mangling_style ctor
      in
      let arg_fname = Json_config.get_mangled_name_of_variant_arg ~inherited:base_mangling_style td.td_configs ctor in
      let cstr_p tail = [%pat? `obj (([%p discriminator_fname_p], `str [%p pstring ~loc discriminator_value])::[%p tail])] in
      let construct name args =
        match Caml_config.get_variant_type td_configs with
        | `regular -> Exp.construct (lidloc name) args
        | `polymorphic -> Exp.variant name args
      in
      let type_constraint body =
        if gen_typcons then
          let self_name = type_name_with_codec ~codec:inherited_codec td_name in
          pexp_constraint ~loc body (typcons ~loc self_name)
        else body
      in
      let tuple_like_body args : expression =
        [%expr Ok
          [%e construct
            vc_name
            (Some (pexp_tuple ~loc (List.mapi (fun i _ -> evari i) args))) |> type_constraint]]
      in
      match Json_config.get_variant_style vc_configs with
      | `flatten ->
        let of_record_fields ~label base_mangling_style fields =
          match fields with
          | [] -> cstr_p [%pat? _], construct vc_name None
          | _ ->
            let bindings = record_bindings ~self_ename ~inherited_codec base_mangling_style fields in
            let body =
              match Caml_config.get_variant_type td_configs with
              | `regular -> record_body ~gen_typcons:false ~inherited_codec td fields
              | `polymorphic -> failwith' "case '%s' with an %s cannot be used in a polymorphic variant" vc_name label
            in
            cstr_p (fields
            |> List.for_all (fun { rf_configs; _ } ->
              Json_config.get_nested_field_style rf_configs = `spreading)
            |> function | true ->  [%pat? _] | false -> param_p
            ),
            bind_results bindings [%expr Ok [%e construct vc_name (Some body) |> type_constraint]]
        in
        begin match vc_param with
          | `no_param -> cstr_p [%pat? _], [%expr Ok [%e construct vc_name None |> type_constraint]]
          | `tuple_like ([ va ] as args)
            when Json_config.get_nested_field_style va.va_configs = `spreading ->
              let base_mangling_style = Json_config.get_mangling_style_opt va.va_configs |? base_mangling_style in
              begin match validate_spreading_type va.va_type with
              | `record_decl _ | `variant_decl _ ->
                let decoder = get_decoder ~inherited_codec base_mangling_style self_ename va.va_type in
                let body = tuple_like_body args in
                cstr_p [%pat? _], bind_results [
                  pvari 0, [%expr [%e decoder] path [%e ebindoj_orig]]
                ] body
              end
          | `tuple_like args ->
            let body = tuple_like_body args in
            let arg_to_decoder va =
              let base_mangling_style = Json_config.get_mangling_style_opt va.va_configs |? base_mangling_style in
              get_decoder ~inherited_codec base_mangling_style self_ename va.va_type
            in
            let is_optional = function
              | { va_type = `direct ct
              | `nested ({ td_kind = Alias_decl ct; _ }, _); _ } ->
                Coretype.is_option ct
              | _ -> false
            in
            let path_arg = [%expr `f [%e estring ~loc arg_fname] :: path ] in
            begin match Json_config.get_tuple_style vc_configs, args with
              | _, [] -> cstr_p [%pat? _], [%expr Ok [%e construct vc_name None]]
              | `obj `default, _ :: _ :: _ ->
                let bindings =
                  args |> List.mapi (fun i arg ->
                    let label_name = tuple_index_to_field_name i in
                    let label_name_e = estring ~loc label_name in
                    let assoc_opt = [%expr List.assoc_opt [%e label_name_e] [%e param_e]] in
                    let decoder = [%expr [%e arg_to_decoder arg] (`f [%e label_name_e] :: path) ] in
                    pvari i, (
                      if is_optional arg then
                        [%expr
                          [%e assoc_opt]
                          |> Option.value ~default:`null
                          |> [%e decoder]]
                      else
                        let error_message = sprintf "mandatory field '%s' does not exist" label_name in
                        [%expr
                          [%e assoc_opt]
                          |> [%e opt_to_result ~loc [%expr ([%e estring ~loc error_message], path)]]
                          >>= [%e decoder]]
                    ))
                in
                cstr_p param_p, bind_results bindings body
              | _, [ va ] when is_optional va ->
                cstr_p param_p, bind_results [
                  pvari 0, [%expr
                    List.assoc_opt [%e estring ~loc arg_fname] [%e param_e]
                    |> Option.value ~default:`null
                    |> [%e arg_to_decoder va] [%e path_arg]]
                ] [%expr
                  Ok ([%e construct vc_name (Some (evari 0))])
                ]
              | _, _ ->
                cstr_p param_p, (
                  match args with
                  | [ arg ] ->
                    [ [%pat? Some arg],
                      bind_results
                        [ pvari 0, [%expr [%e arg_to_decoder arg] [%e path_arg] arg]]
                        body; ]
                  | _ ->
                    [ [%pat? Some (`arr [%p plist ~loc (List.mapi (fun i _ -> pvari i) args)])],
                      bind_results
                        (args |> List.mapi (fun i arg -> pvari i, [%expr
                            [%e arg_to_decoder arg]
                              (`i [%e eint ~loc i] :: [%e path_arg])
                              [%e evari i]]))
                        body;

                      [%pat? Some (`arr xs)],
                      [%expr Error (
                        Printf.sprintf
                          [%e sprintf "expecting an array of length %d, but the given has a length of %%d" (List.length args) |> estring ~loc ]
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
            of_record_fields ~label:"inline record" base_mangling_style fields
          | `reused_inline_record { td_kind; td_configs; _ } ->
            let base_mangling_style =
              Json_config.(get_mangling_style_opt td_configs |? default_mangling_style)
            in
            let fields =
              td_kind |> function
              | Record_decl fields -> fields
              | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." vc_name
            in
            of_record_fields ~label:"reused inline record" base_mangling_style fields
        end)
    |> fun cases ->
      let unexpected_discriminator_error_message_format =
        sprintf "given discriminator field value '%%s' is not one of [ %s ]"
          (cstrs
            |&> (fun ctor ->
              Json_config.get_mangled_name_of_discriminator ~inherited:base_mangling_style ctor
              |> fst
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
  let decoder_of_type_decl =
    fun
      ?(inherited_codec=`default)
      ~(decoder_kind : [`nested of Coretype.codec | `root of string ])
      ({ td_kind; td_configs; _ } as td) ->
    let self_name, gen_typcons =
      match decoder_kind with
      | `root name -> name, false
      | `nested _ -> nested_decoder_name inherited_codec td, true
    in
    let base_mangling_style = Json_config.(get_mangling_style_opt td_configs |? default_mangling_style) in
    let self_ename = evar ~loc self_name in
    self_name, (match td_kind with
    | Alias_decl ct ->
      decoder_of_coretype inherited_codec base_mangling_style self_ename ct
    | Record_decl fields ->
      let bindings = record_bindings ~self_ename ~inherited_codec base_mangling_style fields in
      let body = record_body ~gen_typcons ~inherited_codec td fields in
      bind_results bindings [%expr Ok [%e body]]
      |> fun body -> [%expr fun path [%p pbindoj_orig] ->
        match [%e ebindoj_orig] with
        | `obj [%p param_p] -> [%e body]
        | jv -> [%e object_is_expected_error_record [%expr jv]]
      ]
    | Variant_decl ctors ->
      let discriminator_fname = Json_config.get_variant_discriminator td_configs in
      [%expr fun path [%p pbindoj_orig] -> [%e
        variant_body ~gen_typcons ~self_ename ~inherited_codec td base_mangling_style ctors
        |&> (fun (pat, body) -> case ~lhs:pat ~rhs:(body) ~guard:None)
        |> pexp_match ~loc [%expr
          Kxclib.Jv.pump_field [%e estring ~loc discriminator_fname] [%e ebindoj_orig]
        ]]]
    )
  in
  let wrap_self_contained e =
    if self_contained then
      match gen_builtin_decoders td with
      | [] -> e
      | es ->
         pexp_let ~loc Nonrecursive
           es e
    else e
  in
  let wrap_nested_type_decls e =
    td |> collect_nested_types_uniq (fun inherited_codec -> function
      | ({ td_kind = Alias_decl _; _ }, _) -> None
      | (td, codec) -> Some (
        decoder_of_type_decl ~decoder_kind:(`nested codec) ~inherited_codec td
        |> (fun (name, e) -> (pvar ~loc name, e))
        |> !! Vb.mk)
    )
    |> function
    | [] -> e
    | es -> pexp_let ~loc Recursive es e
  in
  let (self_name, encoder_body) =
    decoder_of_type_decl
      ~decoder_kind:(`root "of_json_impl")
      td
  in
  [%expr let rec [%p pvar ~loc self_name] = [%e
      encoder_body |> wrap_nested_type_decls |> wrap_self_contained]
    in [%e evar ~loc self_name]]

let gen_json_decoder_result' :
      ?self_contained:bool
      -> ?json_shape_explanation_style:[
        | `inline of json_shape_explanation_resolution option
        | `reference ]
      -> ?codec:Coretype.codec
      -> type_decl
      -> string * expression =
  fun ?(self_contained=false) ?(json_shape_explanation_style = `inline None) ?(codec=`default) td ->
  let { td_name; _ } = td in
  let loc = Location.none in
  let self_name = (json_decoder_name ~codec td) ^ "'" in

  let body =
    [%expr fun ?(path=[]) x ->
      [%e gen_json_decoder_impl ~self_contained td] path x
      |> Result.map_error (fun (msg, path) ->
        (msg, path, [%e
          match json_shape_explanation_style with
          | `inline json_shape_explanation_resolution ->
            explain_encoded_json_shape ?json_shape_explanation_resolution td
          | `reference ->
            let json_shape_name = "json_shape_explanation" in
            evar (match codec with
              | `default -> sprintf "%s_%s" td_name json_shape_name
              | `open_ m -> sprintf "%s.%s_%s" m td_name json_shape_name
              | `in_module _ -> json_shape_name)
        ]))]
  in
  (self_name, body)

let gen_json_decoder_result :
      ?self_contained:bool
      -> ?json_shape_explanation_style:[
        | `inline of json_shape_explanation_resolution option
        | `reference ]
      -> ?codec:Coretype.codec
      -> type_decl
      -> value_binding =
  fun ?self_contained ?json_shape_explanation_style ?codec td ->
    let loc = Location.none in
    let name, body =
      gen_json_decoder_result'
        ?self_contained
        ?json_shape_explanation_style
        ?codec td
    in
    Vb.mk
      ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
      (pvar ~loc name)
      (pexp_constraint ~loc body
        [%type: [%t typcons ~loc td.td_name] Bindoj_runtime.json_full_decoder])

let gen_json_decoder_option' :
    ?implementation_style: [
      | `refer_existing_result_variant_json_decoder
      | `embed_full_implementation of [
        | `self_contained
        | `non_self_contained
      ]
    ]
      -> ?codec:Coretype.codec
      -> type_decl
      -> string * expression =
  fun ?(implementation_style=`embed_full_implementation `non_self_contained) ?(codec=`default) td ->
  let loc = Location.none in
  let self_name = json_decoder_name ~codec td in
  let body =
    match implementation_style with
    | `refer_existing_result_variant_json_decoder ->
      [%expr fun x -> [%e evar (self_name ^ "'")] x |> Result.to_option]
    | `embed_full_implementation self_contained ->
      [%expr fun x -> [%e
        gen_json_decoder_impl td
          ~self_contained:(self_contained = `self_contained)
        ] [] x |> Result.to_option
      ]
  in
  (self_name, body)

let gen_json_decoder_option :
    ?implementation_style: [
      | `refer_existing_result_variant_json_decoder
      | `embed_full_implementation of [
        | `self_contained
        | `non_self_contained
      ]
    ]
      -> ?codec:Coretype.codec
      -> type_decl
      -> value_binding =
  fun ?implementation_style ?codec td ->
    let loc = Location.none in
    let (name, body) = gen_json_decoder_option' ?implementation_style ?codec td in
    Vb.mk
      ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
      (pvar ~loc name)
      (pexp_constraint ~loc body
        [%type: Kxclib.Json.jv -> [%t typcons ~loc td.td_name] option])

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
          let base_mangling_style =
            Json_config.(get_mangling_style_opt td.td_configs |? default_mangling_style)
          in
          ctors
          |&> (fun ({ vc_name; vc_param; _ } as ctor) ->
            let arg = if vc_param = `no_param then None else Some(Pat.any ()) in
            let pat =
              if poly then Pat.variant vc_name arg
              else Pat.construct (lidloc vc_name) arg
            in
            let discriminator_value =
              Json_config.get_mangled_name_of_discriminator ~inherited:base_mangling_style ctor
              |> fst |> estring ~loc
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
      gen_json_decoder_option
        ~implementation_style:`refer_existing_result_variant_json_decoder
        ?codec td; ]
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
  let get_id self_mangled_type_name =
    if openapi then None (* OpenAPI v3 does not support `id` *)
    else Some ("#" ^ self_mangled_type_name)
  in

  let docopt = function `docstr s -> Some s | `nodoc -> None in

  let convert_coretype ?alias_decl_props ~self_name ~self_mangled_type_name base_mangling_style ?description (ct: coretype) =
    let base_mangling_style =
      Json_config.get_mangling_style_opt ct.ct_configs |? base_mangling_style
    in
    let tuple_style = Json_config.get_tuple_style ct.ct_configs in
    let rec go ?alias_decl_props =
      let schema, id, title = alias_decl_props |? (None, None, None) in
      let open Coretype in
      function
      | Prim `unit -> Schema_object.integer ?schema ?id ?title ?description ~minimum:1 ~maximum:1 ()
      | Prim `bool -> Schema_object.boolean ?schema ?id ?title ?description ()
      | Prim `int -> Schema_object.integer ?schema ?id ?title ?description ()
      | Prim `int53p -> Schema_object.integer ?schema ?id ?title ?description ()
      | Prim `float -> Schema_object.number ?schema ?id ?title ?description ()
      | Prim `string -> Schema_object.string ?schema ?id ?title ?description ()
      | Prim `uchar -> Schema_object.string ?schema ?id ?title ?description ~minLength:1 ~maxLength:1 ()
      | Prim `byte -> Schema_object.integer ?schema ?id ?title ?description ~minimum:0 ~maximum:255 ()
      | Prim `bytes -> Schema_object.string ?schema ?id ?title ?description ~format:`byte ~pattern:base64_regex ()
      | Uninhabitable -> Schema_object.null ?schema ?id ?title ?description ()
      | Ident { id_name; _ } ->
        let name =
          ct.ct_configs
          |> Json_config.get_name_opt |? id_name
          |> Json_config.mangled `type_name base_mangling_style
        in
        if openapi then (* in OpenAPI, types live in #/components/schemas/ *)
          Schema_object.ref ("#/components/schemas/" ^ name)
        else
          Schema_object.ref ("#" ^ name)
      | Option t ->
        Schema_object.option (go ?alias_decl_props t)
      | Tuple ts ->
        begin match tuple_style with
        | `arr ->
          if openapi then
            raise (Incompatible_with_openapi_v3 (
              sprintf "OpenAPI v3 does not support tuple validation (in type '%s')" self_name))
          else
            Schema_object.tuple ?schema ?id ?title ?description (ts |> List.map go)
        | `obj `default ->
          Schema_object.record ?schema ?id ?title ?description (ts |> List.mapi (fun i x ->
            tuple_index_to_field_name i, go x
          ))
        end
      | List t ->
        Schema_object.array ?schema ?id ?title ?description ~items:(`T (go t)) ()
      | Map (`string, t) ->
        Schema_object.obj ?schema ?id ?title ?description ~additionalProperties:(`T (go t)) ()
      | StringEnum cases ->
        let enum = cases |> List.map (
          Json_config.get_mangled_name_of_string_enum_case ~inherited:base_mangling_style
          &> (fun case -> `str case)) in
        Schema_object.string ?schema ?id ?title ?description ~enum ()
      | Self ->
        let name =
          ct.ct_configs
          |> Json_config.get_name_opt
          >? (Json_config.mangled `type_name base_mangling_style
              &> Bindoj_common.Mangling.(escape ~charmap:charmap_js_identifier))
          |? self_mangled_type_name
        in
        if openapi then (* in OpenAPI, types live in #/components/schemas/ *)
          Schema_object.ref ("#/components/schemas/" ^ name)
        else
          Schema_object.ref ("#" ^ name)
    in
    match ct.ct_configs |> Configs.find_foreign_type_expr Json_config.json_schema with
    | Some schema -> schema
    | None -> go ?alias_decl_props ct.ct_desc
  in

  let variant_or_record_of_fields ?schema ?id ?title ?(additional_fields = []) ~doc =
    function
    | [] -> failwith "The given list must not be empty"
    | [ (_, _, fields) ] ->
      Schema_object.record
        ?schema
        ?title
        ?description:(docopt doc)
        ?id
        (fields @ additional_fields)
    | fs ->
      fs |&> (fun (name, doc, fields) ->
        Schema_object.record
          ?title:name
          ?description:(docopt doc)
          (fields @ additional_fields))
      |> Schema_object.oneOf
        ?schema
        ?title
        ?description:(docopt doc)
        ?id
  in
  let rec fields_to_t ~self_name ~self_mangled_type_name base_mangling_style fields =
    fields |&> (fun field ->
      let field_name, base_mangling_style =
        Json_config.get_mangled_name_of_field ~inherited:base_mangling_style field
      in
      let { rf_type; rf_doc; rf_configs; _ } = field in
      match Json_config.get_nested_field_style rf_configs with
      | `nested ->
        List.return (match rf_type with
        | `direct ct ->
          [ field_name, convert_coretype ~self_name ~self_mangled_type_name ?description:(docopt rf_doc) base_mangling_style ct]
        | `nested (td, _) ->
          [ field_name, type_decl_to_t ~gen_title:false td ]
        )
      | `spreading ->
        begin match validate_spreading_type rf_type with
        | `record_decl({ td_name=self_name; _ } as td, fields, _) ->
          let (self_mangled_type_name, base_mangling_style) = Json_config.get_mangled_name_of_type td in
          fields_to_t ~self_name ~self_mangled_type_name base_mangling_style fields
        | `variant_decl({ td_name=self_name; _ } as td, ctors, _) ->
          let (self_mangled_type_name, base_mangling_style) = Json_config.get_mangled_name_of_type td in
          variant_to_t' ~self_name ~self_mangled_type_name base_mangling_style td ctors
          |&> (fun (_, _, x) -> x)
        end
    ) |> List.fold_left (fun result ->
      List.fmap (fun fields -> result |&> List.rev_append fields)
    ) [ [] ]
    |&> List.rev
  and record_to_t ?schema ?id ?title ?additional_fields ~self_name ~self_mangled_type_name ~doc base_mangling_style fields =
    fields_to_t ~self_name ~self_mangled_type_name base_mangling_style fields
    |&> (fun x -> (None, `nodoc, x))
    |> variant_or_record_of_fields ?schema ?id ?title ?additional_fields ~doc
  and variant_to_t' ~self_name ~self_mangled_type_name base_mangling_style td ctors =
    let discriminator_fname = Json_config.get_mangled_name_of_discriminator_field ~inherited:base_mangling_style td in
    ctors |&>> fun ({ vc_param; vc_doc; vc_configs; _ } as ctor) ->
      let (discriminator_value, base_mangling_style) =
        Json_config.get_mangled_name_of_discriminator ~inherited:base_mangling_style ctor
      in
      let discriminator_field =
        let enum = [`str discriminator_value] in
        [discriminator_fname, Schema_object.string ~enum ()]
      in
      match Json_config.get_variant_style vc_configs with
      | `flatten ->
        begin match vc_param with
        | `no_param | `tuple_like [] ->
          List.return (Some discriminator_value, vc_doc, discriminator_field)
        | `tuple_like [ va ] when Json_config.get_nested_field_style va.va_configs = `spreading ->
          begin match validate_spreading_type va.va_type with
          | `record_decl ({ td_name = self_name; _ } as td, fields, _) ->
            let (self_mangled_type_name, base_mangling_style) = Json_config.get_mangled_name_of_type td in
            fields_to_t ~self_name ~self_mangled_type_name base_mangling_style fields
            |&> (fun x -> (Some discriminator_value, `nodoc, x @ discriminator_field))
          | `variant_decl ({ td_name = self_name; _ } as td, ctors, _) ->
            let (self_mangled_type_name, base_mangling_style) = Json_config.get_mangled_name_of_type td in
            variant_to_t' ~self_name ~self_mangled_type_name base_mangling_style td ctors
            |&> (fun (a, b, c) ->
              let title = match a with | None -> discriminator_value | Some a -> discriminator_value^"_"^a in
              (Some title, b, c @ discriminator_field))
          end
        | `tuple_like ((t :: ts) as args) ->
          let arg_name =
            Json_config.get_mangled_name_of_variant_arg
              ~inherited:base_mangling_style
              td.td_configs
              ctor
          in
          let convert_variant_argument va =
            let base_mangling_style = Json_config.get_mangling_style_opt va.va_configs |? base_mangling_style in
            match va.va_type with
            | `direct ct -> convert_coretype ~self_name ~self_mangled_type_name base_mangling_style ct
            | `nested (td, _) -> type_decl_to_t ~gen_title:false td
          in
          let arg_field =
            match ts, Json_config.get_tuple_style vc_configs with
            | [], _ -> [arg_name, convert_variant_argument t]
            | _, `arr ->
              if openapi then
                raise (Incompatible_with_openapi_v3 (
                  sprintf "OpenAPI v3 does not support tuple validation (in type '%s')" self_name))
              else
                let ts = args |> List.map convert_variant_argument in
                [arg_name, Schema_object.tuple ts]
            | _, `obj `default ->
              args |> List.mapi (fun i t ->
                Json_config.tuple_index_to_field_name i, convert_variant_argument t
              )
          in
          List.return (Some discriminator_value, vc_doc, arg_field @ discriminator_field)
        | `inline_record fields ->
          (fields_to_t ~self_name ~self_mangled_type_name base_mangling_style fields)
          |> List.map (fun xs ->
            Some discriminator_value, vc_doc, xs @ discriminator_field)
        | `reused_inline_record { td_kind; td_configs; _ } ->
          let base_mangling_style =
            Json_config.(get_mangling_style_opt td_configs |? default_mangling_style)
          in
          let fields = td_kind |> function
            | Record_decl fields -> fields
            | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." ctor.vc_name
          in
          (fields_to_t ~self_name ~self_mangled_type_name base_mangling_style fields)
          |> List.map (fun xs ->
            Some discriminator_value, vc_doc, xs @ discriminator_field)
        end
  and variant_to_t ?schema ?id ?title ~self_name ~self_mangled_type_name ~doc td base_mangling_style ctors =
    variant_to_t' ~self_name ~self_mangled_type_name base_mangling_style td ctors
    |> variant_or_record_of_fields ?schema ?id ?title ~doc
  and type_decl_to_t ?schema ?(id=false) ?base_mangling_style ~gen_title ({ td_name = name; td_doc=doc; td_kind; _} as td) =
    let self_name = name in
    let (self_mangled_type_name, base_mangling_style) = Json_config.get_mangled_name_of_type ?inherited:base_mangling_style td in
    let id = if id then get_id self_mangled_type_name else None in
    let title = if gen_title then Some self_mangled_type_name else None in
    match td_kind with
    | Record_decl fields ->
      record_to_t ?schema ?id ?title ~self_name ~self_mangled_type_name ~doc base_mangling_style fields
    | Variant_decl ctors ->
      variant_to_t ?schema ?id ?title ~self_name ~self_mangled_type_name ~doc td base_mangling_style ctors
    | Alias_decl cty ->
      convert_coretype
        ~alias_decl_props:(schema, id, title)
        ~self_name
        ~self_mangled_type_name
        ?description:(docopt doc)
        base_mangling_style
        cty
  in
  let schema =
    if openapi then None (* OpenAPI v3 does not support `$schema` *)
    else Some Schema_object.schema in
  type_decl_to_t ?schema ~id:true ~gen_title:true ?base_mangling_style:None

let gen_openapi_schema : type_decl -> Schema_object.t = gen_json_schema ~openapi:true
