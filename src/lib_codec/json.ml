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
open Bindoj_base
open Typed_type_desc
open Runtime

module Config = struct
  type json_variant_style = [
    | `flatten
  (*| `nested *)
  (*| `tuple *)
  ]

  type json_tuple_style = [
    | `arr
    | `obj of [`default]
  ]

  type ('pos, 'kind) config +=
    | Config_json_name : string -> ('pos, [`json_name]) config
    | Config_json_variant_style :
      json_variant_style -> ([`variant_constructor], [`json_variant_style]) config
    | Config_json_variant_discriminator :
      string -> ([`type_decl], [`json_variant_discriminator]) config
    | Config_json_tuple_style :
      json_tuple_style -> ([< `variant_constructor | `coretype], [`json_tuple_style]) config
    | Config_json_custom_encoder : string -> ([`coretype], [`json_custom_encoder]) config
    | Config_json_custom_decoder : string -> ([`coretype], [`json_custom_decoder]) config

  let tuple_index_to_field_name i = "_" ^ string_of_int i

  module Json_config = struct
    let name name = Config_json_name name
    let get_name default =
      Configs.find_or_default ~default (function
        | Config_json_name s -> Some s
        | _ -> None
      )
    let default_name_of_variant_arg = "arg"

    let default_variant_style : json_variant_style = `flatten
    let variant_style style = Config_json_variant_style style
    let get_variant_style =
      Configs.find_or_default ~default:default_variant_style (function
        | Config_json_variant_style s -> Some s
        | _ -> None
      )

    let default_variant_discriminator = "kind"
    let variant_discriminator discriminator = Config_json_variant_discriminator discriminator
    let get_variant_discriminator =
      Configs.find_or_default ~default:default_variant_discriminator (function
        | Config_json_variant_discriminator s -> Some s
        | _ -> None
      )

    let default_tuple_style = `arr
    let tuple_style style = Config_json_tuple_style style
    let get_tuple_style configs =
      Configs.find_or_default ~default:default_tuple_style (function
        | Config_json_tuple_style s -> Some s
        | _ -> None
      ) configs

    let custom_encoder encoder_name = Config_json_custom_encoder encoder_name
    let get_custom_encoder =
      Configs.find (function | Config_json_custom_encoder s -> Some s | _ -> None)

    let custom_decoder decoder_name = Config_json_custom_decoder decoder_name
    let get_custom_decoder =
      Configs.find (function | Config_json_custom_decoder s -> Some s | _ -> None)
  end
end

include Config

type jv = Kxclib.Json.jv

open MonadOps(Option)

let rec of_json ~env:(env: boxed_type_decl StringMap.t) (a: 'a typed_type_decl) (jv: jv) : 'a option =
  let { td_configs; td_kind; _ } = Typed.decl a in
  let try_opt f = try f () with Invalid_argument _msg -> None in
  let parse_obj_style_tuple (conv: _ -> jv -> Expr.t option) (ts: _ list) (fields: jv StringMap.t) =
    ts
    |> List.mapi (fun i t ->
      fields |> StringMap.find_opt (tuple_index_to_field_name i)
      >>= fun jv -> conv t jv)
    |> sequence_list
  in
  let expr_of_json (ct: coretype) (jv: jv) : Expr.t option =
    let rec go (d: Coretype.desc) (jv: jv) =
      match d, jv with
      | Prim `unit, `null -> Expr.Unit |> some
      | Prim `bool, `bool b -> Expr.Bool b |> some
      | Prim `int, `num x ->
        if Float.is_integer x then Expr.Int (int_of_float x) |> some
        else none
      | Prim `float, `num x -> Expr.Float x |> some
      | Prim `string, `str s -> Expr.String s |> some
      | Prim `uchar, `str s ->
        if String.length s = 1 then
          Expr.Uchar (Uchar.of_char (String.get s 0)) |> some
        else None
      | Prim `byte, `num x ->
        let x = int_of_float x in
        if 0 <= x && x <= 255 then Expr.Byte (char_of_int x) |> some
        else None
      | Prim `bytes,  `str s ->
        try_opt (fun () -> Expr.Bytes (Kxclib.Base64.decode s) |> some)
      | Uninhabitable, `null -> Expr.Unit |> some
      | Ident i, _ ->
        begin match StringMap.find_opt i.id_name env with
        | Some boxed ->
          let t = Typed.unbox boxed in
          of_json ~env t jv >>= fun value -> Expr.Refl (Typed.to_refl t, value) |> some
        | None -> invalid_arg' "type '%s' not found in env" i.id_name
        end
      | Self, _ ->
        of_json ~env a jv >>= fun value -> Expr.Refl (Typed.to_refl a, value) |> some
      | List t, `arr xs ->
        xs |> List.map (go t) >>=* fun xs -> Expr.List xs |> some
      | Tuple ts, _ ->
        try_opt (fun () ->
          match jv, Json_config.get_tuple_style ct.ct_configs with
          | `arr xs, `arr -> List.map2 go ts xs >>=* fun xs -> Expr.Tuple xs |> some
          | `obj fields, `obj `default ->
            parse_obj_style_tuple go ts (StringMap.of_list fields) |> Option.map (fun xs -> Expr.Tuple xs)
          | _, _ -> None
        )
      | Map (`string, t), `obj fields ->
        fields |> List.map (fun (name, x) -> go t x |> Option.map (fun v -> name, v))
        >>=* fun fields -> Expr.Map fields |> some
      | Option _, `null -> Some Expr.None
      | Option t, _ -> go t jv |> Option.map (fun x -> Expr.Some x)
      | StringEnum cases, `str s when List.mem s cases -> Expr.StringEnum s |> some
      | _, _ -> None
    in
    go ct.ct_desc jv
  in
  let record_fields_of_json (fields: record_field list) (jv: jv) : Expr.t StringMap.t option =
    match jv with
    | `obj obj ->
      let obj = StringMap.of_list obj in
      fields |> List.map (fun { rf_name; rf_type; _} ->
        match obj |> StringMap.find_opt rf_name with
        | None when Coretype.is_option rf_type -> Some (rf_name, Expr.None)
        | Some jv -> expr_of_json rf_type jv >>= fun expr -> Some (rf_name, expr)
        | None -> None
      ) |> sequence_list |> Option.map StringMap.of_list
    | _ -> None
  in
  let fail () = invalid_arg "inconsistent type_decl and reflection result" in
  let constructor_of_json (ctors: variant_constructor list) (ref_ctors: 'a Refl.constructor StringMap.t) (jv: jv) : 'a option =
    let discriminator = Json_config.get_variant_discriminator td_configs in
    match jv with
    | `obj obj ->
      let obj = StringMap.of_list obj in
      begin match obj |> StringMap.find_opt discriminator with
      | Some (`str ctor_name) ->
        ctors |> List.find_opt (fun ctor -> ctor.vc_name = ctor_name) >>= (fun ctor ->
          let ref_ctor = ref_ctors |> StringMap.find_opt ctor_name |> Option.v' fail in
          let arg_fname = Json_config.get_name Json_config.default_name_of_variant_arg ctor.vc_configs in
          match Json_config.get_variant_style ctor.vc_configs with
          | `flatten ->
            begin match ctor.vc_param, ref_ctor with
            | `no_param, NoParam { value } -> Some value
            | `tuple_like ts, TupleLike { mk; _ } ->
              begin match Json_config.get_tuple_style ctor.vc_configs, ts with
              | `obj `default, _ :: _ :: _ ->
                parse_obj_style_tuple expr_of_json ts obj >>= mk
              | _, _ ->
                obj |> StringMap.find_opt arg_fname >>= (fun arg ->
                  match ts, arg with
                  | [], _ -> mk []
                  | [t], _ -> expr_of_json t arg >>= fun expr -> mk [expr]
                  | ts, `arr xs -> try_opt (fun () -> List.map2 expr_of_json ts xs >>=* mk)
                  | _ -> None
                )
              end
            | `inline_record fields, InlineRecord { mk; _ } ->
              record_fields_of_json fields jv >>= mk
            | _ -> fail ()
            end
        )
      | _ -> None
      end
    | _ -> None
  in
  match td_kind, Typed.reflect a with
  | Alias_decl ct, Alias { mk; _ } ->
    expr_of_json ct jv >>= fun expr -> mk expr
  | Record_decl fields, Record { mk; _ } ->
    record_fields_of_json fields jv >>= fun fields -> mk fields
  | Variant_decl ctors, Variant { constructors; _ } ->
    constructor_of_json ctors constructors jv
  | _, _ -> fail ()

let rec to_json ~env:(env: boxed_type_decl StringMap.t) (a: 'a typed_type_decl) (value: 'a) : jv =
  let fail msg = invalid_arg' "inconsistent type_decl and reflection result (%s)" msg in
  let { td_configs; td_kind; _ } = Typed.decl a in

  let expr_to_json (ct: coretype) (expr: Expr.t) : jv =
    let rec go (d: Coretype.desc) (e: Expr.t) =
      match d, e with
      | Prim `unit,   Expr.Unit -> `null
      | Prim `bool,   Expr.Bool b -> `bool b
      | Prim `int,    Expr.Int i -> `num (float_of_int i)
      | Prim `float,  Expr.Float f -> `num f
      | Prim `string, Expr.String s -> `str s
      | Prim `uchar,  Expr.Uchar c -> `str (String.of_list [Uchar.to_char c])
      | Prim `byte,   Expr.Byte c -> `num (float_of_int (int_of_char c))
      | Prim `bytes,  Expr.Bytes bs -> `str (Kxclib.Base64.encode bs)
      | Uninhabitable, Expr.Unit -> `null
      | Ident i, Expr.Refl (_, x) ->
        begin match StringMap.find_opt i.id_name env with
        | Some boxed -> to_json ~env (Typed.unbox boxed) (Obj.magic x)
        | None -> invalid_arg' "type '%s' not found in env" i.id_name
        end
      | Self, Expr.Refl (_, x) -> to_json ~env a (Obj.magic x)
      | List t, Expr.List xs -> `arr (List.map (go t) xs)
      | Tuple ts, Expr.Tuple xs ->
        begin try
          match Json_config.get_tuple_style ct.ct_configs with
          | `arr -> `arr (List.map2 go ts xs)
          | `obj `default ->
            `obj (List.combine ts xs |> List.mapi (fun i (t, x) -> tuple_index_to_field_name i, go t x))
        with
          Invalid_argument _msg -> fail "tuple length mismatch"
        end
      | Map (`string, t), Expr.Map xs -> `obj (List.map (fun (k, v) -> k, go t v) xs)
      | Option t, Expr.Some x -> go t x
      | Option _, Expr.None -> `null
      | StringEnum cases, Expr.StringEnum s when List.mem s cases -> `str s
      | _, _ -> fail "type mismatch"
    in
    go ct.ct_desc expr
  in
  let record_to_json_fields (fields: record_field list) (expr: Expr.t StringMap.t) =
    fields |> List.map (fun field ->
      match StringMap.find_opt field.rf_name expr with
      | None -> fail (sprintf "missing field '%s'" field.rf_name)
      | Some value -> field.rf_name, expr_to_json field.rf_type value
    )
  in
  let variant_to_json (ctor: variant_constructor) (expr: 'a Refl.constructor) =
    let discriminator = Json_config.get_variant_discriminator td_configs in
    let arg_fname = Json_config.get_name Json_config.default_name_of_variant_arg ctor.vc_configs in
    let discriminator_field = [discriminator, `str ctor.vc_name] in
    match Json_config.get_variant_style ctor.vc_configs with
    | `flatten ->
      begin match ctor.vc_param, expr with
      | `no_param, NoParam _ -> `obj discriminator_field
      | `inline_record fields, InlineRecord { get; _ } ->
        let fields = record_to_json_fields fields (get value) in
        `obj (discriminator_field @ fields)
      | `tuple_like ts, TupleLike { get; _ } ->
        begin match ts, get value, Json_config.get_tuple_style ctor.vc_configs with
        | [], [], _ -> `obj discriminator_field
        | [t], [e], _ ->
          let value = expr_to_json t e in
          `obj (discriminator_field @ [arg_fname, value])
        | ts, es, `arr ->
          let value = `arr (List.map2 expr_to_json ts es) in
          `obj (discriminator_field @ [arg_fname, value])
        | ts, es, `obj `default ->
          let fields = List.combine ts es |> List.mapi (fun i (t, x) -> tuple_index_to_field_name i, expr_to_json t x) in
          `obj (discriminator_field @ fields)
        end
      | _, _ -> fail "param style mismatch"
      end
  in
  match td_kind, Typed.reflect a with
  | Alias_decl ct, Alias { get; _ } -> expr_to_json ct (get value)
  | Record_decl fields, Record { get; _ } -> `obj (record_to_json_fields fields (get value))
  | Variant_decl ctors, Variant { classify; _ } ->
    let name, ref_ctor = classify value in
    begin match ctors |> List.find_opt (fun { vc_name; _ } -> vc_name = name) with
    | Some ctor -> variant_to_json ctor ref_ctor
    | None -> fail (sprintf "missing constructor '%s'" name)
    end
  | _, _ -> fail "kind mismatch"
