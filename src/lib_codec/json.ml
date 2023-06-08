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

  type json_mangling_style = [
    | `default
    | `no_mangling
  ]

  type ('pos, 'kind) config +=
    | Config_json_name : string -> ('pos, [`json_name]) config
    | Config_json_name_of_variant_arg : string -> ([`variant_constructor], [`json_name_of_variant_arg]) config
    | Config_json_variant_style :
      json_variant_style -> ([`variant_constructor], [`json_variant_style]) config
    | Config_json_variant_discriminator :
      string -> ([`type_decl], [`json_variant_discriminator]) config
    | Config_json_tuple_style :
      json_tuple_style -> ([< `variant_constructor | `coretype], [`json_tuple_style]) config
    | Config_json_mangling_style : json_mangling_style -> ('pos, [`json_mangling_style]) config
    | Config_json_custom_encoder : string -> ([`coretype], [`json_custom_encoder]) config
    | Config_json_custom_decoder : string -> ([`coretype], [`json_custom_decoder]) config
    | Config_json_custom_shape_explanation : json_shape_explanation -> ([`coretype], [`json_custom_shape_explanation]) config

  let tuple_index_to_field_name i = "_" ^ string_of_int i

  module Json_config = struct
    let name name = Config_json_name name

    let get_name_opt configs =
      Configs.find (function
        | Config_json_name s -> Some s
        | _ -> None
      ) configs

    let name_of_variant_arg name = Config_json_name_of_variant_arg name

    let get_name_of_variant_arg default =
      Configs.find_or_default ~default (function
        | Config_json_name_of_variant_arg s -> Some s
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

    let default_mangling_style = `default
    let mangling_style style = Config_json_mangling_style style
    let no_mangling = Config_json_mangling_style `no_mangling
    let default_mangling = Config_json_mangling_style `default
    let get_mangling_style configs =
      Configs.find_or_default ~default:default_mangling_style (function
        | Config_json_mangling_style s -> Some s
        | _ -> None) configs

    let mangled kind configs name =
      let open Bindoj_common.Mangling in
      let casing = match kind with
        | `type_name -> snake_to_upper_camel
        | `field_name -> snake_to_lower_camel
        | `discriminator_value -> cap_snake_to_kebab
      in
      match get_mangling_style configs with
      | `default -> casing name
      | `no_mangling -> name

    let get_mangled_name_of_type : type_decl -> string =
      fun { td_name; td_configs; _ } ->
        td_configs |> get_name_opt |? td_name
        |> mangled `type_name td_configs

    let get_mangled_name_of_field : [`type_decl] configs -> record_field -> string =
      fun _ { rf_name; rf_configs; _ } ->
        rf_configs |> get_name_opt |? rf_name
        |> mangled `field_name rf_configs

    let get_mangled_name_of_discriminator : [`type_decl] configs -> variant_constructor -> string =
      fun _ { vc_name; vc_configs; _ } ->
        vc_configs |> get_name_opt |? vc_name
        |> mangled `discriminator_value vc_configs

    let custom_encoder encoder_name = Config_json_custom_encoder encoder_name
    let get_custom_encoder =
      Configs.find (function | Config_json_custom_encoder s -> Some s | _ -> None)

    let custom_decoder decoder_name = Config_json_custom_decoder decoder_name
    let get_custom_decoder =
      Configs.find (function | Config_json_custom_decoder s -> Some s | _ -> None)

    let custom_shape_explanation json_shape_explanation = Config_json_custom_shape_explanation json_shape_explanation
    let get_custom_shape_explanation =
      Configs.find (function | Config_json_custom_shape_explanation s -> Some s | _ -> None)
  end
end

include Config

open Kxclib.Json

type unsafe_primitive_codec = {
    encode : Expr.t -> jv;
    decode : jv -> Expr.t option;
  }
let lookup_primitive_codec ~(env: tdenv) ident_name : unsafe_primitive_codec option =
  StringMap.find_opt ident_name env.prim_ident_typemap
  >>? (fun (Boxed_prim ({ external_format_codecs }, ttd)) ->
       External_format.LabelMap.find_opt Wellknown.json_format' external_format_codecs
       >? (function
           | (Codec (fmt, codec)) when External_format fmt = Wellknown.json_format' ->
              let codec : ((_, jv) External_format.codec)  = Obj.magic codec in
              let open struct
                    open (val ttd)
                    let proj x = Expr.to_refl reflect x |> Option.get
                    let inj x = Expr.of_refl reflect x
                  end in
              let encode (x : Expr.t) : jv =
                codec.encode (proj x) in
              let decode (y : jv) : Expr.t option =
                codec.decode y >? inj in
              { encode; decode }
           | _ -> failwith' "panic: %s" __LOC__
      )
  )

let get_json_discriminator_value : 'a typed_type_decl -> 'a -> string =
  fun ttd a ->
    match Typed.decl ttd with
    | { td_kind = Variant_decl ctors; td_configs; _ } ->
      begin match Typed.to_refl ttd with
      | lazy (Refl.Variant { classify; _ }) ->
        let name = classify a |> fst in
        ctors
        |> List.find_opt (fun { vc_name; _ } -> vc_name = name)
        |> Option.v' (fun () -> failwith' "constructor '%s' is not found" name)
        |> Json_config.get_mangled_name_of_discriminator td_configs
      | _ -> failwith "inconsistent type_decl and reflection result" end
    | { td_name; _ } -> failwith' "'%s' is not a variant decl" td_name

let explain_encoded_json_shape ~(env: tdenv) (td: 't typed_type_decl) : json_shape_explanation =
  let rec process_td ttd : json_shape_explanation =
    let td = Typed.decl ttd in
    let json_type_name = Json_config.get_mangled_name_of_type td in
    `named (json_type_name, process_kind td)
  and process_kind { td_kind; td_configs; _} : json_shape_explanation =
    match td_kind with
    | Alias_decl ct -> process_coretype' ct
    | Record_decl fields ->
       `object_of (fields |&> process_field td_configs)
    | Variant_decl branches ->
       `anyone_of (branches |&> (fun ({ vc_name; vc_param; vc_configs; _ } as ctor) ->
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
              let arg_shape = `tuple_of (cts |&> process_coretype') in
              process_branch discriminator_value discriminator_fname [
                `mandatory_field (arg_fname, arg_shape)]
            | `inline_record fields ->
              process_branch discriminator_value discriminator_fname
                (fields |&> process_field td_configs)
            | `reused_inline_record decl ->
              let fields = decl.td_kind |> function
                | Record_decl fields -> fields
                | _ -> failwith' "panic - type decl of reused inline record '%s' must be record decl." vc_name
              in
              process_branch discriminator_value discriminator_fname
                (fields |&> process_field td_configs)))
  and process_field td_configs ({ rf_type; _ } as field): json_field_shape_explanation =
    let optional, desc =
      rf_type.ct_desc
      |> (function Option desc -> true, desc | desc -> false, desc)
    in
    let json_field_name = Json_config.get_mangled_name_of_field td_configs field in
    let inner = process_coretype ~configs:rf_type.ct_configs desc in
    match optional with
    | true -> `optional_field (json_field_name, inner)
    | false -> `mandatory_field (json_field_name, inner)
  and process_branch kind_name discriminator_field_name proper_fields =
    (* asuming flat_kind for now *)
    let kind_field =
      `mandatory_field (
          discriminator_field_name,
          `exactly (`str kind_name)) in
    `object_of (kind_field :: proper_fields)
  and process_coretype' ({ ct_desc; ct_configs }: Coretype.t) : json_shape_explanation =
    process_coretype ~configs:ct_configs ct_desc
  and process_coretype ?(configs: [`coretype] configs = Configs.empty) (desc: Coretype.desc) : json_shape_explanation = match desc with
    | Prim `unit -> `special ("unit", `exactly `null)
    | Prim `bool -> `boolean
    | Prim `int -> `integral
    | Prim `int53p -> `proper_int53p
    | Prim `float -> `proper_float
    | Prim `string -> `string
    | Prim `uchar -> `special ("uchar", `string)
    | Prim `byte -> `special ("byte", `string)
    | Prim `bytes -> `base64str
    | Uninhabitable -> `special ("uninhabitable", `exactly `null)
    | Ident { id_name = ident; id_codec; _ } ->
      let ident_json_name =
        configs
        |> Json_config.get_name_opt |? ident
        |> Json_config.mangled `type_name configs
      in
      Json_config.get_custom_shape_explanation configs
      >? (fun s -> `named (ident_json_name, s))
      |?! (fun () ->
        match id_codec with
        | `default | `open_ _ ->
          StringMap.find_opt ident env.alias_ident_typemap
          >? (fun (Boxed ((module T) as ttd)) -> process_td (Obj.magic ttd))
          |? `unresolved ("alias: "^ident)
        | `in_module _ -> `unresolved ("alias with special custom: "^ident))
    | Option d -> `nullable (process_coretype d)
    | Tuple ds -> `tuple_of (ds |&> process_coretype)
    | List desc -> `array_of (process_coretype desc)
    | Map (`string, d) -> `record_of (process_coretype d)
    | StringEnum xs -> `string_enum xs
    | Self -> `self
    | _ -> .
  in
  `with_warning ("not considering any config if exists", process_td td)

open MonadOps(ResultOf(struct type err = string * jvpath end))

let rec of_json_impl : ?path:jvpath -> env:tdenv -> 'a typed_type_decl -> jv -> ('a, string * jvpath) result =
  fun ?(path = []) ~env a jv ->
  let { td_configs; td_kind; td_name; _ } = Typed.decl a in
  let opt_to_result msg = function
    | None -> Result.error msg
    | Some a -> Result.ok a
  in
  let try_result jvpath f = try f () with Invalid_argument msg -> Error (msg, jvpath) in
  let parse_obj_style_tuple path (conv: jvpath -> _ -> jv -> (Expr.t, string * jvpath) result) (ts: _ list) (fields: jv StringMap.t) =
    ts
    |> List.mapi (fun i t ->
      let field_name = tuple_index_to_field_name i in
      fields |> StringMap.find_opt field_name
      |> function
      | None -> Result.error (sprintf "mandatory field '%s' does not exist" field_name, path)
      | Some x -> Result.ok x
      >>= fun jv -> conv (`f field_name :: path) t jv)
    |> sequence_list
  in
  let map2i f l1 l2 =
    let rec map2i i f l1 l2 =
      match (l1, l2) with
      | ([], []) -> Some []
      | (a1::l1, a2::l2) ->
        let r = f i a1 a2 in
        map2i (i + 1) f l1 l2
        |> Option.map (fun tail -> r :: tail)
      | (_, _) -> None
    in
    map2i 0 f l1 l2
  in
  let expr_of_json (path: jvpath) (ct: coretype) (jv: jv) : (Expr.t, string * jvpath) result =
    let rec go (path: jvpath) (d: Coretype.desc) (jv: jv) =
      let type_mismatch_error expected_type jv path =
        Result.error(
          sprintf "expecting type '%s' but the given is of type '%s'"
          expected_type (jv |> classify_jv |> string_of_jv_kind)
          , path)
      in
      match d, jv with
      | Prim `unit, (`bool _ | `num _ | `str _ | `arr [] | `obj []) ->
         Expr.Unit |> Result.ok
      | Prim `bool, `bool b -> Expr.Bool b |> Result.ok
      | Prim `int, `num x ->
        if Float.is_integer x then Expr.Int (int_of_float x) |> Result.ok
        else Result.error (sprintf "expecting an integer but the given is '%f'" x, path)
      | Prim `int53p, `num x -> Expr.Int53p (Int53p.of_float x) |> Result.ok
      | Prim `float, `num x -> Expr.Float x |> Result.ok
      | Prim `string, `str s -> Expr.String s |> Result.ok
      | Prim `uchar, `str s ->
        if String.length s = 1 then
          Expr.Uchar (Uchar.of_char (String.get s 0)) |> Result.ok
        else Result.error (sprintf "string '%s' is not a valid uchar value" s, path)
      | Prim `byte, `num x ->
        let x = int_of_float x in
        if 0 <= x && x <= 255 then Expr.Byte (char_of_int x) |> Result.ok
        else Result.error (sprintf "number '%d' is not a valid byte value" x, path)
      | Prim `bytes, `str s ->
        try_result path (fun () -> Expr.Bytes (Kxclib.Base64.decode s) |> Result.ok)
      | Prim p, jv -> type_mismatch_error (Coretype.string_of_prim p) jv path
      | Uninhabitable, _ -> Result.error (sprintf "unexpected value of *uninhabitable* type: %a" pp_unparse jv, path)
      | Ident i, _ -> begin match lookup_primitive_codec ~env i.id_name with
        | Some codec ->
          codec.decode jv
          |> opt_to_result (sprintf "failed to decode value of primitive ident type '%s': %a" i.id_name pp_unparse jv, path)
        | _ ->
          match StringMap.find_opt i.id_name env.alias_ident_typemap with
          | Some boxed ->
            let t = Typed.unbox boxed in
            of_json_impl ~path ~env t jv
            >|= fun value -> Expr.Refl (Typed.to_refl t, value)
          | None -> invalid_arg' "type '%s' not found in env" i.id_name
        end
      | Self, _ ->
        of_json_impl ~path ~env a jv
        >|= fun value -> Expr.Refl (Typed.to_refl a, value)
      | List t, `arr xs ->
        xs |> List.mapi (fun i -> go (`i i :: path) t) >>=* fun xs -> Expr.List xs |> Result.ok
      | List _, jv -> type_mismatch_error "array" jv path
      | Tuple ts, _ ->
        try_result path (fun () ->
          match jv, Json_config.get_tuple_style ct.ct_configs with
          | `arr xs, `arr -> begin
            map2i (fun i -> go (`i i :: path)) ts xs |> function
            | Some es -> es >>=* fun xs -> Expr.Tuple xs |> Result.ok
            | None ->
              let ts_len = List.length ts in
              let xs_len = List.length xs in
              let msg =
                sprintf "expecting a tuple of length %d, but the given has a length of %d"
                  ts_len xs_len
              in
              Result.error (msg, path)
          end
          | _, `arr ->
            Result.error (sprintf "an array is expected for a tuple value, but the given is of type '%s'" (jv |> classify_jv |> string_of_jv_kind), path)
          | `obj fields, `obj `default ->
            parse_obj_style_tuple path go ts (StringMap.of_list fields)
            >|= (fun xs -> Expr.Tuple xs)
          | _, `obj `default ->
            Result.error (sprintf "an object is expected for a tuple value, but the given is of type '%s'" (jv |> classify_jv |> string_of_jv_kind), path)
        )
      | Map (`string, t), `obj fields ->
        fields |> List.map (fun (name, x) -> go (`f name :: path) t x >|= (fun v -> name, v))
        >>=* fun fields -> Expr.Map fields |> Result.ok
      | Map _, jv -> type_mismatch_error "object" jv path
      | Option _, `null -> Result.ok Expr.None
      | Option t, _ -> go path t jv >|= (fun x -> Expr.Some x)
      | StringEnum cases, `str s ->
        if List.mem s cases then
          Expr.StringEnum s |> Result.ok
        else
          let msg =
            sprintf "given string '%s' is not one of [ %s ]" s
            (cases |&> (sprintf "'%s'") |> String.concat ", ")
          in
          Result.error (msg, path)
      | StringEnum _, jv -> type_mismatch_error "string" jv path
    in
    go path ct.ct_desc jv
  in
  let record_fields_of_json (path: jvpath) (fields: record_field list) (jv: jv) : (Expr.t StringMap.t, string * jvpath) result =
    match jv with
    | `obj obj ->
      let obj = StringMap.of_list obj in
      fields |> List.map (fun ({ rf_name; rf_type; _} as field) ->
        let json_field_name = Json_config.get_mangled_name_of_field td_configs field in
        match obj |> StringMap.find_opt json_field_name with
        | None when Coretype.is_option rf_type -> Result.ok (rf_name, Expr.None)
        | Some jv -> expr_of_json (`f json_field_name :: path) rf_type jv >>= fun expr -> Result.ok (rf_name, expr)
        | None -> Result.error (sprintf "mandatory field '%s' does not exist" json_field_name, path)
      ) |> sequence_list >|= StringMap.of_list
    | _ -> Result.error (sprintf "an object is expected for a record value, but the given is of type '%s'" (jv |> classify_jv |> string_of_jv_kind), path)
  in
  let fail () = invalid_arg "inconsistent type_decl and reflection result" in
  let constructor_of_json (path: jvpath) (ctors: variant_constructor list) (ref_ctors: 'a Refl.constructor StringMap.t) (jv: jv) : ('a, string * jvpath) result =
    let discriminator_fname =
      Json_config.get_variant_discriminator td_configs
      |> Json_config.mangled `field_name td_configs
    in
    match jv with
    | `obj obj ->
      let obj = StringMap.of_list obj in
      begin match obj |> StringMap.find_opt discriminator_fname with
      | Some (`str discriminator_value) ->
        ctors
        |> List.find_opt (fun ctor ->
          Json_config.get_mangled_name_of_discriminator td_configs ctor
          |> (=) discriminator_value)
        |> opt_to_result
          ( sprintf "given discriminator field value '%s' is not one of [ %s ]"
              discriminator_value
              (ctors
                |&> (fun ctor ->
                  Json_config.get_mangled_name_of_discriminator td_configs ctor
                  |> sprintf "'%s'")
                |> String.concat ", "),
            `f discriminator_fname :: path)
        >>= (fun ctor ->
          let ref_ctor = ref_ctors |> StringMap.find_opt ctor.vc_name |> Option.v' fail in
          let arg_fname =
            ctor.vc_configs
            |> Json_config.(get_name_of_variant_arg default_name_of_variant_arg)
            |> Json_config.mangled `field_name ctor.vc_configs
          in
          match Json_config.get_variant_style ctor.vc_configs with
          | `flatten ->
            begin match ctor.vc_param, ref_ctor with
            | `no_param, NoParam { value } -> Ok value
            | `tuple_like ts, TupleLike { mk; _ } ->
              let mk_result (path : jvpath) =
                mk &> opt_to_result (sprintf "panic - failed to make tuple for variant_constructor '%s'" discriminator_value, path)
              in
              begin match Json_config.get_tuple_style ctor.vc_configs, ts with
              | `obj `default, _ :: _ :: _ ->
                parse_obj_style_tuple path expr_of_json ts obj
                >>= (mk_result path)
              | _, [] -> mk_result path []
              | _, _ ->
                obj
                |> StringMap.find_opt arg_fname
                |> opt_to_result (sprintf "mandatory field '%s' does not exist" arg_fname, path)
                >>= (fun arg ->
                  let path = `f arg_fname :: path in
                  match ts, arg with
                  | [t], _ -> expr_of_json path t arg >>= fun expr -> mk_result path [expr]
                  | ts, `arr xs ->
                    try_result path
                      (fun () ->
                        map2i (fun i -> expr_of_json (`i i :: path)) ts xs |> function
                        | Some es -> es >>=* mk_result path
                        | None ->
                          let ts_len = List.length ts in
                          let xs_len = List.length xs in
                          let msg =
                            sprintf "expecting an array of length %d, but the given has a length of %d"
                              ts_len xs_len
                          in
                          Result.error (msg, path))
                  | _, jv -> Result.error (sprintf "an array is expected for a tuple value, but the given is of type '%s'" (jv |> classify_jv |> string_of_jv_kind), path)
                )
              end
            | `inline_record fields, InlineRecord { mk; _ } ->
              record_fields_of_json path fields jv
              >>= (mk &> opt_to_result (sprintf "panic - failed to make inline record for variant_constructor '%s'" discriminator_value, path))
            | `reused_inline_record decl, ReusedInlineRecord { mk; _ } ->
              let fields = decl.td_kind |> function
                | Record_decl fields -> fields
                | _ -> failwith' "panic - type decl of reused inline record '%s' must be record decl." ctor.vc_name
              in
              record_fields_of_json path fields jv
              >>= (mk &> opt_to_result (sprintf "panic - failed to make reused inline record for variant_constructor '%s'" discriminator_value, path))
            | _ -> fail ()
            end
        )
      | Some jv -> Result.error (sprintf "a string is expected for a variant discriminator, but the given is of type '%s'" (jv |> classify_jv |> string_of_jv_kind), `f discriminator_fname :: path)
      | None -> Result.error (sprintf "discriminator field '%s' does not exist" discriminator_fname, path)
      end
    | _ -> Result.error (sprintf "an object is expected for a variant value, but the given is of type '%s'" (jv |> classify_jv |> string_of_jv_kind), path)
  in
  match td_kind, Typed.reflect a with
  | Alias_decl ct, Alias { mk; _ } ->
    expr_of_json path ct jv
    >>= (mk &> opt_to_result (sprintf "panic - failed to make alias value for type '%s'" td_name, path))
  | Record_decl fields, Record { mk; _ } ->
    record_fields_of_json path fields jv
    >>= (mk &> opt_to_result (sprintf "panic - failed to make record value for type '%s'" td_name, path))
  | Variant_decl ctors, Variant { constructors; _ } ->
    constructor_of_json path ctors constructors jv
  | _, _ -> fail ()

let of_json' : env:tdenv -> 'a typed_type_decl -> jv -> 'a OfJsonResult.t =
  fun ~env a jv ->
  of_json_impl ~env a jv
  |> Result.map_error (fun (msg, path) -> (msg, path, explain_encoded_json_shape ~env a))

let of_json ~env a jv = of_json_impl ~env a jv |> Result.to_option

let rec to_json ~(env: tdenv) (a: 'a typed_type_decl) (value: 'a) : jv =
  let fail msg = invalid_arg' "inconsistent type_decl and reflection result (%s)" msg in
  let { td_configs; td_kind; _ } = Typed.decl a in

  let expr_to_json (ct: coretype) (expr: Expr.t) : jv =
    let rec go (d: Coretype.desc) (e: Expr.t) =
      match d, e with
      | Prim `unit,   Expr.Unit -> `num 1.
      | Prim `bool,   Expr.Bool b -> `bool b
      | Prim `int,    Expr.Int i -> `num (float_of_int i)
      | Prim `int53p, Expr.Int53p i -> `num (Int53p.to_float i)
      | Prim `float,  Expr.Float f -> `num f
      | Prim `string, Expr.String s -> `str s
      | Prim `uchar,  Expr.Uchar c -> `str (String.of_list [Uchar.to_char c])
      | Prim `byte,   Expr.Byte c -> `num (float_of_int (int_of_char c))
      | Prim `bytes,  Expr.Bytes bs -> `str (Kxclib.Base64.encode bs)
      | Uninhabitable, Expr.Unit -> `null
      | Ident i, Expr.Refl (_, x) ->
        begin match lookup_primitive_codec ~env i.id_name with
        | Some codec -> codec.encode e
        | _ -> (
          match StringMap.find_opt i.id_name env.alias_ident_typemap with
          | Some boxed -> to_json ~env (Typed.unbox boxed) (Obj.magic x)
          | None -> invalid_arg' "type '%s' not found in env" i.id_name
        )
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
      | Some value ->
        let json_field_name = Json_config.get_mangled_name_of_field td_configs field in
        json_field_name, expr_to_json field.rf_type value
    )
  in
  let variant_to_json (ctor: variant_constructor) (expr: 'a Refl.constructor) =
    let discriminator_fname =
      Json_config.get_variant_discriminator td_configs
      |> Json_config.mangled `field_name td_configs
    in
    let discriminator_value = Json_config.get_mangled_name_of_discriminator td_configs ctor in
    let discriminator_field = [discriminator_fname, `str discriminator_value] in
    let arg_fname =
      ctor.vc_configs
      |> Json_config.(get_name_of_variant_arg default_name_of_variant_arg)
      |> Json_config.mangled `field_name ctor.vc_configs
    in
    match Json_config.get_variant_style ctor.vc_configs with
    | `flatten ->
      begin match ctor.vc_param, expr with
      | `no_param, NoParam _ -> `obj discriminator_field
      | `inline_record fields, InlineRecord { get; _ } ->
        let fields = record_to_json_fields fields (get value) in
        `obj (discriminator_field @ fields)
      | `reused_inline_record decl, ReusedInlineRecord { get; _ } ->
        let fields = decl.td_kind |> function
          | Record_decl fields -> fields
          | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." ctor.vc_name
        in
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
