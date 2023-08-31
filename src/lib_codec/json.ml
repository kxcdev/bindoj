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
open Bindoj_base
open Typed_type_desc
open Runtime
open Kxclib.Json

module Json_config = Bindoj_codec_config.Json_config

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
        let mangling_style =
          Json_config.get_mangling_style_opt td_configs
          |? Json_config.default_mangling_style
        in
        let name = classify a |> fst in
        ctors
        |> List.find_opt (fun { vc_name; _ } -> vc_name = name)
        |> Option.v' (fun () -> failwith' "constructor '%s' is not found" name)
        |> Json_config.get_mangled_name_of_discriminator ~inherited:mangling_style
        |> fst
      | _ -> failwith "inconsistent type_decl and reflection result" end
    | { td_name; _ } -> failwith' "'%s' is not a variant decl" td_name

let validate_spreading_type = function
  | `direct _ -> invalid_arg "non-nested argument/field cannot be spread."
  | `nested ({ td_kind = Alias_decl _; _ }, _) -> invalid_arg "Alias decl cannot be spread."
  | `nested ({ td_kind = Record_decl fields; _ } as td, codec) -> `record_decl (td, fields, codec)
  | `nested ({ td_kind = Variant_decl ctors; _ } as td, codec) -> `variant_decl (td, ctors, codec)

let explain_encoded_json_shape'
  (type shape)
  (type field_shape)
  (jse: (shape, field_shape) json_shape_explaner)
  (resolve_ident: (type_decl -> shape) -> Coretype.codec -> Coretype.ident -> string -> shape)
  (td: type_decl): shape =
  let inherit_codec base_codec = function
    | `default -> base_codec
    | codec -> codec
  in
  let open (val jse) in
  let rec process_td ~base_ident_codec ?base_mangling_style td: shape =
    let (json_type_name, base_mangling_style) = Json_config.get_mangled_name_of_type ?inherited:base_mangling_style td in
    named (json_type_name, process_kind ~base_ident_codec base_mangling_style td)
  and process_kind ~base_ident_codec base_mangling_style ({ td_kind; _} as td): shape =
    match td_kind with
    | Alias_decl ct -> process_coretype' ~base_ident_codec base_mangling_style ct
    | Record_decl fields ->
      process_fields ~base_ident_codec base_mangling_style fields
      |> process_object
    | Variant_decl branches ->
      let discriminator_fname = Json_config.get_mangled_name_of_discriminator_field ~inherited:base_mangling_style td in
      branches |&>> (process_constructor ~base_ident_codec base_mangling_style discriminator_fname)
      |> process_object
  and process_non_spread_variant_argument ~base_ident_codec base_mangling_style va: shape =
    if Json_config.get_nested_field_style va.va_configs = `spreading then
      invalid_arg "Spreading of the nested type of tuple_like is only valid if there is only a single argument.";
    match va.va_type with
    | `direct ct ->
      process_coretype' ~base_ident_codec base_mangling_style ct
    | `nested (td, codec) ->
      process_td ~base_ident_codec:(inherit_codec base_ident_codec codec) td
  and process_non_spread_variant_argument' ~base_ident_codec base_mangling_style field_name va: field_shape =
    if Json_config.get_nested_field_style va.va_configs = `spreading then
      invalid_arg "Spreading of the nested type of tuple_like is only valid if there is only a single argument.";
    match va.va_type with
    | `direct ({ ct_desc = Option desc; _ } as ct)
    | `nested ({ td_kind = Alias_decl ({ ct_desc = Option desc; _ } as ct); _ }, _) ->
      optional_field (field_name, process_coretype' ~base_ident_codec base_mangling_style { ct with ct_desc = desc })
    | _ ->
      let shape = process_non_spread_variant_argument ~base_ident_codec base_mangling_style va in
      mandatory_field (field_name, shape)
  and process_spread_variant_argument ~base_ident_codec va: field_shape list list =
    let base_mangling_style = Json_config.default_mangling_style in
    begin match validate_spreading_type va.va_type with
    | `record_decl (td, fields, codec) ->
      let base_mangling_style = Json_config.get_mangling_style_opt td.td_configs |? base_mangling_style in
      process_fields ~base_ident_codec:(inherit_codec base_ident_codec codec) base_mangling_style fields
    | `variant_decl (td, ctors, codec) ->
      let base_mangling_style = Json_config.get_mangling_style_opt td.td_configs |? base_mangling_style in
      let discriminator_fname =
        Json_config.(get_variant_discriminator td.td_configs
        |> mangled `field_name base_mangling_style)
      in
      ctors |&>> (process_constructor ~base_ident_codec:(inherit_codec base_ident_codec codec) base_mangling_style discriminator_fname)
    end
  and process_fields ~base_ident_codec base_mangling_style fields: field_shape list list =
    fields |&> (fun ({ rf_type; rf_configs; _ } as field) ->
      let field_of_coretype base_mangling_style json_field_name (ct: coretype) =
        let optional, desc =
          ct.ct_desc |> function
          | Option desc -> true, desc
          | desc -> false, desc
        in
        let inner = process_coretype ~base_ident_codec base_mangling_style ct.ct_configs desc in
        begin match optional with
        | true -> optional_field (json_field_name, inner)
        | false -> mandatory_field (json_field_name, inner)
        end
      in
      let (json_field_name, base_mangling_style) = Json_config.get_mangled_name_of_field ~inherited:base_mangling_style field in
      let nested_style = Json_config.get_nested_field_style rf_configs in

      match nested_style, rf_type with
      | `nested, `direct ct ->
        List.return [ field_of_coretype base_mangling_style json_field_name ct ]
      | `nested, `nested ({ td_kind = Alias_decl ({ ct_desc = Option desc; _ } as ct); _ }, codec) ->
        List.return [
          optional_field (
            json_field_name,
            process_coretype'
              ~base_ident_codec:(inherit_codec base_ident_codec codec)
              Json_config.default_mangling_style
              { ct with ct_desc = desc })
        ]
      | `nested, `nested (td, codec) ->
        List.return [
          mandatory_field (json_field_name, process_td ~base_ident_codec:(inherit_codec base_ident_codec codec) td) ]
      | `spreading, _ ->
        begin match validate_spreading_type rf_type with
        | `record_decl (td, fields, codec) ->
          let base_mangling_style = Json_config.(get_mangling_style_opt td.td_configs |? default_mangling_style) in
          process_fields ~base_ident_codec:(inherit_codec base_ident_codec codec) base_mangling_style fields
        | `variant_decl (td, ctors, codec) ->
          let base_mangling_style = Json_config.(get_mangling_style_opt td.td_configs |? default_mangling_style) in
          let discriminator_fname = Json_config.get_mangled_name_of_discriminator_field ~inherited:base_mangling_style td in
          ctors |&>> (process_constructor ~base_ident_codec:(inherit_codec base_ident_codec codec) base_mangling_style discriminator_fname)
      end
    )
    |> List.fold_left (fun result ->
      List.fmap (fun fields -> result |&> List.rev_append fields)
    ) [ [] ]
    |&> List.rev
  and process_constructor ~base_ident_codec base_mangling_style discriminator_fname ({ vc_name; vc_param; vc_configs; _ } as ctor) =
    let (discriminator_value, base_mangling_style) =
      Json_config.get_mangled_name_of_discriminator ~inherited:base_mangling_style ctor
    in
    begin match vc_param with
    | `no_param ->
      List.return []
    | `tuple_like [ va ] when Json_config.get_nested_field_style va.va_configs = `spreading ->
      process_spread_variant_argument ~base_ident_codec va
    | `tuple_like vas ->
      let arg_fname = Json_config.get_mangled_name_of_variant_arg ~inherited:base_mangling_style ctor in
      let tuple_style = Json_config.get_tuple_style vc_configs in
      begin match tuple_style, vas with
      | `obj `default, vas ->
        vas |> List.mapi (fun i ->
          Json_config.tuple_index_to_field_name i
          |> process_non_spread_variant_argument' ~base_ident_codec base_mangling_style)
      | `arr, [ va ] ->
        [ process_non_spread_variant_argument' ~base_ident_codec base_mangling_style arg_fname va ]
      | `arr, vas ->
        let arg_shape = tuple_of (vas |&> process_non_spread_variant_argument ~base_ident_codec base_mangling_style) in
        [ mandatory_field (arg_fname, arg_shape) ]
      end
      |> List.return
    | `inline_record fields ->
      process_fields ~base_ident_codec base_mangling_style fields
    | `reused_inline_record { td_kind; td_configs; _ } ->
      let base_mangling_style =
        Json_config.(get_mangling_style_opt td_configs |? default_mangling_style)
      in
      let fields = td_kind |> function
        | Record_decl fields -> fields
        | _ -> failwith' "panic - type decl of reused inline record '%s' must be record decl." vc_name
      in
      process_fields ~base_ident_codec base_mangling_style fields
    end
    |> function
    | [] -> failwith "The given list must not be empty."
    | fs ->
      fs |&> fun fields ->
        (mandatory_field (discriminator_fname, exactly (`str discriminator_value)) :: fields)
  and process_object fields =
    match fields with
    | [] -> failwith "The given list must not be empty."
    | [ fields ] -> object_of fields
    | fs -> anyone_of (fs |&> (fun fields -> object_of fields))
  and process_coretype' ~base_ident_codec base_mangling_style ({ ct_desc; ct_configs }: Coretype.t) =
    process_coretype ~base_ident_codec base_mangling_style ct_configs ct_desc
  and process_coretype ~base_ident_codec base_mangling_style (configs: [`coretype] configs) desc: shape =
    let base_mangling_style = Json_config.get_mangling_style_opt configs |? base_mangling_style in
    let rec go: Coretype.desc -> _ = function
      | Prim `unit -> special ("unit", exactly `null)
      | Prim `bool -> boolean
      | Prim `int -> integral
      | Prim `int53p -> proper_int53p
      | Prim `float -> proper_float
      | Prim `string -> string
      | Prim `uchar -> special ("uchar", string)
      | Prim `byte -> special ("byte", string)
      | Prim `bytes -> base64str
      | Uninhabitable -> special ("uninhabitable", exactly `null)
      | Ident ({ id_name; id_codec; _ } as ident) ->
        let ident_json_name =
          configs
          |> Json_config.get_name_opt |? id_name
          |> Json_config.mangled `type_name base_mangling_style
        in
        (Json_config.get_custom_shape_explanation configs |> function
        | Some s -> named (ident_json_name, shape_of_json_shape_explanation s)
        | None ->
          resolve_ident
            (process_td
              ~base_ident_codec:(inherit_codec base_ident_codec id_codec)
              ~base_mangling_style)
            base_ident_codec ident ident_json_name)
      | Option d -> nullable (go d)
      | Tuple ds ->
        begin match Json_config.get_tuple_style configs with
        | `obj `default ->
          ds |> List.mapi (fun i ->
            let field_name = Json_config.tuple_index_to_field_name i in
            function
            | Coretype.Option desc -> optional_field (field_name, go desc)
            | desc -> mandatory_field (field_name, go desc))
          |> object_of
        | `arr -> tuple_of (ds |&> go)
        end
      | List desc -> array_of (go desc)
      | Map (`string, d) -> record_of (go d)
      | StringEnum xs -> string_enum (xs |&> Json_config.get_mangled_name_of_string_enum_case ~inherited:base_mangling_style)
      | Self -> self
      | _ -> .
    in go desc
  in
  with_warning ("not considering any config if exists", process_td ~base_ident_codec:`default td)

let explain_encoded_json_shape ~(env: tdenv) (td: 't typed_type_decl) : json_shape_explanation =
  explain_encoded_json_shape'
    (json_shape_explanation)
    (fun (process_td: type_decl -> json_shape_explanation) _ ({ id_name; id_codec }: Coretype.ident) _ ->
      match id_codec with
      | `default | `open_ _ ->
        StringMap.find_opt id_name env.alias_ident_typemap
        >? (fun (Boxed (module T)) -> process_td T.decl)
        |? `unresolved ("alias: "^id_name)
      | `in_module _ -> `unresolved ("alias with special custom: "^id_name))
    (Typed.decl td)

let nested_type_to_coretype =
  function
  | `direct ct | `nested({ td_kind = Alias_decl ct; _ }, _) ->
    ct, Coretype.is_option ct
  | `nested ({ td_name; td_configs; _ }, codec) ->
    let mangling_style = Json_config.(get_mangling_style_opt td_configs |? default_mangling_style) in
    Coretype.mk_ident
      ~configs:[ Json_config.mangling_style mangling_style ]
      ~codec td_name,
      false

open MonadOps(ResultOf(struct type err = string * jvpath end))

let rec of_json_impl : ?path:jvpath -> env:tdenv -> 'a typed_type_decl -> jv -> ('a, string * jvpath) result =
  fun ?(path = []) ~env a jv ->
  let { td_configs; td_kind; td_name; _ } as td = Typed.decl a in
  let opt_to_result msg = function
    | None -> Result.error msg
    | Some a -> Result.ok a
  in
  let try_result jvpath f = try f () with Invalid_argument msg -> Error (msg, jvpath) in
  let parse_obj_style_tuple path (conv: jvpath -> string -> _ -> jv option -> (Expr.t, string * jvpath) result) (ts: _ list) (fields: jv StringMap.t) =
    ts
    |> List.mapi (fun i t ->
      let field_name = Json_config.tuple_index_to_field_name i in
      fields |> StringMap.find_opt field_name
      |> conv (`f field_name :: path) field_name t)
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
  let expr_of_json base_mangling_style (path: jvpath) (ct: coretype) (jv: jv) : (Expr.t, string * jvpath) result =
    let base_mangling_style = Json_config.get_mangling_style_opt ct.ct_configs |? base_mangling_style in
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
            parse_obj_style_tuple path (fun path' field_name -> !? (function
              | Coretype.Option (Option _), _ -> Error ("Nested option types cannot be json fields.", path')
              | Option _, None -> Ok Expr.None
              | desc, Some jv -> go path' desc jv
              | _, None -> Error (sprintf "mandatory field '%s' does not exist" field_name, path)
            )) ts (StringMap.of_list fields)
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
        let case_names =
          cases |&> (fun ((name, _, _) as c) ->
            Json_config.get_mangled_name_of_string_enum_case ~inherited:base_mangling_style c, name)
        in
        case_names
        |> List.assoc_opt s
        |> begin function
        | Some name -> Expr.StringEnum name |> Result.ok
        | None ->
          Result.error
            (sprintf "given string '%s' is not one of [ %s ]" s
              (case_names |&> (fst &> sprintf "'%s'") |> String.concat ", "),
            path)
        end
      | StringEnum _, jv -> type_mismatch_error "string" jv path
    in
    go path ct.ct_desc jv
  in
  let fail msg = invalid_arg' "inconsistent type_decl and reflection result (%s)" msg in
  let record_fields_of_json base_mangling_style path fields jv : (Expr.t StringMap.t, string * jvpath) result =
    match jv with
    | `obj obj ->
      let obj = StringMap.of_list obj in
      fields |&> (fun ({ rf_name; rf_type; rf_configs; _} as field) ->
        let (json_field_name, base_mangling_style) = Json_config.get_mangled_name_of_field ~inherited:base_mangling_style field in
        let ct, is_option = nested_type_to_coretype rf_type in
        let ct_of_json path jv =
          expr_of_json base_mangling_style path ct jv
          >|= (fun expr -> [ rf_name, expr ])
        in
        begin match Json_config.get_nested_field_style rf_configs, rf_type with
        | `spreading, _ ->
          begin match validate_spreading_type rf_type with
          | `record_decl _ | `variant_decl _ -> ct_of_json path jv
          end
        | `nested, _ ->
          begin match StringMap.find_opt json_field_name obj with
          | None when is_option -> Ok [ rf_name, Expr.None ]
          | Some jv -> ct_of_json (`f json_field_name :: path) jv
          | None -> Error (sprintf "mandatory field '%s' does not exist" json_field_name, path)
          end
        end
      ) |> sequence_list >|= (List.concat &> StringMap.of_list)
    | _ -> Result.error (sprintf "an object is expected for a record value, but the given is of type '%s'" (jv |> classify_jv |> string_of_jv_kind), path)
  in
  let constructor_of_json base_mangling_style (path: jvpath) (ctors: variant_constructor list) (ref_ctors: 'a Refl.constructor StringMap.t) (jv: jv) : ('a, string * jvpath) result =
    let discriminator_fname = Json_config.get_mangled_name_of_discriminator_field ~inherited:base_mangling_style td in
    match jv with
    | `obj obj ->
      let obj = StringMap.of_list obj in
      begin match obj |> StringMap.find_opt discriminator_fname with
      | Some (`str discriminator_value) ->
        ctors
        |> List.find_opt (fun ctor ->
          Json_config.get_mangled_name_of_discriminator ~inherited:base_mangling_style ctor
          |> fst
          |> (=) discriminator_value)
        |> opt_to_result
          ( sprintf "given discriminator field value '%s' is not one of [ %s ]"
              discriminator_value
              (ctors
                |&> (fun ctor ->
                  Json_config.get_mangled_name_of_discriminator ~inherited:base_mangling_style ctor
                  |> fst
                  |> sprintf "'%s'")
                |> String.concat ", "),
            `f discriminator_fname :: path)
        >>= (fun ctor ->
          let base_mangling_style =
            Json_config.get_mangling_style_opt ctor.vc_configs |? base_mangling_style
          in
          let ref_ctor = ref_ctors |> StringMap.find_opt ctor.vc_name |> Option.v' (fun () -> fail @@ sprintf "refl of constructor '%s' not found" ctor.vc_name) in
          let arg_fname = Json_config.get_mangled_name_of_variant_arg ~inherited:base_mangling_style ctor in
          let mk_result label mk (path : jvpath) x =
            mk x |> opt_to_result (sprintf "panic - failed to make %s for variant_constructor '%s'" label discriminator_value, path)
          in
          match Json_config.get_variant_style ctor.vc_configs with
          | `flatten ->
            begin match ctor.vc_param, ref_ctor with
            | `no_param, NoParam { value } -> Ok value
            | `tuple_like [ va ], TupleLike { mk; _ }
              when Json_config.get_nested_field_style va.va_configs = `spreading ->
                begin match validate_spreading_type va.va_type with
                | `record_decl _ | `variant_decl _ ->
                  let base_mangling_style = Json_config.get_mangling_style_opt va.va_configs |? base_mangling_style in
                  let ct, _ = nested_type_to_coretype va.va_type in
                  expr_of_json base_mangling_style path ct jv
                  >>= fun expr -> mk_result "tuple" mk path [expr]
                end
            | `tuple_like ts, TupleLike { mk; _ } ->
              let mk_result = mk_result "tuple" mk in
              let variant_argument_of_json base_mangling_style path va jv =
                if Json_config.get_nested_field_style va.va_configs = `spreading then
                  invalid_arg "Spreading of the nested type of tuple_like is only valid if there is only a single argument.";
                let base_mangling_style = Json_config.get_mangling_style_opt va.va_configs |? base_mangling_style in
                let ct, _ = nested_type_to_coretype va.va_type in
                expr_of_json base_mangling_style path ct jv
              in
              begin match Json_config.get_tuple_style ctor.vc_configs, ts with
              | `obj `default, _ :: _ :: _ ->
                parse_obj_style_tuple path (fun path' field_name -> !?(function
                  | { va_type = `direct ct | `nested ({ td_kind = Alias_decl ct; _ }, _); _ }, None
                    when Coretype.is_option ct ->
                      Ok Expr.None
                  | va, Some jv -> variant_argument_of_json base_mangling_style path' va jv
                  | _, None -> Error (sprintf "mandatory field '%s' does not exist" field_name, path))
                ) ts obj
                >>= (fun x -> mk_result path x)
              | _, [] -> mk_result path []
              | _, _ ->
                obj
                |> StringMap.find_opt arg_fname
                |> (fun arg ->
                  let arg_path = `f arg_fname :: path in
                  match ts, arg with
                  | [ { va_type = `direct ct | `nested ({ td_kind = Alias_decl ct; _}, _); _ }
                    ], None when Coretype.is_option ct ->
                    mk_result path [ Expr.None ]
                  | [t], Some arg ->
                    variant_argument_of_json base_mangling_style arg_path t arg
                    >>= fun expr -> mk_result path [expr]
                  | ts, Some (`arr xs) ->
                    try_result path (fun () ->
                      map2i (fun i va ->
                        variant_argument_of_json base_mangling_style (`i i :: arg_path) va) ts xs
                        |> function
                        | Some es -> es >>=* fun x -> mk_result path x
                        | None ->
                          let ts_len = List.length ts in
                          let xs_len = List.length xs in
                          let msg = sprintf "expecting an array of length %d, but the given has a length of %d" ts_len xs_len in
                          Result.error (msg, arg_path))
                  | _, Some jv -> Result.error (sprintf "an array is expected for a tuple value, but the given is of type '%s'" (jv |> classify_jv |> string_of_jv_kind), path)
                  | _, None -> Result.error (sprintf "mandatory field '%s' does not exist" arg_fname, path)
                )
              end
            | `inline_record fields, InlineRecord { mk; _ } ->
              record_fields_of_json base_mangling_style path fields jv
              >>= (mk_result "inline record" mk path)
            | `reused_inline_record { td_kind; td_configs; _ }, ReusedInlineRecord { mk; _ } ->
              let base_mangling_style =
                Json_config.(get_mangling_style_opt td_configs |? default_mangling_style)
              in
              let fields = td_kind |> function
                | Record_decl fields -> fields
                | _ -> failwith' "panic - type decl of reused inline record '%s' must be record decl." ctor.vc_name
              in
              record_fields_of_json base_mangling_style path fields jv
              >>= (mk_result "reused inline record" mk path)
            | _ -> fail "type mismatch"
            end
        )
      | Some jv -> Result.error (sprintf "a string is expected for a variant discriminator, but the given is of type '%s'" (jv |> classify_jv |> string_of_jv_kind), `f discriminator_fname :: path)
      | None -> Result.error (sprintf "discriminator field '%s' does not exist" discriminator_fname, path)
      end
    | _ -> Result.error (sprintf "an object is expected for a variant value, but the given is of type '%s'" (jv |> classify_jv |> string_of_jv_kind), path)
  in
  let base_mangling_style = Json_config.(get_mangling_style_opt td_configs |? default_mangling_style) in
  match td_kind, Typed.reflect a with
  | Alias_decl ct, Alias { mk; _ } ->
    expr_of_json base_mangling_style path ct jv
    >>= (mk &> opt_to_result (sprintf "panic - failed to make alias value for type '%s'" td_name, path))
  | Record_decl fields, Record { mk; _ } ->
    record_fields_of_json base_mangling_style path fields jv
    >>= (mk &> opt_to_result (sprintf "panic - failed to make record value for type '%s'" td_name, path))
  | Variant_decl ctors, Variant { constructors; _ } ->
    constructor_of_json base_mangling_style path ctors constructors jv
  | _, _ -> fail "refl mismatch"

let of_json' : env:tdenv -> 'a typed_type_decl -> jv -> 'a OfJsonResult.t =
  fun ~env a jv ->
  of_json_impl ~env a jv
  |> Result.map_error (fun (msg, path) -> (msg, path, explain_encoded_json_shape ~env a))

let of_json ~env a jv = of_json_impl ~env a jv |> Result.to_option

let rec to_json ~(env: tdenv) (a: 'a typed_type_decl) (value: 'a) : jv =
  let fail msg = invalid_arg' "inconsistent type_decl and reflection result (%s)" msg in
  let { td_configs; td_kind; _ } as td = Typed.decl a in
  let expr_to_json base_mangling_style (ct: coretype) (expr: Expr.t) : jv =
    let base_mangling_style =
      Json_config.get_mangling_style_opt ct.ct_configs |? base_mangling_style
    in
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
        | _ ->
          match StringMap.find_opt i.id_name env.alias_ident_typemap with
        | Some boxed -> to_json ~env (Typed.unbox boxed) (Obj.magic x)
        | None -> invalid_arg' "type '%s' not found in env" i.id_name
        end
      | Self, Expr.Refl (_, x) -> to_json ~env a (Obj.magic x)
      | List t, Expr.List xs -> `arr (List.map (go t) xs)
      | Tuple ts, Expr.Tuple xs ->
        begin
          match Json_config.get_tuple_style ct.ct_configs with
          | `arr -> `arr (List.map2 go ts xs)
          | `obj `default ->
            `obj (
              List.combine ts xs
              |> List.mapi (fun i -> function
                | Coretype.Option (Option _), _ -> failwith "Nested option types cannot be json fields."
                | Option _, Expr.None -> None
                | (t, x) -> Some (Json_config.tuple_index_to_field_name i, go t x)
              )
              |> List.filter_map identity)
        end
      | Map (`string, t), Expr.Map xs -> `obj (List.map (fun (k, v) -> k, go t v) xs)
      | Option t, Expr.Some x -> go t x
      | Option _, Expr.None -> `null
      | StringEnum cases, Expr.StringEnum s ->
          cases
          |> List.find_opt (fun (name, _, _) -> name = s)
          |> begin function
          | Some c -> `str (Json_config.get_mangled_name_of_string_enum_case ~inherited:base_mangling_style c)
          | None -> fail "string enum case not found for the given name"
          end
      | _, _ -> fail "type mismatch"
    in
    go ct.ct_desc expr
  in
  let record_to_json_fields base_mangling_style (fields: record_field list) (expr: Expr.t StringMap.t) =
    fields |&>> fun field ->
      let { rf_name; rf_type; rf_configs; _ } = field in
      let (json_field_name, base_mangling_style) = Json_config.get_mangled_name_of_field ~inherited:base_mangling_style field in
      let nested_style = Json_config.get_nested_field_style rf_configs in
      match StringMap.find_opt field.rf_name expr with
      | None -> fail (sprintf "missing field '%s'" rf_name)
      | Some value ->
        let ct, is_option = nested_type_to_coretype rf_type in
        match nested_style, rf_type with
        | `spreading, _ ->
          begin match validate_spreading_type rf_type with
          | `record_decl _ | `variant_decl _ ->
            begin match expr_to_json base_mangling_style ct value with
            | `obj fields -> fields
            | _ -> fail "Only record decl or variant decl can be spread."
            end
          end
        | `nested, _ when is_option && value = Expr.None -> []
        | `nested, _ -> [ json_field_name, expr_to_json base_mangling_style ct value ]
  in
  let variant_to_json base_mangling_style (ctor: variant_constructor) (expr: 'a Refl.constructor) =
    let discriminator_fname = Json_config.get_mangled_name_of_discriminator_field ~inherited:base_mangling_style td in
    let (discriminator_value, base_mangling_style) = Json_config.get_mangled_name_of_discriminator ~inherited:base_mangling_style ctor in
    let discriminator_field = [ discriminator_fname, `str discriminator_value ] in
    let arg_fname = Json_config.get_mangled_name_of_variant_arg ~inherited:base_mangling_style ctor in
    match Json_config.get_variant_style ctor.vc_configs with
    | `flatten ->
      begin match ctor.vc_param, expr with
      | `no_param, NoParam _ -> `obj discriminator_field
      | `inline_record fields, InlineRecord { get; _ } ->
        let fields = record_to_json_fields base_mangling_style fields (get value) in
        `obj (discriminator_field @ fields)
      | `reused_inline_record { td_kind; td_configs; _ }, ReusedInlineRecord { get; _ } ->
        let base_mangling_style =
          Json_config.(get_mangling_style_opt td_configs |? default_mangling_style)
        in
        let fields = td_kind |> function
          | Record_decl fields -> fields
          | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." ctor.vc_name
        in
        let fields = record_to_json_fields base_mangling_style fields (get value) in
        `obj (discriminator_field @ fields)
      | `tuple_like [ va ], TupleLike { get; _ }
        when Json_config.get_nested_field_style va.va_configs = `spreading ->
        begin match validate_spreading_type va.va_type, get value with
        | (`record_decl _ | `variant_decl _), [e] ->
          let base_mangling_style = Json_config.get_mangling_style_opt va.va_configs |? base_mangling_style in
          let ct, _ = nested_type_to_coretype va.va_type in
          let jv = expr_to_json base_mangling_style ct e in
          begin match jv with
          | `obj fields -> `obj (discriminator_field @ fields)
          | _ -> fail "Only record decl or variant decl can be spread." end
        | (`record_decl _ | `variant_decl _), _ ->
          fail "tuple length mismatch"
        end
      | `tuple_like ts, TupleLike { get; _ } ->
        let variant_argument_to_json base_mangling_style va expr =
          if Json_config.get_nested_field_style va.va_configs = `spreading then
            invalid_arg "Spreading of the nested type of tuple_like is only valid if there is only a single argument.";
          let ct, is_option = nested_type_to_coretype va.va_type in
          expr_to_json base_mangling_style ct expr, is_option
        in
        begin match ts, get value, Json_config.get_tuple_style ctor.vc_configs with
        | [], [], _ -> `obj discriminator_field
        | [t], [e], _ ->
          let value, is_option = variant_argument_to_json base_mangling_style t e in
          if is_option && e = Expr.None then
            `obj discriminator_field
          else
            `obj (discriminator_field @ [arg_fname, value])
        | ts, es, `arr ->
          let value = `arr (List.map2 (fun va expr -> variant_argument_to_json base_mangling_style va expr |> fst) ts es) in
          `obj (discriminator_field @ [arg_fname, value])
        | ts, es, `obj `default ->
          let fields = List.combine ts es |> List.mapi (fun i (t, e) ->
            let value, is_option = variant_argument_to_json base_mangling_style t e in
            if is_option && e = Expr.None then None
            else
              Some (Json_config.tuple_index_to_field_name i, value)
            ) |&?> identity
          in
          `obj (discriminator_field @ fields)
        end
      | _, _ -> fail "param style mismatch"
      end
  in
  let base_mangling_style = Json_config.(get_mangling_style_opt td_configs |? default_mangling_style) in
  match td_kind, Typed.reflect a with
  | Alias_decl ct, Alias { get; _ } -> expr_to_json base_mangling_style ct (get value)
  | Record_decl fields, Record { get; _ } -> `obj (record_to_json_fields base_mangling_style fields (get value))
  | Variant_decl ctors, Variant { classify; _ } ->
    let name, ref_ctor = classify value in
    begin match ctors |> List.find_opt (fun { vc_name; _ } -> vc_name = name) with
    | Some ctor -> variant_to_json base_mangling_style ctor ref_ctor
    | None -> fail (sprintf "missing constructor '%s'" name)
    end
  | _, _ -> fail "kind mismatch"
