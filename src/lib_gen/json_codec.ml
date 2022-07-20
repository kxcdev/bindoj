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

open Ppxlib
open Ast_builder.Default
open Ast_helper
open Utils
open Bindoj_base
open Bindoj_base.Type_desc

include Bindoj_codec.Json.Config

let get_encoder_name type_name = function
  | `default -> type_name^"_to_json"
  (* | `codec_val v -> v *)
  | `in_module m -> m^".to_json"
let get_decoder_name type_name = function
  | `default -> type_name^"_of_json"
  (* | `codec_val v -> v *)
  | `in_module m -> m^".of_json"

type builtin_codec = {
  encoder: expression;
  decoder: expression;
  (* validator stuffs are meant to go here *)
}

module Builtin_codecs = struct
  open struct let loc = Location.none end

  let unit = {
      encoder = [%expr fun () -> (`null : Kxclib.Json.jv)];
      decoder = [%expr function `null -> Some () | _ -> None];
    }
  let bool = {
      encoder = [%expr fun (x : bool) -> (`bool x : Kxclib.Json.jv)];
      decoder = [%expr function
        | (`bool x : Kxclib.Json.jv) -> Some x
        | (_ : Kxclib.Json.jv) -> None
      ];
    }
  let int = {
      encoder = [%expr fun (x : int) -> (`num (float_of_int x) : Kxclib.Json.jv)];
      decoder = [%expr function
        | (`num x : Kxclib.Json.jv) -> Some (int_of_float x)
        | (_ : Kxclib.Json.jv) -> None
      ];
    }
  let float = {
      encoder = [%expr fun (x : float) -> (`num x : Kxclib.Json.jv)];
      decoder = [%expr function
        | (`num x : Kxclib.Json.jv) -> Some x
        | _ -> None
      ];
    }
  let string = {
      encoder = [%expr fun (x : string) -> (`str x : Kxclib.Json.jv)];
      decoder = [%expr function
        | (`str x : Kxclib.Json.jv) -> Some x
        | _ -> None
      ];
    }
  let uchar = {
      encoder = [%expr fun (x: Uchar.t) -> (`str (String.of_seq (List.to_seq [Uchar.to_char x])) : Kxclib.Json.jv)];
      decoder = [%expr function
        | (`str x : Kxclib.Json.jv) ->
            if String.length x = 1 then Some (Uchar.of_char (String.get x 0)) else None
        | _ -> None
      ];
    }
  let byte = {
      encoder = [%expr fun (x: char) -> (`num (float_of_int (int_of_char x)) : Kxclib.Json.jv)];
      decoder = [%expr function
        | (`num x : Kxclib.Json.jv) ->
            let x = int_of_float x in
            if 0 <= x && x <= 255 then Some (char_of_int x) else None
        | (_ : Kxclib.Json.jv) -> None
      ];
    }
  let bytes = {
      encoder = [%expr fun (x : Bytes.t) -> (`str (Kxclib.Base64.encode x) : Kxclib.Json.jv)];
      decoder = [%expr function
        | (`str x : Kxclib.Json.jv) ->
          (try Some (Kxclib.Base64.decode x) with Invalid_argument _msg -> None)
        | _ -> None
      ];
    }
  let option = {
      encoder = [%expr fun t_to_json -> function
        | Some x -> t_to_json x
        | None -> (`null : Kxclib.Json.jv)
      ];
      decoder = [%expr fun t_of_json -> function
        | `null -> Some None
        | x ->
          match t_of_json x with
          | Some x -> Some (Some x)
          | None -> None
      ];
    }
  let list = {
      encoder = [%expr fun t_to_json xs -> (`arr (List.map t_to_json xs) : Kxclib.Json.jv)];
      decoder = [%expr fun t_of_json -> function
        | (`arr xs : Kxclib.Json.jv) ->
          let result = List.filter_map t_of_json xs in
          if List.length xs = List.length result then
            Some result
          else
            None
        | _ -> None
      ];
    }
  let uninhabitable = {
      encoder = [%expr fun () -> (`null : Kxclib.Json.jv)];
      decoder = [%expr function `null -> Some () | _ -> None];
    }
  let map = {
      encoder = [%expr fun key_to_string v_to_json fields ->
        let fields =
          fields |> List.map (fun (k, v) -> key_to_string k, v_to_json v)
        in
        (`obj fields : Kxclib.Json.jv)
      ];
      decoder = [%expr fun key_of_string v_of_json -> function
        | `obj fields ->
          let result =
            List.filter_map (fun (k, v) ->
                match key_of_string k, v_of_json v with
                | Some k, Some v -> Some (k, v)
                | _, _ -> None
              ) fields
          in
          if List.length fields = List.length result then
            Some result
          else
            None
        | _ -> None
      ];
    }

  let all = [
      "unit", unit;
      "bool", bool;
      "int", int;
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

let codec_of_coretype ~get_custom_codec ~get_name ~map_key_converter ~tuple_case ~string_enum_case self_ename (ct: coretype) =
  let open Coretype in
  let loc = Location.none in
  match get_custom_codec ct.ct_configs with
  | Some coder -> evar coder
  | None ->
    let evar_name ?(codec = `default) name =
      get_name name codec |> evar
    in
    let rec go = function
      | Prim p -> evar_name (Coretype.string_of_prim p)
      | Uninhabitable -> evar_name "uninhabitable"
      | Ident i -> evar_name ~codec:i.id_codec i.id_name
      | Option t -> [%expr [%e evar_name "option"] [%e go t]] (* option_of_json t_of_json *)
      | List t -> [%expr [%e evar_name "list"] [%e go t]] (* list_of_json t_of_json *)
      | Map (k, v) -> [%expr [%e evar_name "map"] [%e map_key_converter k] [%e go v]] (* map_of_json key_of_string t_of_json *)
      | Tuple ts -> tuple_case go ts
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

let encoder_of_coretype =
  let open Coretype in
  let vari i = "x"^string_of_int i in
  let loc = Location.none in
  let evari i = evar (vari i) in
  let pvari i = pvar (vari i) in
  let tuple_case (go: desc -> expression) (ts: desc list) =
    let args =
      ts |> List.mapi (fun i _ -> pvari i)
         |> Pat.tuple
    in
    let rec mk_list acc = function
      | [] -> acc
      | x :: xs -> mk_list [%expr [%e x] :: [%e acc]] xs
    in
    let ret =
      ts |> List.mapi (fun i t -> [%expr [%e go t] [%e evari i]])
         |> List.rev |> mk_list [%expr []]
    in
    [%expr fun [%p args] -> (`arr [%e ret] : Kxclib.Json.jv)]
  in
  let map_key_converter (k: map_key) = (* key_to_string *)
    match k with
    | `string -> [%expr fun (k: string) -> k]
  in
  let string_enum_case (cs: string list) =
    let cases =
      cs |> List.map (fun c ->
        let pat = Pat.variant (Utils.escape_as_constructor_name c) None in
        let expr = Exp.constant (Const.string c) in
        Exp.case pat expr
      )
    in
    Exp.function_ cases
  in
  codec_of_coretype
    ~get_custom_codec:Json_config.get_custom_encoder
    ~get_name:get_encoder_name
    ~tuple_case ~map_key_converter ~string_enum_case

let decoder_of_coretype =
  let open Coretype in
  let vari i = "x"^string_of_int i in
  let loc = Location.none in
  let evari i = evar (vari i) in
  let pvari i = pvar (vari i) in
  let tuple_case (go: desc -> expression) (ts: desc list) =
    let rec mk_list acc = function
      | [] -> acc
      | x :: xs -> mk_list [%pat? [%p x] :: [%p acc]] xs
    in
    let args =
      ts |> List.mapi (fun i _ -> pvari i)
         |> List.rev |> mk_list [%pat? []]
    in
    let ret =
      let tmp, args, ret =
        ts |> List.mapi (fun i t -> [%expr [%e go t] [%e evari i]]) |> Exp.tuple,
        ts |> List.mapi (fun i _ -> [%pat? Some [%p pvari i]]) |> Pat.tuple,
        ts |> List.mapi (fun i _ -> [%expr [%e evari i]]) |> Exp.tuple
      in
      [%expr match [%e tmp] with [%p args] -> Some [%e ret] | _ -> None]
    in
    [%expr function
      | (`arr [%p args] : Kxclib.Json.jv) -> [%e ret]
      | _ -> None
    ]
  in
  let map_key_converter (k: map_key) = (* key_of_string *)
    match k with
    | `string -> [%expr fun (s: string) -> Some s]
  in
  let string_enum_case (cs: string list) =
    let cases =
      cs |> List.map (fun c ->
        let pat = Pat.constant (Const.string c) in
        let expr = Exp.variant (Utils.escape_as_constructor_name c) None in
        Exp.case pat [%expr Some [%e expr]]
      ) |> fun cases ->
        cases @ [
          Exp.case (Pat.any ()) [%expr None]
        ]
    in
    [%expr function
      | `str s -> [%e Exp.function_ cases] s
      | _ -> None
    ]
  in
  codec_of_coretype
    ~get_custom_codec:Json_config.get_custom_decoder
    ~get_name:get_decoder_name
    ~tuple_case ~map_key_converter ~string_enum_case

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

let gen_json_encoder :
      ?self_contained:bool
      -> ?codec:Coretype.codec
      -> type_decl
      -> value_binding =
  fun ?(self_contained=false) ?(codec=`default) td ->
  let { td_name; td_kind=kind; td_configs; _ } = td in
  let loc = Location.none in
  let self_name =
    match codec with
    | `default -> td_name ^ "_to_json"
    | `in_module _ -> "to_json"
  in
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
    fun i { rf_name; rf_type; _ } ->
    [%expr ([%e estring ~loc rf_name],
            [%e encoder_of_coretype self_ename rf_type] [%e evari i])]
  in
  let record_body : record_field list -> expression = fun fields ->
    let members = List.mapi member_of_field fields in
    [%expr `obj [%e elist ~loc members]]
  in
  let variant_params : variant_constructor list -> pattern list = fun constrs ->
    constrs |&> fun { vc_name; vc_param; _ } ->
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
      | `inline_record fields ->
        begin match Caml_config.get_variant_type td_configs with
        | `regular ->
          ppat_construct ~loc
            (lidloc ~loc vc_name)
            (Some (record_params fields))
        | `polymorphic ->
          failwith' "case '%s' with an inline record cannot be used in a polymorphic variant" vc_name
        end
  in
  let variant_body : variant_constructor list -> expression list = fun cnstrs ->
    cnstrs |&> fun { vc_name; vc_param; vc_configs; _ } ->
      let discriminator = Json_config.get_variant_discriminator td_configs in
      let arg_fname = Json_config.get_name Json_config.default_name_of_variant_arg vc_configs in
      match Json_config.get_variant_style vc_configs with
      | `flatten -> begin
        match vc_param with
        | `no_param ->
          let cstr = [%expr ([%e estring ~loc discriminator], `str [%e estring ~loc vc_name])] in
          [%expr `obj [[%e cstr]]]
        | `tuple_like args ->
          let discriminator = estring ~loc discriminator in
          let arg_fname = estring ~loc arg_fname in
          let cstr = [%expr ([%e discriminator], `str [%e estring ~loc vc_name])] in
          let args =
            List.mapi (fun i typ ->
                [%expr [%e encoder_of_coretype self_ename typ] [%e evari i]])
              args in
          begin match args with
          | [] -> [%expr `obj [[%e cstr]]]
          | [arg] -> [%expr `obj [[%e cstr]; ([%e arg_fname], [%e arg])]]
          | _ -> [%expr `obj [[%e cstr]; ([%e arg_fname], `arr [%e elist ~loc args])]]
          end
        | `inline_record fields ->
          let discriminator = estring ~loc discriminator in
          let cstr = [%expr ([%e discriminator], `str [%e estring ~loc vc_name])] in
          let args = List.mapi (fun i field -> member_of_field i field) fields in
          [%expr `obj [%e elist ~loc (cstr :: args)]]
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

let gen_json_decoder :
      ?self_contained:bool
      -> ?codec:Coretype.codec
      -> type_decl
      -> value_binding =
  fun ?(self_contained=false) ?(codec=`default) td ->
  let { td_name; td_kind=kind; td_configs; _ } = td in
  let loc = Location.none in
  let self_name =
    match codec with
    | `default -> td_name ^ "_of_json"
    | `in_module _ -> "of_json"
  in
  let self_pname = pvar self_name in
  let self_ename = evar self_name in
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
  let bind_options : (pattern * expression) list -> expression -> expression = fun bindings body ->
    [%expr
     let (>>=) = Option.bind in
         [%e List.fold_right (fun (p, e) body ->
               [%expr [%e e] >>= (fun [%p p] -> [%e body])])
             bindings body]]
  in
  let record_bindings : record_field list -> (pattern * expression) list = fun fields ->
    List.mapi (fun i { rf_name; rf_type; _; } ->
        let expr =
          if Coretype.is_option rf_type then
            [%expr
              List.assoc_opt [%e estring ~loc rf_name] [%e param_e]
              |> Option.value ~default:`null
              |> [%e decoder_of_coretype self_ename rf_type]]
          else
            [%expr
              List.assoc_opt [%e estring ~loc rf_name] [%e param_e]
              >>= [%e decoder_of_coretype self_ename rf_type]]
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
  let variant_params : variant_constructor list -> pattern list = fun cstrs ->
    cstrs |&> fun { vc_name; vc_param; vc_configs; _ } ->
      let discriminator = Json_config.get_variant_discriminator td_configs in
      let arg_fname = Json_config.get_name Json_config.default_name_of_variant_arg vc_configs in
      match Json_config.get_variant_style vc_configs with
      | `flatten -> begin
        match vc_param with
        | `no_param ->
          let discriminator = pstring ~loc discriminator in
          let cstr = [%pat? ([%p discriminator], `str [%p pstring ~loc vc_name])] in
          [%pat? `obj [[%p cstr]]]
        | `tuple_like args ->
          let discriminator = pstring ~loc discriminator in
          let arg_fname = pstring ~loc arg_fname in
          let cstr = [%pat? ([%p discriminator], `str [%p pstring ~loc vc_name])] in
          let args = List.mapi (fun i _ -> pvari i) args in
          begin match args with
          | [] -> [%pat? `obj [[%p cstr]]]
          | [arg] -> [%pat? `obj [[%p cstr]; ([%p arg_fname], [%p arg])]]
          | _ -> [%pat? `obj [[%p cstr]; ([%p arg_fname], `arr [%p plist ~loc args])]]
          end
        | `inline_record _ ->
          let discriminator = pstring ~loc discriminator in
          let cstr = [%pat? ([%p discriminator], `str [%p pstring ~loc vc_name])] in
          [%pat? `obj ([%p cstr] :: [%p param_p])]
      end
  in
  let variant_body : variant_constructor list -> expression list = fun cstrs ->
    let construct name args =
      match Caml_config.get_variant_type td_configs with
      | `regular -> Exp.construct (lidloc name) args
      | `polymorphic -> Exp.variant name args
    in
    cstrs |&> fun { vc_name; vc_param; _ } ->
      match vc_param with
      | `no_param -> [%expr Some [%e construct vc_name None]]
      | `tuple_like args ->
        begin match args with
        | [] -> [%expr Some [%e construct vc_name None]]
        | _ ->
          let bindings : (pattern * expression) list =
            List.mapi (fun i arg ->
                (pvari i,
                  [%expr [%e decoder_of_coretype self_ename arg] [%e evari i]]))
              args in
          let body : expression =
            [%expr Some
                [%e construct
                    vc_name
                    (Some (pexp_tuple ~loc (List.mapi (fun i _ -> evari i) args)))]] in
          bind_options bindings body
        end
      | `inline_record fields ->
        begin match fields with
        | [] -> construct vc_name None
        | _ ->
          let bindings = record_bindings fields in
          let body =
            match Caml_config.get_variant_type td_configs with
            | `regular -> record_body fields
            | `polymorphic -> failwith' "case '%s' with an inline record cannot be used in a polymorphic variant" vc_name
          in
          bind_options bindings [%expr Some [%e (construct vc_name (Some body))]]
        end
  in
  begin match kind with
  | Alias_decl cty ->
    Vb.mk
      ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
      self_pname
      (pexp_constraint ~loc
         (wrap_self_contained (decoder_of_coretype self_ename cty))
         [%type: Kxclib.Json.jv -> [%t typcons ~loc td_name] option])
  | Record_decl fields ->
    let bindings = record_bindings fields in
    let body = record_body fields in
    Vb.mk
      ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
      self_pname
      (pexp_constraint ~loc
        (wrap_self_contained
            [%expr function
                | `obj [%p param_p] -> [%e bind_options bindings [%expr Some [%e body]]]
                | _ -> None])
        [%type: Kxclib.Json.jv -> [%t typcons ~loc td_name] option])
  | Variant_decl ctors ->
    let params = variant_params ctors in
    let body = variant_body ctors in
    let discriminator = Json_config.get_variant_discriminator td_configs in
    let cases =
      List.map2
        (fun p b -> case ~lhs:p ~rhs:b ~guard:None)
        params body
      @ [(case ~lhs:(ppat_any ~loc) ~rhs:[%expr None] ~guard:None)] in
    Vb.mk
      ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
      self_pname
      (pexp_constraint ~loc
        [%expr fun __bindoj_orig ->
            Kxclib.Jv.pump_field [%e estring ~loc discriminator ] __bindoj_orig
            |> [%e (wrap_self_contained (pexp_function ~loc cases))]]
        [%type: Kxclib.Json.jv -> [%t typcons ~loc td_name] option])
  end

open Bindoj_openapi.V3

let base64_regex = {|^(?:[A-Za-z0-9+\/]{4})*(?:[A-Za-z0-9+\/][AQgw]==|[A-Za-z0-9+\/]{2}[AEIMQUYcgkosw048]=)?$|}

let gen_openapi_schema : type_decl -> Json.jv =
  let schema = Schema_object.schema in

  let docopt = function `docstr s -> Some s | `nodoc -> None in

  let convert_coretype ~self_id ?description (ct: coretype) =
    let rec go =
      let open Coretype in
      function
      | Prim `unit -> Schema_object.null ?description ()
      | Prim `bool -> Schema_object.boolean ?description ()
      | Prim `int -> Schema_object.integer ?description ()
      | Prim `float -> Schema_object.number ?description ()
      | Prim `string -> Schema_object.string ?description ()
      | Prim `uchar -> Schema_object.string ~minLength:1 ~maxLength:1 ?description ()
      | Prim `byte -> Schema_object.integer ~minimum:0 ~maximum:255 ?description ()
      | Prim `bytes -> Schema_object.string ~format:`byte ~pattern:base64_regex ?description ()
      | Uninhabitable -> Schema_object.null ?description ()
      | Ident id -> Schema_object.ref ("#" ^ id.id_name)
      | Option t ->
        Schema_object.oneOf ?description [go t; Schema_object.null ()]
      | Tuple ts ->
        Schema_object.tuple ?description (ts |> List.map go )
      | List t ->
        Schema_object.array ?description ~items:(`T (go t)) ()
      | Map (`string, t) ->
        Schema_object.obj ?description ~additionalProperties:(`T (go t)) ()
      | StringEnum cases ->
        let enum = cases |> List.map (fun case -> `str case) in
        Schema_object.string ~enum ()
      | Self -> Schema_object.ref self_id
    in
    go ct.ct_desc
  in

  let record_to_t ?schema ~self_id ?id ?(additional_fields = []) ~name ~doc fields =
    let field_to_t field =
      field.rf_name,
      convert_coretype ~self_id ?description:(docopt field.rf_doc) field.rf_type
    in
    let fields = fields |> List.map field_to_t in
    Schema_object.record
      ?schema
      ~title:name
      ?description:(docopt doc)
      ?id
      (fields @ additional_fields)
  in

  fun { td_name = name; td_kind; td_doc = doc; td_configs } ->
    let self_id = "#" ^ name in
    let id = self_id in
    let so =
      match td_kind with
      | Record_decl fields ->
        record_to_t ~schema ~self_id ~id ~name ~doc fields
      | Variant_decl ctors ->
        let discriminator = Json_config.get_variant_discriminator td_configs in
        let ctor_to_t { vc_name = name; vc_param; vc_doc = doc; vc_configs } =
          let discriminator_field =
            let enum = [`str name] in
            [discriminator, Schema_object.string ~enum ()]
          in
          match Json_config.get_variant_style vc_configs with
          | `flatten ->
            begin match vc_param with
            | `no_param | `tuple_like [] ->
              Schema_object.record ?description:(docopt doc) ~title:name discriminator_field
            | `tuple_like (t :: ts) ->
              let arg_name = Json_config.get_name Json_config.default_name_of_variant_arg vc_configs in
              let arg_field =
                match ts with
                | [] -> [arg_name, convert_coretype ~self_id t]
                | _ ->
                  let ts = (t :: ts) |> List.map (convert_coretype ~self_id) in
                  [arg_name, Schema_object.tuple ts]
              in
              let fields = discriminator_field @ arg_field in
              Schema_object.record ?description:(docopt doc) ~title:name fields
            | `inline_record fields ->
              record_to_t ~self_id ~additional_fields:discriminator_field ~name ~doc fields
            end
        in
        Schema_object.oneOf
          ~schema
          ~title:name
          ?description:(docopt doc)
          ~id
          (ctors |> List.map ctor_to_t)
      | Alias_decl cty ->
        convert_coretype ~self_id ?description:(docopt doc) cty
    in
    Schema_object.to_json so
