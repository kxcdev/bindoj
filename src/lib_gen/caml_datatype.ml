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
open Ast_helper
open Utils
open Bindoj_base
open Bindoj_base.Type_desc

let rec type_declaration_of_type_decl :
          ?type_name:string
          -> ?attrs:attribute list
          -> type_decl
          -> type_declaration =
  fun ?type_name ?(attrs=[]) td ->
  let self_name = type_name |? td.td_name in
  let doc = td.td_doc in
  let attrs = attrs @ doc_attribute doc in
  Type.mk
    ~kind:(type_kind_of_type_decl_kind ~self_name td)
    ?manifest:(type_manifest_of_type_decl_kind ~self_name td)
    ~attrs
    (locmk self_name)

and type_kind_of_type_decl_kind : self_name:string -> type_decl -> type_kind =
  fun ~self_name td ->
  match td.td_kind with
  | Alias_decl _ -> Ptype_abstract
  | Record_decl fields ->
    Ptype_record (fields |> label_declarations_of_record_fields ~self_name)
  | Variant_decl ctors ->
    match Caml_config.get_variant_type td.td_configs with
    | `regular ->
      Ptype_variant (ctors |> constructor_declarations_of_variant_constructors ~self_name)
    | `polymorphic -> Ptype_abstract

and type_manifest_of_type_decl_kind : self_name:string -> type_decl -> core_type option =
  fun ~self_name td ->
  match td.td_kind with
  | Alias_decl cty -> Some (type_of_coretype ~self_name cty)
  | Record_decl _ -> None
  | Variant_decl ctors ->
    match Caml_config.get_variant_type td.td_configs with
    | `regular -> None
    | `polymorphic ->
      Some (Utils.type_of_polymorphic_variant ~self_name ctors)

and label_declarations_of_record_fields ~self_name fields =
  fields |&> fun field ->
    Type.field
      ~attrs:(doc_attribute field.rf_doc)
      (locmk field.rf_name)
      (type_of_nested_type ~self_name field.rf_type)

and constructor_declarations_of_variant_constructors ~self_name ctors =
  ctors |&> fun ctor ->
    let name, args =
      match ctor.vc_param with
      | `no_param ->
        ctor.vc_name, Pcstr_tuple []
      | `tuple_like ts ->
        ctor.vc_name, Pcstr_tuple (ts |&> fun { va_type; _ } -> type_of_nested_type ~self_name va_type)
      | `inline_record fields ->
        ctor.vc_name, Pcstr_record (fields |> label_declarations_of_record_fields ~self_name)
      | `reused_inline_record decl ->
        let fields = decl.td_kind |> function
          | Record_decl fields -> fields
          | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." ctor.vc_name
        in
        ctor.vc_name, Pcstr_record (fields |> label_declarations_of_record_fields ~self_name)
    in
    Type.constructor ~attrs:(doc_attribute ctor.vc_doc) ~args (locmk name)

open Bindoj_base.Typed_type_desc

let vari ?(basename="x") i = sprintf "%s%d" basename i
let evari ?(basename="x") i = evar (vari ~basename i)
let pvari ?(basename="x") i = pvar (vari ~basename i)

let gen_reflect_name ?(codec=(`default : Coretype.codec)) td =
  match codec with
  | `default -> td.td_name ^ "_reflect"
  | `open_ m -> sprintf "%s.%s_reflect" m td.td_name
  | `in_module _ -> "reflect"

let rec gen_reflect ?(codec=(`default : Coretype.codec)) td : value_binding =
  let loc = Location.none in
  let self_name = gen_reflect_name ~codec td in
  let body =
    match td.td_kind with
    | Alias_decl cty -> gen_reflect_alias ~self_name cty
    | Record_decl fields -> gen_reflect_record ~self_name fields
    | Variant_decl ctors ->
      let vty = Caml_config.get_variant_type td.td_configs in
      let poly = vty = `polymorphic in
      gen_reflect_variant ~self_name ~poly ctors
  in
  let with_opens e = [%expr
    let open Bindoj_runtime in
    let open Kxclib.Option.Ops_monad in
    [%e e]]
  in
  Vb.mk
    ~attrs:(warning_attribute "-33-39") (* suppress unused rec & open warning *)
    [%pat? ([%p pvar self_name] : _ Bindoj_runtime.Refl.t)]
    [%expr lazy [%e with_opens body]]

and get_refl (id: Coretype.ident) : expression =
  let name =
    match id.id_codec with
    | `default -> Longident.parse (id.id_name ^ "_reflect")
    | `open_ m -> Longident.(Ldot (Lident m, id.id_name ^ "_reflect"))
    | `in_module m -> Longident.(Ldot (Lident m, "reflect"))
  in
  Exp.ident (locmk name)

and coretype_of_expr ~self_name (ct: coretype) =
  let loc = Location.none in
  let open MonadOps(Option) in
  let rec go =
    function
    | Coretype.Uninhabitable
    | Prim `unit   -> [%expr Expr.to_unit]
    | Prim `bool   -> [%expr Expr.to_bool]
    | Prim `int    -> [%expr Expr.to_int]
    | Prim `int53p -> [%expr Expr.to_int53p]
    | Prim `float  -> [%expr Expr.to_float]
    | Prim `string -> [%expr Expr.to_string]
    | Prim `uchar  -> [%expr Expr.to_uchar]
    | Prim `byte   -> [%expr Expr.to_byte]
    | Prim `bytes  -> [%expr Expr.to_bytes]
    | Option t -> [%expr Expr.to_option [%e go t]]
    | List t -> [%expr Expr.to_list [%e go t]]
    | Map (`string, t) -> [%expr Expr.to_map [%e go t]]
    | Tuple ts ->
      let elems = ts |> List.mapi (fun i t -> pvari i, (evari i, go t)) in
      let pat = elems |> List.map fst |> plist in
      let body =
        elems |> List.map (fun (_, (ev, _)) -> ev) |> Exp.tuple
      in
      let expr =
        elems
        |> List.rev
        |> List.fold_left (fun expr (pv, (ev, conv)) ->
            [%expr [%e conv] [%e ev] >>= fun [%p pv] -> [%e expr]]
          ) [%expr Some [%e body]]
      in
      Exp.function_ [
        Exp.case [%pat? Expr.Tuple [%p pat]] expr;
        Exp.case (Pat.any ()) [%expr None]
      ]
    | StringEnum cs ->
      let cases =
        cs |> List.map (fun (c, _, _) ->
          let pat = [%pat? Expr.StringEnum [%p Pat.constant (Const.string c)]] in
          let expr = Exp.variant (Utils.escape_as_constructor_name c) None in
          Exp.case pat [%expr Some [%e expr]]
        )
      in
      Exp.function_ (cases @ [Exp.case (Pat.any ()) [%expr None]])
    | Self -> [%expr Expr.to_refl [%e evar self_name]]
    | Ident i -> [%expr Expr.to_refl [%e get_refl i]]
  in
  go ct.ct_desc

and nested_type_of_expr ~self_name = function
  | `direct ct | `nested({ td_kind = Alias_decl ct; _ }, _) ->
    coretype_of_expr ~self_name ct
  | `nested({ td_name; _}, id_codec) ->
    let loc = Location.none in
    let ident = { Coretype.id_name = td_name; id_codec } in
    [%expr Expr.to_refl [%e get_refl ident]]

and coretype_to_expr ~self_name (ct: coretype) =
  let loc = Location.none in
  let rec go =
    function
    | Coretype.Uninhabitable
    | Prim `unit   -> [%expr Expr.of_unit]
    | Prim `bool   -> [%expr Expr.of_bool]
    | Prim `int    -> [%expr Expr.of_int]
    | Prim `int53p -> [%expr Expr.of_int53p]
    | Prim `float  -> [%expr Expr.of_float]
    | Prim `string -> [%expr Expr.of_string]
    | Prim `uchar  -> [%expr Expr.of_uchar]
    | Prim `byte   -> [%expr Expr.of_byte]
    | Prim `bytes  -> [%expr Expr.of_bytes]
    | Option t -> [%expr Expr.of_option [%e go t]]
    | List t -> [%expr Expr.of_list [%e go t]]
    | Map (`string, t) -> [%expr Expr.of_map [%e go t]]
    | Tuple ts ->
      let elems = ts |> List.mapi (fun i t -> pvari i, [%expr [%e go t] [%e evari i]]) in
      let pat = Pat.tuple (elems |> List.map fst) in
      let list = elems |> List.map snd |> elist in
      [%expr fun [%p pat] -> Expr.Tuple ([%e list])]
    | StringEnum cs ->
      let cases =
        cs |> List.map (fun (c, _, _) ->
          let pat = Pat.variant (Utils.escape_as_constructor_name c) None in
          let expr = [%expr Expr.StringEnum ([%e Exp.constant (Const.string c)])] in
          Exp.case pat expr
        )
      in
      Exp.function_ cases
    | Self -> [%expr Expr.of_refl [%e evar self_name]]
    | Ident i -> [%expr Expr.of_refl [%e get_refl i]]
  in
  go ct.ct_desc

and nested_type_to_expr ~self_name = function
  | `direct ct | `nested({ td_kind = Alias_decl ct; _ }, _) ->
    coretype_to_expr ~self_name ct
  | `nested({ td_name; _}, id_codec) ->
    let loc = Location.none in
    let ident = { Coretype.id_name = td_name; id_codec } in
    [%expr Expr.of_refl [%e get_refl ident]]

and gen_reflect_alias ~self_name (ct: coretype) : expression =
  let loc = Location.none in
  [%expr Refl.Alias {
    get = [%e coretype_to_expr ~self_name ct];
    mk = [%e coretype_of_expr ~self_name ct]
  }]

and gen_reflect_record ~self_name (fields: record_field list) : expression =
  let (get_pat, get_expr), mk = gen_reflect_record_impl  ~self_name fields in
  let loc = Location.none in
  [%expr Refl.Record {
    get = (fun [%p get_pat] -> [%e get_expr]);
    mk = [%e mk]
  }]

and gen_reflect_record_impl ~self_name ?(mk_ctor=identity) (fields: record_field list) : (pattern * expression) * expression =
  let loc = Location.none in
  let get_pat =
    let field_pats = fields |> List.map (fun field -> lidloc field.rf_name, pvar field.rf_name) in
    Pat.record field_pats Closed
  in
  let get_expr =
    fields |> List.map (fun field ->
      Exp.tuple [
        Exp.constant (Const.string (field.rf_name));
        [%expr [%e nested_type_to_expr ~self_name field.rf_type] [%e evar field.rf_name]]
      ]
    ) |> elist |> fun e -> [%expr StringMap.of_list [%e e]]
  in
  let mk_body =
    let fields = fields |> List.map (fun field -> lidloc field.rf_name, evar field.rf_name) in
    mk_ctor (Exp.record fields None)
  in
  let mk_expr arg =
    fields |> List.rev |> List.fold_left (fun body field ->
      let sname = Exp.constant (Const.string (field.rf_name)) in
      let pname = pvar field.rf_name in
      [%expr
        [%e arg]
        |> StringMap.find_opt [%e sname]
        >>= [%e nested_type_of_expr ~self_name field.rf_type]
        >>= fun [%p pname] -> [%e body]
      ]
    ) [%expr Some [%e mk_body]]
  in
  let mk = [%expr fun xs -> [%e mk_expr [%expr xs]]] in
  (get_pat, get_expr), mk

and gen_reflect_variant_impl ~self_name ~poly (ctors: variant_constructor list) : (variant_constructor * expression) list =
  let loc = Location.none in
  let ctor_expr ctor ?arg () =
    if poly then
      Exp.variant ctor.vc_name arg
    else
      Exp.construct (lidloc ctor.vc_name) arg
  in
  let ctor_pat ctor ?arg () =
    if poly then
      Pat.variant ctor.vc_name arg
    else
      Pat.construct (lidloc ctor.vc_name) arg
  in
  ctors |> List.map (fun ctor ->
    let ctor_expr = ctor_expr ctor in
    let ctor_pat = ctor_pat ctor in
    let invalid = [%expr invalid_arg ([%e Exp.constant (Const.string (ctor.vc_name ^ " is expected"))])] in
    let of_record_fields ~label fields make_refl =
      if poly then failwith' "case '%s' with an %s cannot be used in a polymorphic variant" ctor.vc_name label
      else
        let (get_pat, get_expr), mk =
          gen_reflect_record_impl ~mk_ctor:(fun x -> ctor_expr ~arg:x ()) ~self_name fields
        in
        let get = [%expr function
          | [%p ctor_pat ~arg:get_pat ()] -> [%e get_expr]
          | _ -> [%e invalid]]
        in
        ctor, make_refl get mk
    in
    match ctor.vc_param with
    | `no_param ->
      let value = ctor_expr () in
      ctor, [%expr Refl.NoParam { value = [%e value] }]
    | `tuple_like [] -> invalid_arg "tuple_like but 0 items defined"
    | `tuple_like [t] ->
      let get = [%expr function
        | [%p ctor_pat ~arg:[%pat? x] ()] -> [[%e nested_type_to_expr ~self_name t.va_type] x]
        | _ -> [%e invalid]]
      in
      let mk = [%expr function
        | [x] ->
          [%e nested_type_of_expr ~self_name t.va_type] x
          |> Option.map (fun x -> [%e ctor_expr ~arg:[%expr x] ()])
        | _ -> None]
      in
      ctor, [%expr Refl.TupleLike { get = [%e get]; mk = [%e mk] }]
    | `tuple_like ts ->
      let pvars = ts |> List.mapi (fun i _ -> pvari i) in
      let evars = ts |> List.mapi (fun i t -> t, evari i) in
      let get_body =
        evars |> List.map (fun (t, x) ->
          [%expr [%e nested_type_to_expr ~self_name t.va_type] [%e x]]
        )
      in
      let get = [%expr function
        | [%p ctor_pat ~arg:(Pat.tuple pvars) ()] -> [%e elist get_body]
        | _ -> [%e invalid]]
      in
      let mk_body =
        let body = ctor_expr ~arg:(evars |> List.map snd |> Exp.tuple) () in
        ts
        |> List.mapi (fun i t -> pvari i, [%expr [%e nested_type_of_expr ~self_name t.va_type] [%e evari i]])
        |> List.rev
        |> List.fold_left (fun expr (pv, result) ->
            [%expr [%e result] >>= fun [%p pv] -> [%e expr]]
          ) [%expr Some [%e body]]
      in
      let mk = [%expr function
        | [%p plist pvars] -> [%e mk_body]
        | _ -> None]
      in
      ctor, [%expr Refl.TupleLike { get = [%e get]; mk = [%e mk] }]
    | `inline_record fields ->
      of_record_fields ~label:"inline record" fields (fun get mk -> [%expr Refl.InlineRecord { get = [%e get]; mk = [%e mk] }])
    | `reused_inline_record decl ->
      let fields = decl.td_kind |> function
        | Record_decl fields -> fields
        | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." ctor.vc_name
      in
      of_record_fields ~label:"reused inline record" fields (fun get mk -> [%expr Refl.ReusedInlineRecord { get = [%e get]; mk = [%e mk] }])
  )

and gen_reflect_variant ~self_name ?(poly=false) (ctors: variant_constructor list) : expression =
  let loc = Location.none in
  let ctor_pat ctor ?arg () =
    if poly then
      Pat.variant ctor.vc_name arg
    else
      Pat.construct (lidloc ctor.vc_name) arg
  in
  let names =
    gen_reflect_variant_impl ~self_name ~poly ctors
    |> List.map (fun (ctor, refl_ctor) -> ctor, refl_ctor, sprintf "ctor_%s" ctor.vc_name)
  in
  let name_map =
    names
    |> List.map (fun (ctor, _, name) -> ctor.vc_name, evar name)
    |> List.to_seq
    |> StringMap.of_seq
  in
  let constructors =
    ctors |> List.map (fun ctor ->
      Exp.tuple [
        Exp.constant (Const.string ctor.vc_name);
        name_map |> StringMap.find ctor.vc_name;
      ]
    ) |> elist |> fun e -> [%expr StringMap.of_list [%e e]]
  in
  let classify =
    ctors |> List.map (fun ctor ->
      let has_args = ctor.vc_param <> `no_param in
      let pat =
        if has_args then
          ctor_pat ctor ~arg:(Pat.any ()) ()
        else
          ctor_pat ctor ()
      in
      let ref_ctor = name_map |> StringMap.find ctor.vc_name in
      Exp.case pat (Exp.tuple [ Exp.constant (Const.string ctor.vc_name); ref_ctor ])
    ) |> Exp.function_
  in
  names
  |> List.rev
  |> List.fold_left (fun body (_, refl_ctor, name) ->
    let pat = pvar name in
    [%expr let [%p pat] = [%e refl_ctor] in [%e body]]
  ) [%expr Refl.Variant { constructors = [%e constructors]; classify = [%e classify]}]

let mayappend cond x xs = if cond then xs@[x] else xs

let gen_structure :
  ?type_name:string
  -> ?refl:bool
  -> ?attrs:attribute list
  -> ?codec:Coretype.codec
  -> ?generators:(?codec:Coretype.codec -> type_decl -> structure) list
  -> ?type_decl:[`path of string | `expr of expression]
  -> type_decl -> structure =
  fun ?type_name ?(refl=true) ?attrs ?(codec=`default) ?(generators=[]) ?type_decl td ->
    let rec_flag = to_rec_flag td in
    let decl =
      Str.type_ rec_flag [type_declaration_of_type_decl ?type_name ?attrs td]
    in
    let reflect = gen_reflect ~codec td in
    let generators =
      generators |> List.concat_map (fun gen -> gen ?codec:(Some codec) td) in
    let type_decl =
      match type_decl with
      | None -> []
      | Some path_or_expr ->
        let decl_expr =
          match path_or_expr with
          | `path s -> Exp.ident (locmk (Longident.parse s))
          | `expr e -> e
        in
        let get_name suffix =
          match codec with
          | `default | `open_ _ -> td.td_name ^ "_" ^ suffix
          | `in_module _ -> suffix
        in
        let reflect =
          let rec go = function
            | Ppat_var l -> evar l.txt
            | Ppat_constraint (p, _) -> go p.ppat_desc
            | _ -> failwith' "impossible @%s: %a" __LOC__
                     Emitter.pattern reflect.pvb_pat
          in
          go reflect.pvb_pat.ppat_desc
        in
        let mk name expr = Str.value Nonrecursive [Vb.mk (pvar name) expr] in
        let loc = Location.none in
        [ mk (get_name "decl") decl_expr ]
        |> mayappend refl
             (mk (get_name "typed_decl")
                [%expr Bindoj_runtime.mk_generic_typed_type_decl [%e decl_expr] [%e reflect]])
    in
    ([decl]
     |> mayappend refl (Str.value rec_flag [reflect]))
    @ generators @ type_decl

let gen_reflect_signature :
  ?refl_type_abbr:string
  -> ?codec:Coretype.codec
  -> type_decl
  -> value_description =
  fun ?refl_type_abbr ?(codec=`default) td ->
    let loc = Location.none in
    let self_name = gen_reflect_name ~codec td in
    let typ = typcons ~loc td.td_name in
    let refl_typ = match refl_type_abbr with
      | None -> [%type: [%t typ] Bindoj_runtime.Refl.t]
      | Some abbr -> typcons ~loc abbr ~args:[typ]
    in
    Val.mk ~loc (strloc ~loc self_name) refl_typ

let gen_signature :
  ?type_name:string
  -> ?refl:bool
  -> ?attrs:attribute list
  -> ?codec:Coretype.codec
  -> ?generators:(?codec:Coretype.codec -> type_decl -> signature) list
  -> ?type_decl:bool
  -> ?refl_type_abbr:string
  -> ?type_decl_type_abbr:string
  -> ?typed_type_decl_type_abbr:string
  -> type_decl -> signature =
  fun ?type_name
      ?(refl=true)
      ?attrs ?(codec=`default)
      ?(generators=[])
      ?(type_decl = false)
      ?refl_type_abbr ?type_decl_type_abbr ?typed_type_decl_type_abbr
      td ->
    let loc = Location.none in
    let rec_flag = to_rec_flag td in
    let decl =
      Sig.type_ rec_flag [type_declaration_of_type_decl ?type_name ?attrs td]
    in
    let reflect = gen_reflect_signature ?refl_type_abbr ~codec td in
    let generators =
      generators |> List.concat_map (fun gen -> gen ?codec:(Some codec) td) in
    let type_decl =
      if type_decl then
        let decl_typ = match type_decl_type_abbr with
          | None -> [%type: Bindoj_typedesc.Type_desc.type_decl]
          | Some abbr -> typcons ~loc abbr
        in
    let mk_typed_type_decl_type typ =
      match typed_type_decl_type_abbr with
      | None -> [%type: ([%t decl_typ], [%t typ]) Bindoj_runtime.generic_typed_type_decl]
      | Some abbr -> typcons ~loc ~args:[typ] abbr in
        let get_name suffix =
          match codec with
          | `default | `open_ _ -> td.td_name ^ "_" ^ suffix
          | `in_module _ -> suffix
        in
        let mk (name : string) typ = Sig.value ~loc (Val.mk ~loc (strloc ~loc name) typ) in
        [ mk (get_name "decl") decl_typ ]
        |> mayappend refl
             (mk (get_name "typed_decl") (mk_typed_type_decl_type (typcons ~loc td.td_name))
                (* [%type: ([%t decl_typ], [%t typcons ~loc td.td_name]) Bindoj_runtime.generic_typed_type_decl] *))
      else []
    in
    ([decl]
     |> mayappend refl (Sig.value reflect))
    @ generators @ type_decl
