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
open Ast_builder.Default

let locmk ?loc txt = { txt; loc = loc |? !Ast_helper.default_loc }
let strloc ?loc x : label with_loc = locmk ?loc x
let lidloc ?loc x = locmk ?loc (Longident.parse x)

let typcons ?loc ?attrs ?(args=[]) x = Typ.constr ?loc ?attrs (lidloc ?loc x) args
let typpack ?loc ?attrs ?(args=[]) x = Typ.package ?loc ?attrs (lidloc ?loc x) (args |&> (?< (lidloc ?loc)))
let pvar ?loc ?attrs s = Pat.var ?loc ?attrs (strloc s)
let evar ?open_ ?loc ?attrs s =
  Exp.ident ?loc ?attrs (lidloc s)
  |> match open_ with
     | None -> identity
     | Some opening ->
        ({ popen_expr  = Mod.ident ?loc (lidloc ?loc opening);
           popen_override = Fresh;
           popen_loc = loc |? !Ast_helper.default_loc;
           popen_attributes = [];
         } : open_declaration
        ) |> Exp.open_ ?loc

let elist ?(loc=Location.none) =
  let rec go acc = function
    | [] -> acc
    | x :: xs -> go [%expr [%e x] :: [%e acc]] xs
  in
  fun xs -> List.rev xs |> go [%expr []]
let plist ?(loc=Location.none) =
  let rec go acc = function
    | [] -> acc
    | x :: xs -> go [%pat? [%p x] :: [%p acc]] xs
  in
  fun xs -> List.rev xs |> go [%pat? []]

let efun ?(loc=Location.none) =
  let rec go acc = function
  | [] -> acc
  | (a, b, c) :: xs -> go (Exp.fun_ ~loc a b c acc) xs
  in
  fun args body -> go body (List.rev args)

  let rec ejv ?(loc=Location.none) (jv: Json.jv) =
    match jv with
    | `null -> [%expr `null]
    | `bool x -> [%expr `bool [%e ebool ~loc x]]
    | `num x -> [%expr `num [%e efloat ~loc (Float.to_string x)]]
    | `str x -> [%expr `str [%e estring ~loc x ]]
    | `arr xs -> [%expr `arr [%e xs |&> (ejv ~loc) |> elist ~loc ]]
    | `obj xs -> [%expr
      `obj [%e xs
        |&> (fun (s, jv) ->
          [%expr ([%e estring ~loc s], [%e ejv ~loc jv])])
        |> elist ~loc ]]

let attr name value =
  Attr.mk (locmk name) (PStr [Str.eval value])
let doc_attribute = function
  | `docstr doc -> [attr "ocaml.doc" (Exp.constant (Const.string doc))]
  | `nodoc -> []
  | _ -> failwith "unknown polymorphic variant for docstr"
let show_attribute = [attr "deriving" (Exp.ident (lidloc "show"))]
let warning_attribute str = [attr "warning" (Exp.constant (Const.string str))]

let sprintf fmt = Format.asprintf fmt

let escape_as_constructor_name (s: string) =
  (* TODO #128: proper escaping *)
  s

open Bindoj_typedesc.Typed_type_desc

let to_rec_flag { td_kind; _ } =
  match td_kind with
  | Alias_decl _ -> Nonrecursive
  | Record_decl _ | Variant_decl _ -> Recursive

let type_name_with_codec : ?codec:Coretype.codec -> string -> string =
  fun ?(codec=`default) name ->
  match codec with
  | `default -> name
  | `open_ m -> sprintf "%s.%s" m name
  | `in_module m -> sprintf "%s.t" m

let type_of_coretype : ?attrs:_ -> ?self_name:string -> coretype -> core_type =
  fun ?attrs ?self_name { ct_desc; _ } ->
  let open Coretype in
  let type_of_prim = function
    | `unit -> typcons "unit"
    | `bool -> typcons "bool"
    | `int -> typcons "int"
    | `int53p -> typcons "Kxclib.int53p"
    | `float -> typcons "float"
    | `string -> typcons "string"
    | `uchar -> typcons "Uchar.t"
    | `byte -> typcons "char"
    | `bytes -> typcons "Bytes.t"
  in
  let rec go = function
    | Prim p -> type_of_prim p
    | Uninhabitable -> typcons "unit"
    | Ident { id_name; id_codec; } ->
      typcons (type_name_with_codec ~codec:id_codec id_name)
    | Option t -> typcons "option" ~args:[go t]
    | List t -> typcons "list" ~args:[go t]
    | Map (k, v) ->
      let k = desc_of_map_key k in
      typcons "list" ~args:[Typ.tuple [go k; go v]]
    | Tuple ts -> ts |> List.map go |> Typ.tuple
    | StringEnum cs ->
      let cases = cs |&> (fun (k, _, doc) ->
        Rf.tag
          ~attrs:(doc_attribute doc)
          (locmk (escape_as_constructor_name k)) true [])
      in
      Typ.variant cases Closed None
    | Self ->
      begin match self_name with
      | Some self_name -> typcons self_name
      | None -> failwith "Cannot construct a recursive type because the argument 'self_name' is not specified."
      end
    in
    let typ = go ct_desc in
    match attrs with
    | None -> typ
    | Some attrs -> { typ with ptyp_attributes = attrs }

let type_of_nested_type ?attrs ?self_name = function
  | `direct ct -> type_of_coretype ?attrs ?self_name ct
  | `nested ({ td_name; _ }, codec) ->
    typcons ?attrs (type_name_with_codec ~codec td_name)

let type_of_polymorphic_variant =
  fun ?attrs ?self_name ctors ->
    let fields =
      ctors |&> fun ctor ->
        match ctor.vc_param with
        | `no_param ->
          Rf.tag ~attrs:(doc_attribute ctor.vc_doc)
            (locmk ctor.vc_name)
            false
            []
        | `tuple_like ts ->
          Rf.tag ~attrs:(doc_attribute ctor.vc_doc)
            (locmk ctor.vc_name)
            true
            (match ts with
            | [] -> failwith "impossible"
            | [arg] -> [type_of_nested_type ?self_name arg.va_type]
            | args -> [Typ.tuple (args |&> fun { va_type; _ } -> type_of_nested_type ?self_name va_type)])
        | `inline_record _ ->
          failwith' "case '%s' with an inline record cannot be used in a polymorphic variant" ctor.vc_name
        | `reused_inline_record _ ->
          failwith' "case '%s' with an reused inline record cannot be used in a polymorphic variant" ctor.vc_name
    in
    Typ.variant ?attrs fields Closed None

open Bindoj_runtime

let rec e_runtime_expr ~loc =
  let rec go = function
  | Expr.Unit -> [%expr ()]
  | Bool x -> ebool ~loc x
  | Int x -> eint ~loc x
  | Int53p x ->
    begin match Int53p.impl_flavor with
      | `int_impl -> [%expr Int53p.of_int [%e eint ~loc @@Int53p.to_int x]]
      | `int64_impl -> [%expr Int53p.of_int64 [%e eint64 ~loc & Int53p.to_int64 x]]
      | `float_impl -> [%expr Int53p.of_float [%e efloat ~loc & Float.to_string & Int53p.to_float x]]
      | `custom_impl _ -> failwith "custom int53p implementation is not supported"
    end
  | Float x -> efloat ~loc (Float.to_string x)
  | String x -> estring ~loc x
  | Uchar x -> [%expr Uchar.of_int [%e eint ~loc (Uchar.to_int x)]]
  | Byte x -> echar ~loc x
  | Bytes x -> [%expr Bytes.of_string [%e estring ~loc & Bytes.to_string x]]
  | Some x -> [%expr Some ([%e go x])]
  | None -> [%expr None]
  | Tuple xs -> Exp.tuple (xs |&> go)
  | List xs -> elist ~loc (xs |&> go)
  | Map xs ->
    [%expr Bindoj_runtime.StringMap.of_list [%e
      xs
      |&> (fun (label, x) ->
        Exp.tuple [ estring ~loc label; go x])
      |> elist ~loc
    ]]
  | StringEnum s -> Exp.variant s None
  | Refl (lazy refl, x) ->
    e_runtime_refl ~loc
      (Obj.magic refl : 'a Refl.result) (Obj.magic x : 'a)
  in
  go
and e_runtime_refl =
  let erecord ~loc efields =
    efields
    |> StringMap.to_list
    |&> (fun (label, expr) -> lidloc label, e_runtime_expr ~loc expr)
    |> Fn.flip Exp.record None
  in
  fun ~loc (refl: 'a Refl.result) (x: 'a) ->
  match refl with
  | Refl.Alias { get; _ } -> e_runtime_expr ~loc (get x)
  | Record { get; _ } ->
    erecord ~loc (get x)
  | Variant { classify; _ } ->
    let label, kind = classify x in
    Exp.construct (lidloc label) 
      (match kind with
      | Refl.NoParam _ -> None
      | TupleLike { get; _ } ->
        Some (Exp.tuple ~loc (get x |&> e_runtime_expr ~loc))
      | InlineRecord { get; _ }
      | ReusedInlineRecord { get; _ } ->
        Some (erecord ~loc (get x)))

let encoded_map_key_expression =
  let rec emap_key_type_desc ~loc : Map_key.map_key_type_desc -> expression = function
    | `string -> [%expr `string]
    | `int53p -> [%expr `int53p]
    | `Tuple ds -> [%expr `Tuple [%e ds |&> emap_key_type_desc ~loc |> elist]]
    | `StringEnum cases -> [%expr `StringEnum [%e cases |&> (estring ~loc) |> elist]]
    | `Dictionary d -> [%expr `Dictionary [%e emap_key_type_desc ~loc d]]
  in
  fun ?(loc=Location.none) desc body ->
  [%expr
  encode_map_key
    ~check_type:[%e emap_key_type_desc ~loc desc]
    [%e
      match desc with
      | `string -> [%expr Mk_string [%e body]]
      | `int53p -> [%expr Mk_int53 [%e body]]
      | `Tuple _ -> [%expr Mk_tuple [%e body]]
      | `StringEnum cases ->
        [%expr Mk_string_enum [%e
          cases
          |&> (fun label ->
              Exp.case (Pat.variant label None) (estring ~loc label)
            )
          |> Exp.match_ body
        ]]
      | `Dictionary _ -> [%expr Mk_dictionary [%e body]]
    ]
]