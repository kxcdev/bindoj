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
open Bindoj_runtime

type pos = [
  | `type_decl
  | `record_field
  | `variant_constructor
  | `variant_tuple_argument
  | `coretype
  | `string_enum_case
]

type ('pos, 'kind) config = .. constraint 'pos = [< pos]

module Configs = struct
  type 'pos t =
    | [] : 'pos t
    | (::) : (('pos, _) config * 'pos t) -> 'pos t

  type ('pos, 'kind) config +=
    | Config_dummy
    | Config_foreign_type_expression :
        ('tag, 'datatype_expr) foreign_language * 'datatype_expr ->
        ([`coretype], [`foreign_type_expression]) config

  let empty : 'pos t = []

  let find : (('pos, 'kind) config -> 'a option) -> 'pos t -> 'a option =
    fun finder configs ->
    let rec go : 'pos t -> 'a option = function
      | [] -> None
      | kind :: rest ->
        match finder (Obj.magic kind) with
        | None -> go rest
        | Some v -> Some v
    in
    go configs

  let find_or_default : default:'a -> (('pos, 'kind) config -> 'a option) -> 'pos t -> 'a =
    fun ~default finder configs -> find finder configs |> Option.value ~default

  let get : ?default:('pos, 'kind) config -> (('pos, 'kind) config -> bool) -> 'pos t -> ('pos, 'kind) config =
    fun ?(default = Config_dummy) pred configs ->
    let rec go : 'pos t -> _ = function
      | [] -> default
      | kind :: rest ->
        let kind = Obj.magic kind in
        if pred kind then kind else go rest
    in
    go configs

  let find_foreign_type_expr
    (lang: ('tag, 'datatype_expr) foreign_language)
    (configs: [`coretype] t) : 'datatype_expr option =
    find (function
      | Config_foreign_type_expression (lang', expr') when lang' == (Obj.magic lang) ->
        Some (Obj.magic expr' : 'datatype_expr)
      | _ -> None
    ) configs
end
type 'pos configs = 'pos Configs.t constraint 'pos = [< pos]

let pp_configs _ ppf _ = Format.pp_print_string ppf "<configs>"
let equal_configs _ _ _ = true

type 't ignore_order_list = 't list [@@deriving show]
let equal_ignore_order_list equal_t xs ys =
  List.equal equal_t (List.sort compare xs) (List.sort compare ys)

module Coretype = struct
  type prim = [
    | `unit (** [unit] in ocaml *)
    | `bool (** [bool] in ocaml *)
    | `int (** [int] in ocaml *)
    | `int53p (** [Kxclib.int53p] in ocaml *)
    | `float (** [float] in ocaml *)
    | `string (** holding a text string; [string] in ocaml *)
    | `uchar (** holding one unicode scalar value; [Uchar.t] in ocaml *)
    | `byte (** holding a byte; [char] in ocaml *)
    | `bytes (** holding a octet string; [Bytes.t] in ocaml *)
  ] [@@deriving show,eq]

  type map_key = [
    | `string
(*  | prim *)
(*  | `Tuple of 'key list *)
(*  | `StringEnum of string list *)
(*  | `IntEnum of (string * int list ) *)
  ] [@@deriving show,eq] (* TODO #125: extend map_key *)

  type string_enum_case = string * [`string_enum_case] configs * doc [@@deriving show,eq]

  type desc =
    | Prim of prim (** primitive types *)
    | Uninhabitable (** an uninhabitable type; [Kxclib.null] in ocaml *)
    | Ident of ident (** user-defined types *)
    | Option of desc (** [t option] in ocaml *)
    | Tuple of desc list (** invariant: list len >= 2; [t1*t2*..] in ocaml *)
    | List of desc (** [t list] in ocaml *)
    | Map of map_key * desc (** [(k*v) list] in ocaml *)
    | StringEnum of string_enum_case ignore_order_list (** 0-ary polyvariant in OCaml *)
(*  | IntEnum of (string * int) ignore_order_list *) (* TODO #126: implement IntEnum *)
    | Self (** self type. used in a recursive definition *)
    [@@deriving show,eq]

  and ident = {
    id_name: string;
    id_codec: codec;
  } [@@deriving show,eq]

  and codec = [
    | `default
    | `in_module of string
    | `open_ of string
 (* | `codec_val of string *)
  ] [@@deriving show,eq]

  let desc_of_map_key : map_key -> desc = function
    | `string -> Prim `string

  let prim p = Prim p
  let ident ?(codec = `default) name = Ident { id_name = name; id_codec = codec }
  let option t = Option t
  let tuple ts =
    if List.length ts < 2 then
      invalid_arg "length(ts) < 2"
    else
      Tuple ts
  let list t = List t
  let map k v = Map (k, v)
  let string_enum cases = StringEnum cases
  let string_enum_case ?(configs = Configs.empty) ?(doc = `nodoc) name = name,configs,doc
  let uninhabitable = Uninhabitable
  let self = Self

  type t = {
    ct_desc: desc;
    ct_configs: [`coretype] configs
  } [@@deriving show,eq]

  let rec fold' (f: 'a -> desc -> [`break of 'a | `continue of 'a]) (state: 'a) desc =
    let result = f state desc in
    match result with
    | `break state -> state
    | `continue state ->
      begin match desc with
      | Option desc -> fold' f state desc
      | List desc -> fold' f state desc
      | Map (_, v_desc) -> fold' f state v_desc
      | Tuple descs -> List.fold_left (fold' f) state descs
      | Prim _ | Uninhabitable | Ident _ | StringEnum _ | Self -> state
      end

  let fold f state desc = fold' (fun state x -> `continue (f state x)) state desc

  let rec map_ident f = function
    | Ident s -> Ident (f s)
    | Option desc -> Option (map_ident f desc)
    | List desc -> List (map_ident f desc)
    | Map (k_desc, v_desc) -> Map (k_desc, map_ident f v_desc)
    | Tuple descs -> Tuple (List.map (map_ident f) descs)
    | (Prim _ | Uninhabitable | Self | StringEnum _) as x -> x

  let mk ?(configs=Configs.([])) desc = { ct_desc = desc; ct_configs=configs }
  let mk_impl ?configs ~f = f (fun ct_desc -> mk ?configs ct_desc)

  let mk_prim = mk_impl ~f:(fun mk p -> mk (prim p))
  let mk_ident = mk_impl ~f:(fun mk ?codec name -> mk (ident ?codec name))
  let mk_option = mk_impl ~f:(fun mk t -> mk (option t))
  let mk_tuple = mk_impl ~f:(fun mk ts -> mk (tuple ts))
  let mk_list = mk_impl ~f:(fun mk t -> mk (list t))
  let mk_map = mk_impl ~f:(fun mk k v -> mk (map k v))
  let mk_string_enum = mk_impl ~f:(fun mk cs -> mk (string_enum cs))
  let mk_uninhabitable = mk_impl ~f:(fun mk () -> mk uninhabitable)
  let mk_self = mk_impl ~f:(fun mk () -> mk self)

  let is_option { ct_desc; _ } =
    match ct_desc with Option _ -> true | _ -> false

  let string_of_prim : prim -> string = function
    | `unit -> "unit"
    | `bool -> "bool"
    | `int -> "int"
    | `int53p -> "int53p"
    | `float -> "float"
    | `string -> "string"
    | `uchar -> "uchar"
    | `byte -> "byte"
    | `bytes -> "bytes"
  let string_of_desc : desc -> string = show_desc
  let to_string : t -> string = show
end

type coretype = Coretype.t [@@deriving show,eq]
let string_of_coretype = show_coretype

type record_field = {
  rf_name: string;
  rf_type: [ `direct of coretype | `nested of type_decl * Coretype.codec ];
  rf_configs: [`record_field] configs;
  rf_doc: doc;
} [@@deriving show,eq]

and variant_tuple_argument = {
  va_type: [ `direct of coretype | `nested of type_decl * Coretype.codec ];
  va_configs: [`variant_tuple_argument] configs;
  va_doc: doc;
}
and variant_constructor = {
  vc_name: string;
  vc_param: [
    | `no_param
    | `tuple_like of variant_tuple_argument list
    | `inline_record of record_field ignore_order_list
    | `reused_inline_record of type_decl
  ];
  vc_configs: [`variant_constructor] configs;
  vc_doc: doc;
} [@@deriving show,eq]

and type_decl_kind =
  | Alias_decl of coretype
  | Record_decl of record_field ignore_order_list
  | Variant_decl of variant_constructor ignore_order_list
  [@@deriving show,eq]

and type_decl = {
  td_name: string;
  td_configs: [`type_decl] configs;
  td_kind : type_decl_kind;
  td_doc: doc;
} [@@deriving show,eq]

let string_of_record_field = show_record_field

let record_field ?(doc=`nodoc) ?(configs=Configs.empty) rf_name rf_type =
  { rf_name; rf_type = `direct rf_type; rf_configs = configs; rf_doc = doc }

let record_field_nested ?(doc=`nodoc) ?(configs=Configs.empty) ?(codec=`default) rf_name rf_type =
  { rf_name; rf_type = `nested (rf_type, codec); rf_configs = configs; rf_doc = doc }

let variant_argument ?(doc=`nodoc) ?(configs=Configs.empty) ct =
  { va_type = `direct ct; va_configs = configs; va_doc = doc }

let variant_argument_nested ?(doc=`nodoc) ?(configs=Configs.empty) ?(codec=`default) ct =
  { va_type = `nested (ct, codec); va_configs = configs; va_doc = doc }

let string_of_variant_constructor = show_variant_constructor

let variant_constructor ?(doc=`nodoc) ?(configs=Configs.empty) vc_name vc_param =
  match vc_param with
  | `tuple_like [] -> invalid_arg "`tuple_like but no type is given"
  | `inline_record [] -> invalid_arg "`inline_record but no field is given"
  | `reused_inline_record { td_kind = Alias_decl _ | Variant_decl _; _ } ->
    invalid_arg "`reused_inline_record but the given type decl is not record"
  | _ -> { vc_name; vc_param; vc_configs = configs; vc_doc = doc }

let string_of_type_decl = show_type_decl

type variant_type = [
  | `regular (** the default *)
  | `polymorphic
(*| `extensible (* future work *) *)
]

type ('pos, 'kind) config +=
  | Config_caml_variant_type : variant_type -> ([`type_decl], [`caml_variant_type]) config

module Caml_config = struct
  let variant_type f = Config_caml_variant_type f

  let get_variant_type configs =
    Configs.find_or_default ~default:`regular (function | Config_caml_variant_type f -> Some f | _ -> None) configs
end

let alias_decl ?(doc=`nodoc) ?(configs=Configs.empty) td_name ct =
  {
    td_name;
    td_kind = Alias_decl ct;
    td_doc = doc;
    td_configs = configs;
  }

let record_decl ?(doc=`nodoc) ?(configs=Configs.empty) td_name fields =
  match fields with
  | [] -> invalid_arg "no field is given"
  | _ ->
    {
      td_name;
      td_kind = Record_decl fields;
      td_doc = doc;
      td_configs = configs;
    }

let variant_decl ?variant_type ?(doc=`nodoc) ?(configs=Configs.empty) td_name ctors =
  let configs =
    match variant_type with
    | None -> configs
    | Some ty -> Caml_config.variant_type ty :: configs
  in
  match ctors with
  | [] -> invalid_arg "no constructor is given"
  | _ ->
    {
      td_name;
      td_kind = Variant_decl ctors;
      td_doc = doc;
      td_configs = configs;
    }

let rec fold_coretypes folder state td =
  let fold_record_fields =
    List.fold_left (fun state field ->
      match field.rf_type with
      | `direct ct -> folder state ct
      | `nested (td, _) -> fold_coretypes folder state td
    )
  in
  match td.td_kind with
  | Alias_decl ct -> folder state ct
  | Record_decl fields ->
    fold_record_fields state fields
  | Variant_decl ctors ->
    ctors |> List.fold_left (fun state ctor ->
      match ctor.vc_param with
      | `no_param -> state
      | `tuple_like ts ->
        ts |> List.fold_left (fun state va ->
          match va.va_type with
          | `direct ct -> folder state ct
          | `nested (td, _) -> fold_coretypes folder state td
        ) state
      | `inline_record fields ->
        fold_record_fields state fields
      | `reused_inline_record decl ->
        let fields = decl.td_kind |> function
          | Record_decl fields -> fields
          | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." ctor.vc_name
        in
        fold_record_fields state fields
    ) state

let is_recursive =
  let check =
    let open Coretype in
    fold' (fun state -> function
      | Self -> `break true
      | _ -> `continue state
    ) false
  in
  fold_coretypes (fun state t -> state || check t.ct_desc) false
