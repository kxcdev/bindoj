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
open Bindoj_runtime

type pos = [
  | `type_decl
  | `record_field
  | `variant_constructor
  | `coretype
]

type ('pos, 'kind) config = .. constraint 'pos = [< pos]

module Configs : sig
  type 'pos t =
    | [] : 'pos t
    | (::) : (('pos, 'a) config * 'pos t) -> 'pos t

  val empty : 'pos t

  val find : (('pos, 'kind) config -> 'a option) -> 'pos t -> 'a option

  val find_or_default :
    default:'a -> (('pos, 'kind) config -> 'a option) -> 'pos t -> 'a

  val get :
    ?default:('pos, 'kind) config ->
    (('pos, 'kind) config -> bool) -> 'pos t -> ('pos, 'kind) config

  type (_, _) config +=
     | Config_foreign_type_expression :
         ('tag, 'datatype_expr) foreign_language * 'datatype_expr ->
         ([`coretype], [`foreign_type_expression]) config

  val find_foreign_type_expr :
    ('tag, 'datatype_expr) foreign_language -> [`coretype] t
    -> 'datatype_expr option
end
type 'pos configs = 'pos Configs.t constraint 'pos = [< pos]

val pp_configs : _ -> ppf -> _ configs -> unit (** dummy printer. always prints "<configs>" *)

val equal_configs : _ -> _ configs -> _ configs -> bool (** dummy equality function. always return true *)
[@@alert equal_configs "Bindoj_type_desc.equal_configs always returns true"]

module Coretype : sig
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
  ]

 type map_key = [
    | `string
(*  | prim *)
(*  | `Tuple of 'key list *)
(*  | `StringEnum of string list *)
(*  | `IntEnum of (string * int list ) *)
  ] (* TODO #125: extend map_key *)

  type desc =
    | Prim of prim (** primitive types *)
    | Uninhabitable (** an uninhabitable type; [Kxclib.null] in ocaml *)
    | Ident of ident (** user-defined types *)
    | Option of desc (** [t option] in ocaml *)
    | Tuple of desc list (** invariant: list len >= 2; [t1*t2*..] in ocaml *)
    | List of desc (** [t list] in ocaml *)
    | Map of map_key * desc (** [(k*v) list] in ocaml *)
    | StringEnum of string list (** 0-ary polyvariant in OCaml *)
(*  | IntEnum of (string * int) list *) (* TODO #126: implement IntEnum *)
    | Self (** self type. used in a recursive definition *)

  and ident = {
    id_name: string;
    id_codec: codec;
  }

  and codec = [
    | `default
    | `in_module of string
 (* | `codec_val of string *)
  ]

  val desc_of_map_key : map_key -> desc

  val prim : prim -> desc
  val uninhabitable : desc
  val ident : ?codec:codec -> string -> desc
  val option : desc -> desc
  val tuple : desc list -> desc
  val list : desc -> desc
  val map : map_key -> desc -> desc
  val string_enum : string list -> desc
  val self : desc

  type t = {
    ct_desc: desc;
    ct_configs: [`coretype] configs
  }

  val fold' : ('a -> desc -> [`break of 'a | `continue of 'a]) -> 'a -> desc -> 'a
  val fold : ('a -> desc -> 'a) -> 'a -> desc -> 'a
  val map_ident: (ident -> ident) -> desc -> desc

  val mk : ?configs:[`coretype] configs -> desc -> t
  val mk_prim : ?configs:[`coretype] configs -> prim -> t
  val mk_ident : ?configs:[`coretype] configs -> ?codec:codec -> string -> t
  val mk_option : ?configs:[`coretype] configs -> desc -> t
  val mk_list : ?configs:[`coretype] configs -> desc -> t
  val mk_tuple : ?configs:[`coretype] configs -> desc list -> t
  val mk_map : ?configs:[`coretype] configs -> map_key -> desc -> t
  val mk_string_enum : ?configs:[`coretype] configs -> string list -> t
  val mk_uninhabitable : ?configs:[`coretype] configs -> unit -> t
  val mk_self : ?configs:[`coretype] configs -> unit -> t

  (** returns the name of the constructor e.g. [`int -> "int", `string -> "string", ... ]*)
  val string_of_prim : prim -> string
  val string_of_desc : desc -> string
  val to_string : t -> string
  val is_option : t -> bool
end
type coretype = Coretype.t
val pp_coretype : ppf -> coretype -> unit
val string_of_coretype : coretype -> string
val equal_coretype : coretype -> coretype -> bool

type doc = [
  | `docstr of string
  | `nodoc
]
val pp_doc : ppf -> doc -> unit
val string_of_doc : doc -> string
val equal_doc : doc -> doc -> bool

type record_field = {
  rf_name: string;
  rf_type: coretype;
  rf_configs: [`record_field] configs;
  rf_doc: doc;
}
val pp_record_field : ppf -> record_field -> unit
val string_of_record_field : record_field -> string
val equal_record_field : record_field -> record_field -> bool

val record_field :
  ?doc:doc -> ?configs:[`record_field ] configs ->
  string -> coretype -> record_field

type variant_constructor = {
  vc_name: string;
  vc_param: [
    | `no_param
    | `tuple_like of coretype list (** invariant: at least one *)
    | `inline_record of record_field list (** invariant: at least one *)
  ];
  vc_configs: [`variant_constructor] configs;
  vc_doc: doc;
}
val pp_variant_constructor : ppf -> variant_constructor -> unit
val string_of_variant_constructor : variant_constructor -> string
val equal_variant_constructor : variant_constructor -> variant_constructor -> bool

val variant_constructor :
  ?doc:doc -> ?configs:[`variant_constructor] configs ->
  string -> [
    | `no_param
    | `tuple_like of coretype list (** invariant: at least one *)
    | `inline_record of record_field list (** invariant: at least one *)
  ] -> variant_constructor

type type_decl_kind =
  | Alias_decl of coretype
  | Record_decl of record_field list
  | Variant_decl of variant_constructor list

type type_decl = {
  td_name: string;
  td_configs: [`type_decl] configs;
  td_kind : type_decl_kind;
  td_doc: doc;
}
val pp_type_decl : ppf -> type_decl -> unit
val string_of_type_decl : type_decl -> string
val equal_type_decl : type_decl -> type_decl -> bool

type variant_type = [
  | `regular (** the default *)
  | `polymorphic
(*| `extensible (* future work *) *)
]

type ('pos, 'kind) config +=
  | Config_caml_variant_type : variant_type -> ([`type_decl], [`caml_variant_type]) config

module Caml_config : sig
  val variant_type : [`regular | `polymorphic] -> ([`type_decl], [`caml_variant_type]) config
  val get_variant_type : [`type_decl] configs -> [`regular | `polymorphic]
end

val alias_decl :
  ?doc:doc -> ?configs:[`type_decl] configs ->
  string -> coretype -> type_decl

val record_decl :
  ?doc:doc -> ?configs:[`type_decl] configs ->
  string -> record_field list -> type_decl

val variant_decl :
  ?variant_type:variant_type -> ?doc:doc -> ?configs:[`type_decl] configs ->
  string -> variant_constructor list -> type_decl

val fold_coretypes : ('a -> coretype -> 'a) -> 'a -> type_decl -> 'a

val is_recursive : type_decl -> bool
