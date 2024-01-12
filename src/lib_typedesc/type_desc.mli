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
(** This module provides functionalities to describe the types used in Bindoj. *)
open Bindoj_runtime

type pos = [
  | `type_decl
  | `record_field
  | `variant_constructor
  | `variant_tuple_argument
  | `coretype
  | `string_enum_case
  | `method_bundle_brideable_decl
]

(** Configuration type where 'pos is restricted to pos types. *)
type ('pos, 'kind) config = .. constraint 'pos = [< pos]

module Configs : sig
  (** This module provides the functionality of configurations.
      Contains types and functions for handling configurations. *)

  type 'pos t =
    | [] : 'pos t
    | (::) : (('pos, 'a) config * 'pos t) -> 'pos t

  val empty : 'pos t
  (** An empty configuration. *)

  val find : (('pos, 'kind) config -> 'a option) -> 'pos t -> 'a option
  (** Returns the result of the appropriate conversion if found, else [None]. *)

  val merge : ([< pos] as 'pos) t -> 'pos t -> 'pos t

  val find_or_default :
    default:'a -> (('pos, 'kind) config -> 'a option) -> 'pos t -> 'a
  (** Returns the result of the appropriate conversion if found, else the default value. *)

  val get :
    ?default:('pos, 'kind) config ->
    (('pos, 'kind) config -> bool) -> 'pos t -> ('pos, 'kind) config
   (** Returns the first configuration satisfying the predicate if found, else the default configuration. *)

  type (_, _) config +=
      | Config_foreign_type_expression :
         ('tag, 'datatype_expr) foreign_language * 'datatype_expr ->
         ([`coretype], [`foreign_type_expression]) config
         (** Config for foreign type expression. *)

  val find_foreign_type_expr :
    ('tag, 'datatype_expr) foreign_language -> [`coretype] t
    -> 'datatype_expr option
  (** Returns the datatype_expr of the configuration involving the given foreign language if found, else [None]. *)
end

type 'pos configs = 'pos Configs.t constraint 'pos = [< pos]

val pp_configs : _ -> ppf -> _ configs -> unit (** dummy printer. always prints "<configs>". *)

val equal_configs : _ -> _ configs -> _ configs -> bool (** dummy equality function. always return [true]. *)
[@@alert equal_configs "Bindoj_type_desc.equal_configs always returns true"]

module Coretype : sig
  (** This module provides functionalities to handle core types. *)

  (** Represents primitive types. *)
  type prim = [
    | `unit (** [unit] in ocaml *)
    | `bool (** [bool] in ocaml *)
    | `int (** [int] in ocaml *)
    | `int53p (** [Kxclib.int53p] in ocaml *)
    | `float (** [float] in ocaml *)
    | `string (** holding a text string; [string] in ocaml *)
    | `uchar (** holding one unicode scalar value; [Uchar.t] in ocaml *)
    | `byte (** holding a byte; [char] in ocaml *)
    | `bytes (** holding an octet string; [Bytes.t] in ocaml *)
  ]

  (** Represents a map key *)
  type map_key = [
    | `string
(*  | prim *)
(*  | `Tuple of 'key list *)
(*  | `StringEnum of string list *)
(*  | `IntEnum of (string * int list ) *)
  ] (* TODO #125: extend map_key *)

  (** Represents a string enum case *)
  type string_enum_case = string * [`string_enum_case] configs * doc

  (** Describes the structure of a {!Coretype.t} *)
  type desc =
    | Prim of prim (** primitive types *)
    | Uninhabitable (** an uninhabitable type; [Kxclib.null] in ocaml *)
    | Ident of ident (** user-defined types *)
    | Option of desc (** [t option] in ocaml *)
    | Tuple of desc list (** invariant: list len >= 2; [t1*t2*..] in ocaml *)
    | List of desc (** [t list] in ocaml *)
    | Map of map_key * desc (** [(k*v) list] in ocaml *)
    | StringEnum of string_enum_case list (** 0-ary polyvariant in OCaml *)
  (*  | IntEnum of (string * int) list *) (* TODO #126: implement IntEnum *)
    | Self (** self type. used in a recursive definition *)

  (** Identifier type containing name and associated {!codec} *)
  and ident = {
    id_name: string;
    id_codec: codec;
  }

  (** Codec that describes how the {!type-ident} is represented *)
  and codec = [
    | `default
    | `in_module of string
    | `open_ of string
  (* | `codec_val of string *)
  ]

  val desc_of_map_key : map_key -> desc
  (** Creates a new {!type-desc} of the given key. *)

  val prim : prim -> desc
  (** Creates a new {!desc.Prim} of the given primitive type. *)

  val uninhabitable : desc
  (** Create a value of {!desc.Uninhabitable}. *)

  val ident : ?codec:codec -> string (** type name*) -> desc
  (** Creates a new {!desc.Ident} of the given type name. *)

  val option : desc -> desc
  (** Creates a new {!desc.Option} of the given descripter. *)

  val tuple : desc list -> desc
  (** Create a a value of {!desc.Tuple} of the given descripters. *)

  val list : desc -> desc
  (** Creates a new {!desc.List} of the given descripter. *)

  val map : map_key -> desc -> desc
  (** Creates a new {!desc.Map} of the given key and descripter. *)

  val string_enum : string_enum_case list -> desc
  (** Creates a new {!desc.StringEnum} of the given cases. *)

  val string_enum_case : ?configs:[`string_enum_case] configs -> ?doc:doc -> string (** case name *) -> string_enum_case
  (** Creates a new {!type-string_enum_case} for the name. *)

  val self : desc
  (** Creates a new {!desc.Self}. *)

  (** Represents coretype. *)
  type t = {
    ct_desc: desc;
    ct_configs: [`coretype] configs
  }

  val fold' : ('a -> desc -> [`break of 'a | `continue of 'a]) -> 'a (** initial state *) -> desc -> 'a
  (** Folds over the given {!type-desc} tree, allowing to break early or continue as normal. *)

  val fold : ('a -> desc -> 'a) -> 'a (** initial state *) -> desc -> 'a
  (** Folds over the given {!type-desc} tree. *)

  val map_ident: (ident -> ident) -> desc -> desc
  (** Apply the give function to each value of {!type-ident} in the given {!type-desc} tree. *)

  val mk : ?configs:[`coretype] configs -> desc -> t
  (** Creates a new coretype with the optional [\[`coretype\] configs]. *)

  val mk_prim : ?configs:[`coretype] configs -> prim -> t
  (** Creates a new coretype of the given primitive type. *)

  val mk_ident : ?configs:[`coretype] configs -> ?codec:codec -> string (** type name*) -> t
  (** Creates a new coretype of {!desc.Ident} for the given type name. *)

  val mk_option : ?configs:[`coretype] configs -> desc -> t
  (** Creates a new coretype of {!desc.Option} of the given descripter. *)

  val mk_list : ?configs:[`coretype] configs -> desc -> t
  (** Creates a new coretype of {!desc.List} of the given descripter. *)

  val mk_tuple : ?configs:[`coretype] configs -> desc list -> t
  (** Creates a new coretype of {!desc.Tuple} of the given descripters. *)

  val mk_map : ?configs:[`coretype] configs -> map_key -> desc -> t
  (** Creates a new coretype of {!desc.Map} of the given key and descripter. *)

  val mk_string_enum : ?configs:[`coretype] configs -> string_enum_case list -> t
  (** Creates a new coretype of {!desc.StringEnum} of the given cases. *)

  val mk_uninhabitable : ?configs:[`coretype] configs -> unit -> t
  (** Creates a new coretype of {!desc.Uninhabitable}. *)

  val mk_self : ?configs:[`coretype] configs -> unit -> t
  (** Creates a new coretype of {!desc.Self}. *)

  val string_of_prim : prim -> string
  (** Creates the name of the given primitive type e.g. [`int -> "int", `string -> "string", ... ]. *)

  val string_of_desc : desc -> string

  val to_string : t -> string

  val is_option : t -> bool
  (** Checks if {!type-desc} of the give coretype is {!desc.Option} or not. *)
end

type coretype = Coretype.t

val pp_coretype : ppf -> coretype -> unit
(** Pretty printer for the {!coretype}. *)

val string_of_coretype : coretype -> string

val equal_coretype : coretype -> coretype -> bool
(** Checks equality of two coretypes. *)

(** Represents a record field. *)
type record_field = {
  rf_name: string; (** Name of the record field *)
  rf_type: [ `direct of coretype | `nested of type_decl * Coretype.codec ]; (** Type of the record field *)
  rf_configs: [`record_field] configs; (** Configurations associated with the record field *)
  rf_doc: doc; (** Documentation of the record field *)
}

and variant_tuple_argument = {
  va_type: [ `direct of coretype | `nested of type_decl * Coretype.codec ];
  va_configs: [`variant_tuple_argument] configs;
  va_doc: doc;
}

(** Represents a variant constructor. *)
and variant_constructor = {
  vc_name: string; (** Name of the variant constructor *)
  vc_param: [
    | `no_param
    | `tuple_like of variant_tuple_argument list (** invariant: at least one *)
    | `inline_record of record_field list (** invariant: at least one *)
    | `reused_inline_record of type_decl (** must be Record_decl kinded *)
  ];
  vc_configs: [`variant_constructor] configs; (** Configurations associated with the variant constructor *)
  vc_doc: doc; (** Documentation of the variant constructor *)
}

(** Describe the kind of a type declaration. *)
and type_decl_kind =
  | Alias_decl of coretype  (** Type alias kind *)
  | Record_decl of record_field list  (** Record type kind *)
  | Variant_decl of variant_constructor list  (** Variant type kind *)

(** Represents a type declaration. *)
and type_decl = {
  td_name: string;  (** Name of the type declaration *)
  td_configs: [`type_decl] configs;  (** Configurations associated with the type declaration *)
  td_kind : type_decl_kind;  (** Kind of the type declaration *)
  td_doc: doc;  (** Documentation of the type declaration *)
}

val pp_record_field : ppf -> record_field -> unit
(** Pretty printer for the {!type-record_field}. *)

val string_of_record_field : record_field -> string

val equal_record_field : record_field -> record_field -> bool
(** Checks equality of two record_fields. *)

val record_field :
  ?doc:doc -> ?configs:[`record_field ] configs ->
  string -> coretype -> record_field
(** Creates a new {!type-record_field} backed by a {!type-coretype}. *)

val record_field_nested :
  ?doc:doc -> ?configs:[`record_field ] configs -> ?codec:Coretype.codec ->
  string -> type_decl -> record_field
(** Creates a new {!type-record_field} backed by another {!type-type_decl}. *)

val variant_argument :
  ?doc:doc -> ?configs:[`variant_tuple_argument ] configs ->
  coretype -> variant_tuple_argument
(** Creates a new argument type specification backed by a {!type-coretype} for a position
    in a variant constructor that takes tuple-like arguments. *)

val variant_argument_nested :
  ?doc:doc -> ?configs:[`variant_tuple_argument ] configs -> ?codec:Coretype.codec ->
  type_decl -> variant_tuple_argument
(** Creates a new argument type specification backed by a {!type-type_decl} for a position
    in a variant constructor that takes tuple-like arguments. *)

val pp_variant_constructor : ppf -> variant_constructor -> unit
(** Pretty printer for the {!type-variant_constructor}. *)

val string_of_variant_constructor : variant_constructor -> string

val equal_variant_constructor : variant_constructor -> variant_constructor -> bool
(** Checks equality of two variant_constructors. *)

val variant_constructor :
  ?doc:doc -> ?configs:[`variant_constructor] configs ->
  string -> [
    | `no_param
    | `tuple_like of variant_tuple_argument list (** invariant: at least one *)
    | `inline_record of record_field list (** invariant: at least one *)
    | `reused_inline_record of type_decl (** must be Variant_decl kinded *)
  ] -> variant_constructor
(** Creates a new {!type-variant_constructor} *)

val pp_type_decl : ppf -> type_decl -> unit
(** Pretty printer for the {!type_decl}. *)

val string_of_type_decl : type_decl -> string

val equal_type_decl : type_decl -> type_decl -> bool
(** Checks equality of two type declarations. *)

(** Represents a variant type. *)
type variant_type = [
  | `regular (** the default *)
  | `polymorphic
(*| `extensible (* future work *) *)
]

type ('pos, 'kind) config +=
  | Config_caml_variant_type :
    variant_type -> ([`type_decl], [`caml_variant_type]) config
    (** Configuration for the Caml variant type. *)

module Caml_config : sig
  (** This module provides functionalities to handle configs of OCaml. *)

  val variant_type : [`regular | `polymorphic] -> ([`type_decl], [`caml_variant_type]) config
  (** Creates a new variant type configuration. *)

  val get_variant_type : [`type_decl] configs -> [`regular | `polymorphic]
  (** Gets the variant type from the given config. *)
end

val alias_decl :
  ?doc:doc -> ?configs:[`type_decl] configs ->
  string -> coretype -> type_decl
  (** Creates a new {!type_decl} of {!type_decl_kind.Alias_decl}. *)

val record_decl :
  ?doc:doc -> ?configs:[`type_decl] configs ->
  string -> record_field list -> type_decl
  (** Creates a new {!type_decl} of {!type_decl_kind.Record_decl}. *)

val variant_decl :
  ?variant_type:variant_type -> ?doc:doc -> ?configs:[`type_decl] configs ->
  string -> variant_constructor list -> type_decl
(** Creates a new {!type_decl} of {!type_decl_kind.Variant_decl}. *)

type ancestral_configs = [
  | `alias of [`type_decl] configs
  (** config of alias decl. *)
  | `record_field of [`type_decl] configs * [`record_field] configs
  (** configs of field of record decl. *)
  | `variant_field of [`type_decl] configs * [`variant_constructor] configs * [`record_field] configs
  (** configs of field of variant decl. *)
  | `variant_reused_field of [`type_decl] configs * [`variant_constructor] configs * [`type_decl] configs * [`record_field] configs
  (** configs of reused field of variant decl. *)
  | `variant_argument of (** length of arguments *) int * [`type_decl] configs * [`variant_constructor] configs * [`variant_tuple_argument] configs
  (** length of arguments and configs of argument of variant decl. *)
  ] list
  (** List of kind and configs of type_decl, the parent of the coretype.
      The format of the list is path, with head being the closest. *)

val fold_coretypes' : ('a -> coretype * ancestral_configs -> 'a) -> 'a -> type_decl -> 'a
(** Folds a function over all {!coretype} in the given {!type_decl}. *)

val fold_coretypes : ('a -> coretype -> 'a) -> 'a -> type_decl -> 'a
(** Folds a function over all {!coretype} in the given {!type_decl}. *)

val is_recursive : type_decl -> bool
(** Returns if the given {!type_decl} is recursive or not. *)
