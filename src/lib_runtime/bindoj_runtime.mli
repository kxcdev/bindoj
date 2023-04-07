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
include module type of Utils

type 'camlrepr external_format = ..
type ('tag, 'datatype_expr) foreign_language = ..

type external_format_label = External_format : _ external_format -> external_format_label
type foreign_language_label = Foreign_language : _ foreign_language -> foreign_language_label

module External_format : sig
  module LabelOrdered : sig
    type t = external_format_label
    val compare : t -> t -> int
  end
  module LabelSet : Set.S with type elt = LabelOrdered.t
  module LabelMap : Map.S with type key = LabelOrdered.t
  type label_set = LabelSet.t
  type 'x label_map = 'x LabelMap.t

  type ('t, 'ext) codec = {
      encode : 't -> 'ext;
      decode : 'ext -> 't option;
    }
  type ('t) codec' =
    Codec : 'ext external_format*('t, 'ext) codec -> 't codec'
  type 't codecs = 't codec' LabelMap.t
end


module rec Refl : sig
  type 't constructor =
    | InlineRecord of {
        get: 't -> Expr.t StringMap.t;
        mk: Expr.t StringMap.t -> 't option;
      }
    | TupleLike of {
        get: 't -> Expr.t list;
        mk: Expr.t list -> 't option;
      }
    | NoParam of {
        value: 't
      }

  type 't result =
    | Record of {
        get: 't -> Expr.t StringMap.t;
        mk: Expr.t StringMap.t -> 't option;
      }
    | Variant of {
        constructors: 't constructor StringMap.t;
        classify: 't -> (string * 't constructor);
      }
    | Alias of {
        get: 't -> Expr.t;
        mk: Expr.t -> 't option;
      }

  type 'a t = 'a result Lazy.t
end

and Expr : sig
  type t =
    | Unit
    | Bool of bool
    | Int of int
    | Int53p of int53p
    | Float of float
    | String of string
    | Uchar of Uchar.t
    | Byte of char
    | Bytes of Bytes.t
    | Some of t
    | None
    | Tuple of t list
    | List of t list
    | Map of (string * t) list
    | StringEnum of string
    | Refl : 'a Refl.t * 'a -> t

  val of_unit : unit -> t
  val to_unit : t -> unit option
  val of_bool : bool -> t
  val to_bool : t -> bool option
  val of_int : int -> t
  val to_int : t -> int option
  val of_int53p : int53p -> t
  val to_int53p : t -> int53p option
  val of_float : float -> t
  val to_float : t -> float option
  val of_string : string -> t
  val to_string : t -> string option
  val of_uchar : Uchar.t -> t
  val to_uchar : t -> Uchar.t option
  val of_byte : char -> t
  val to_byte : t -> char option
  val of_bytes : Bytes.t -> t
  val to_bytes : t -> Bytes.t option
  val of_option : ('a -> t) -> 'a option -> t
  val to_option : (t -> 'a option) -> t -> 'a option option
  val of_list : ('a -> t) -> 'a list -> t
  val to_list : (t -> 'a option) -> t -> 'a list option
  val of_map : ('a -> t) -> (string * 'a) list -> t
  val to_map : (t -> 'a option) -> t -> (string * 'a) list option
  val of_refl : 'a Refl.t -> 'a -> t
  val to_refl : 'a Refl.t -> t -> 'a option
end

module type Generic_typed_type_decl = sig
  type type_decl
  type t
  val decl : type_decl
  val reflect : t Refl.t
end
type ('type_decl, 't) generic_typed_type_decl =
  (module Generic_typed_type_decl with
            type type_decl = 'type_decl
          and type t = 't)

val mk_generic_typed_type_decl :
  'type_decl -> 't Refl.t
  -> ('type_decl, 't) generic_typed_type_decl

module Reflects : sig
  val reflect_of_alias :
    ('a -> Expr.t) -> (Expr.t -> 'a option) -> 'a Refl.t
  val unit_reflect : unit Refl.t
  val bool_reflect : bool Refl.t
  val int_reflect : int Refl.t
  val int53p_reflect : int53p Refl.t
  val float_reflect : float Refl.t
  val string_reflect : string Refl.t
  val uchar_reflect : Uchar.t Refl.t
  val byte_reflect : char Refl.t
  val bytes_reflect : bytes Refl.t
end

module Wellknown : sig
  type _ external_format +=
     | External_format_json : Kxclib.Json.jv external_format
  val json_format : Kxclib.Json.jv external_format
  val json_format' : external_format_label
end

module Json_shape : sig
  type shape_explanation = [
    | `self
    | `named of string*shape_explanation
    | `special of string*shape_explanation
    | `with_warning of string*shape_explanation
    | `exactly of Kxclib.Json.jv
    | `unresolved of string
    | `anyone_of of shape_explanation list
    | `string_enum of string list
    | `nullable of shape_explanation
    | `boolean
    | `numeric
    | `integral | `proper_int53p | `proper_float
    | `string | `base64str
    | `array_of of shape_explanation
    | `tuple_of of shape_explanation list
    | `record_of of shape_explanation
    | `object_of of field_shape_explanation list
    ]
  and field_shape_explanation = [
    | `mandatory_field of string*shape_explanation
    | `optional_field of string*shape_explanation
    ]

  val show_shape_explanation : shape_explanation -> string
  val show_field_shape_explanation : field_shape_explanation -> string

  val pp_shape_explanation : Format.formatter -> shape_explanation -> unit
  val pp_field_shape_explanation : Format.formatter -> field_shape_explanation -> unit

  val string_of_shape_explanation : shape_explanation -> string
  val string_of_field_shape_explanation : field_shape_explanation -> string
end

type json_shape_explanation = Json_shape.shape_explanation
type json_field_shape_explanation = Json_shape.field_shape_explanation

val string_of_json_shape_explanation : json_shape_explanation -> string
val string_of_json_field_shape_explanation : json_field_shape_explanation -> string
