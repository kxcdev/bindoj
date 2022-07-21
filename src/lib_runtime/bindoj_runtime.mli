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

include module type of Utils

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
  val float_reflect : float Refl.t
  val string_reflect : string Refl.t
  val uchar_reflect : Uchar.t Refl.t
  val byte_reflect : char Refl.t
  val bytes_reflect : bytes Refl.t
end
