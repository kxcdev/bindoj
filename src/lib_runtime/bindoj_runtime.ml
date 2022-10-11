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
include Utils

type 'camlrepr external_format = ..
type ('tag, 'datatype_expr) foreign_language = ..

type external_format_label = External_format : _ external_format -> external_format_label
type foreign_language_label = Foreign_language : _ foreign_language -> foreign_language_label

module External_format = struct
  module LabelOrdered = struct
    type t = external_format_label
    let compare x y = Stdlib.compare x y
  end
  module LabelSet = Set.Make(LabelOrdered)
  module LabelMap = Map.Make(LabelOrdered)
  type label_set = LabelSet.t
  type 'x label_map = 'x LabelMap.t

  type ('t, 'ext) codec = {
      encode : 't -> 'ext;
      decode : 'ext -> 't option;
    }
  type 't codec' =
    Codec : 'ext external_format*('t, 'ext) codec -> 't codec'
  type 't codecs = 't codec' LabelMap.t
end

module rec Refl : sig
  type 't constructor =
    | InlineRecord of {
        get: 't -> Expr'.t StringMap.t;
        mk: Expr'.t StringMap.t -> 't option;
      }
    | TupleLike of {
        get: 't -> Expr'.t list;
        mk: Expr'.t list -> 't option;
      }
    | NoParam of {
        value: 't
      }

  type 't result =
    | Record of {
        get: 't -> Expr'.t StringMap.t;
        mk: Expr'.t StringMap.t -> 't option;
      }
    | Variant of {
        constructors: 't constructor StringMap.t;
        classify: 't -> (string * 't constructor);
      }
    | Alias of {
        get: 't -> Expr'.t;
        mk: Expr'.t -> 't option;
      }

  type 'a t = 'a result Lazy.t
end = Refl

and Expr' : sig
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
end = Expr'

module Expr = struct
  include Expr'
  open Option.Ops_monad

  let of_unit () = Unit
  let to_unit = function Unit -> some () | _ -> none
  let of_bool b = Bool b
  let to_bool = function Bool b -> some b | _ -> none
  let of_int i = Int i
  let to_int = function Int i -> some i | _ -> none
  let of_float f = Float f
  let to_float = function Float f -> some f | _ -> none
  let of_string s = String s
  let to_string = function String s -> some s | _ -> none
  let of_uchar c = Uchar c
  let to_uchar = function Uchar c -> some c | _ -> none
  let of_byte b = Byte b
  let to_byte = function Byte b -> some b | _ -> none
  let of_bytes s = Bytes s
  let to_bytes = function Bytes s -> some s | _ -> none
  let of_option of_a = function
    | Option.Some a -> Some (of_a a)
    | Option.None -> None
  let to_option to_a e =
    match e with
    | Some x -> to_a x |> Option.map (fun x -> some x)
    | None -> some none
    | _ -> none
  let of_list of_a xs = List (xs |> List.map of_a)
  let to_list to_a =
    function
    | List xs -> xs |> List.map to_a |> sequence_list
    | _ -> none
  let of_map of_a xs = Map (xs |> List.map (fun (k, v) -> k, of_a v))
  let to_map to_a = function
    | Map xs ->
      xs |> List.map (fun (k, v) ->
        to_a v |> Option.map (fun v -> k, v)
      ) |> sequence_list
    | _ -> none
  let of_refl refl x = Refl (refl, x)
  let to_refl (type a) (refl: a Refl.t) : t -> a option = function
    | Refl (refl', x) ->
      if Obj.magic refl == Obj.magic refl' then Some (Obj.magic x)
      else None
    | _ -> None
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

let mk_generic_typed_type_decl
    : type type_decl t. type_decl -> t Refl.t
           -> (type_decl, t) generic_typed_type_decl =
  fun decl refl -> (
    module struct
      type nonrec type_decl = type_decl
      type nonrec t = t
      let decl = decl
      let reflect = refl
    end)

module Reflects = struct
  open Refl

  let reflect_of_alias get mk : _ Refl.t =
    lazy (Alias { get; mk })

  let unit_reflect =
    Expr.(reflect_of_alias
            of_unit to_unit)
  let bool_reflect =
    Expr.(reflect_of_alias
            of_bool to_bool)
  let int_reflect =
    Expr.(reflect_of_alias
            of_int to_int)
  let float_reflect =
    Expr.(reflect_of_alias
            of_float to_float)
  let string_reflect =
    Expr.(reflect_of_alias
            of_string to_string)
  let uchar_reflect =
    Expr.(reflect_of_alias
            of_uchar to_uchar)
  let byte_reflect =
    Expr.(reflect_of_alias
            of_byte to_byte)
  let bytes_reflect =
    Expr.(reflect_of_alias
            of_bytes to_bytes)
end

module Wellknown = struct
  type _ external_format +=
     | External_format_json : Kxclib.Json.jv external_format
  let json_format = External_format_json
  let json_format' = External_format json_format
end
