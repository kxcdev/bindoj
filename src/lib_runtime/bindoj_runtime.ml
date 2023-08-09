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
include Utils

open Kxclib.Json

type 'camlrepr external_format = ..
type ('tag, 'datatype_expr) foreign_language = ..

type external_format_label = External_format : _ external_format -> external_format_label
type foreign_language_label = Foreign_language : _ foreign_language -> foreign_language_label

module External_format = struct
  module LabelOrdered = struct
    type t = external_format_label
    let compare x y = compare x y
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
    | ReusedInlineRecord of {
      get: 't -> Expr'.t StringMap.t;
      mk: Expr'.t StringMap.t -> 't option;
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

  and 'a t = 'a result Lazy.t
end = Refl

and Expr' : sig
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
  let of_int53p i = Int53p i
  let to_int53p = function Int53p i -> some i | _ -> none
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
      if Lazy.force refl == Obj.magic (Lazy.force refl') then Some (Obj.magic x)
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

type _ boxed_generic_typed_type_decl =
  | Boxed_generic_typed_type_decl :
      ('type_decl, _) generic_typed_type_decl -> ('type_decl) boxed_generic_typed_type_decl

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

let box_generic_typed_type_decl :
  ('type_decl, 't) generic_typed_type_decl
  -> ('type_decl) boxed_generic_typed_type_decl
  = fun ttd -> Boxed_generic_typed_type_decl ttd

let mk_boxed_generic_typed_type_decl :
  'type_decl -> 't Refl.t
  -> ('type_decl) boxed_generic_typed_type_decl
  = fun decl refl ->
  mk_generic_typed_type_decl decl refl |> box_generic_typed_type_decl

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
  let int53p_reflect =
    Expr.(reflect_of_alias
            of_int53p to_int53p)
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
     | External_format_json : jv external_format
  let json_format = External_format_json
  let json_format' = External_format json_format
end

module Json_shape = struct
  let pp_jv = pp_unparse

  type shape_explanation = [
    | `self
    | `named of string*shape_explanation
    | `special of string*shape_explanation
    | `with_warning of string*shape_explanation
    | `exactly of jv
    | `any_json_value
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
  [@@deriving show]
  and field_shape_explanation = [
    | `mandatory_field of string*shape_explanation
    | `optional_field of string*shape_explanation
    ]
  [@@deriving show]

  let string_of_shape_explanation = show_shape_explanation
  let string_of_field_shape_explanation = show_field_shape_explanation
end

type json_shape_explanation = Json_shape.shape_explanation
type json_field_shape_explanation = Json_shape.field_shape_explanation

let string_of_json_shape_explanation = Json_shape.string_of_shape_explanation
let string_of_json_field_shape_explanation = Json_shape.string_of_field_shape_explanation


module type Json_shape_explaner = sig
  type shape
  type field_shape
  val shape_of_json_shape_explanation : json_shape_explanation -> shape
  val self : shape
  val named : string * shape -> shape
  val special : string * shape -> shape
  val with_warning : string * shape -> shape
  val exactly : jv -> shape
  val any_json_value : shape
  val unresolved : string -> shape
  val anyone_of : shape list -> shape
  val string_enum : string list -> shape
  val nullable : shape -> shape
  val boolean : shape
  val numeric : shape
  val integral : shape
  val proper_int53p : shape
  val proper_float : shape
  val string : shape
  val base64str : shape
  val array_of : shape -> shape
  val tuple_of : shape list -> shape
  val record_of : shape -> shape
  val object_of : field_shape list -> shape
  val mandatory_field : string * shape -> field_shape
  val optional_field : string * shape -> field_shape
end

type ('shape, 'field_shape) json_shape_explaner =
(module Json_shape_explaner
  with type shape = 'shape
  and type field_shape = 'field_shape)

let json_shape_explanation : (json_shape_explanation, json_field_shape_explanation) json_shape_explaner =
  (module struct
    type shape = json_shape_explanation
    type field_shape = json_field_shape_explanation
    let shape_of_json_shape_explanation : json_shape_explanation -> shape = identity
    let self : shape = `self
    let named : string * shape -> shape = fun x -> `named x
    let special : string * shape -> shape = fun x -> `special x
    let with_warning : string * shape -> shape = fun x -> `with_warning x
    let exactly : jv -> shape = fun x -> `exactly x
    let any_json_value : shape = `any_json_value
    let unresolved : string -> shape = fun x -> `unresolved x
    let anyone_of : shape list -> shape = fun x -> `anyone_of x
    let string_enum : string list -> shape = fun x -> `string_enum x
    let nullable : shape -> shape = fun x -> `nullable x
    let boolean : shape = `boolean
    let numeric : shape = `numeric
    let integral : shape = `integral
    let proper_int53p : shape = `proper_int53p
    let proper_float : shape = `proper_float
    let string : shape = `string
    let base64str : shape = `base64str
    let array_of : shape -> shape = fun x -> `array_of x
    let tuple_of : shape list -> shape = fun x -> `tuple_of x
    let record_of : shape -> shape = fun x -> `record_of x
    let object_of : field_shape list -> shape = fun x ->  `object_of x
    let mandatory_field : string * shape -> field_shape = fun x -> `mandatory_field x
    let optional_field : string * shape -> field_shape = fun x -> `optional_field x
  end)

module OfJsonResult = struct
  module Err = struct
    type t = string * jvpath * json_shape_explanation
    let to_string (msg, path, _) =
      match path with
      | [] -> sprintf "%s at root" msg
      | path -> sprintf "%s at path %s" msg ((path |> List.rev) |> Json.unparse_jvpath)
    let message (msg, _, _) = msg
    let path (_, path, _) = path
    let shape (_, _, shape) = shape
  end

  module R0 = ResultOf'(struct
                  type nonrec err = Err.t
                  let string_of_err = Err.to_string &> Option.some
                end)
  include R0
  module Ops_monad = MonadOps(R0)
end

type 'a json_full_decoder =
  ?path:Kxclib.Json.jvpath
  -> Kxclib.Json.jv
  -> 'a OfJsonResult.t
