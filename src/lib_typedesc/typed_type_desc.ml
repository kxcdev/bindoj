(* Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

open Utils

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
  open MonadOps(Option)

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

include Type_desc

module type T = sig
  type t
  val decl : type_decl
  val reflect : t Refl.t
end

type 'a typed_type_decl = (module T with type t = 'a)

type boxed_type_decl =
  | Boxed : 'a typed_type_decl -> boxed_type_decl

module Typed = struct
  let mk (type a) decl reflect : a typed_type_decl =
    (module struct
      type t = a
      let decl = decl
      let reflect = reflect
    end)

  let decl : 'a typed_type_decl -> type_decl =
    fun (type a) (module A: T with type t = a) -> A.decl

  let reflect : 'a typed_type_decl -> 'a Refl.result =
    fun (type a) (module A: T with type t = a) -> Lazy.force A.reflect

  let to_refl : 'a typed_type_decl -> 'a Refl.t =
    fun (type a) (module A: T with type t = a) -> A.reflect

  let cast (a: 'a typed_type_decl) (b: 'b typed_type_decl) (value: 'a) : 'b option =
    if decl a = decl b then Some (Obj.magic value : 'b)
    else None

  let box (a: 'a typed_type_decl) : boxed_type_decl = Boxed a

  let unbox (Boxed a) : 'a typed_type_decl = Obj.magic a
end

