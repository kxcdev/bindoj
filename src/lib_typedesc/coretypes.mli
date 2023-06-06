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

open Type_desc

type 'x t

val with_config : [`coretype] configs -> 'x t -> 'x t

val to_refl : ?env:Typed_type_desc.tdenv -> ?self:'x Refl.t -> 'x t -> 'x Refl.t
val to_coretype : 'x t -> coretype

val of_typed_type_decl : ?codec:Coretype.codec -> 'x Typed_type_desc.typed_type_decl -> 'x t
val of_ : ?codec:Coretype.codec -> 'x Typed_type_desc.typed_type_decl -> 'x t

val to_typed_type_decl :
  ?env:Typed_type_desc.tdenv
  -> ?configs:[`type_decl] configs
  -> ?self:'x Refl.t
  -> string
  -> 'x t
  -> 'x Typed_type_desc.typed_type_decl

module Prims : sig
  val unit : unit t
  val bool : bool t
  val int : int t
  val int53p : int53p t
  val float : float t
  val string : string t
  val uchar : Uchar.t t
  val byte : int t
  val bytes: bytes t
end

val ident : ?codec:Coretype.codec -> string  -> 'any t
(** it is the caller's responsibility to ensure that [id] resolves to a type_decl of
    type with OCaml repr of type ['any] whenever [ident id] is used *)

val ident' : ?codec:Coretype.codec -> 'x Typed_type_desc.typed_type_decl -> 'x t

val uninhabitable : Kxclib.null t

val option : 'v t -> 'v option t

module Tuple : sig
  val tuple_unsafe : 'tup_t -> 'tup t
  (** the first argument must be of type ['v1 t * 'v2 t * .. * 'vk t]
      and the return type shall be casted/restricted to [('v1 * 'v2 * .. * 'vk) t]
      it is the caller's responsibility to ensure the type matches.

      the [tupN] functions are provided for N=2..9 and is recommended over this function *)

  val tup2 : ('v1 t * 'v2 t) -> ('v1 * 'v2) t
  val tup3 : ('v1 t * 'v2 t * 'v3 t) -> ('v1 * 'v2 * 'v3) t
  val tup4 : ('v1 t * 'v2 t * 'v3 t * 'v4 t) -> ('v1 * 'v2 * 'v3 * 'v4) t
  val tup5 : ('v1 t * 'v2 t * 'v3 t * 'v4 t * 'v5) -> ('v1 * 'v2 * 'v3 * 'v4 * 'v5) t
  val tup6 : ('v1 t * 'v2 t * 'v3 t * 'v4 t * 'v5 t * 'v6 t) -> ('v1 * 'v2 * 'v3 * 'v4 * 'v5 * 'v6) t
  val tup7 : ('v1 t * 'v2 t * 'v3 t * 'v4 t * 'v5 t * 'v6 t * 'v7 t) -> ('v1 * 'v2 * 'v3 * 'v4 * 'v5 * 'v6 * 'v7) t
  val tup8 : ('v1 t * 'v2 t * 'v3 t * 'v4 t * 'v5 t * 'v6 t * 'v7 t * 'v8 t) -> ('v1 * 'v2 * 'v3 * 'v4 * 'v5 * 'v6 * 'v7 * 'v8) t
  val tup9 : ('v1 t * 'v2 t * 'v3 t * 'v4 t * 'v5 t * 'v6 t * 'v7 t * 'v8 t * 'v9 t) -> ('v1 * 'v2 * 'v3 * 'v4 * 'v5 * 'v6 * 'v7 * 'v8 * 'v9) t
end

val list : 'v t -> 'v list t

module Map : sig
  val string_map : 'v t -> (string * 'v) list t
end

module Enum : sig
  type 't poly = 't constraint 't = [>]
  val string_enum : ('tags poly * string) list -> 'tags t
(** example: [string_enum [ `monday, "monday" ; `tuesday, "tuesday"; .. ]]

    caller must ensure that for each element of the argument,
    (1) the string matches the name of the polymorphic varaint constructor; and
    (2) there is no duplication *)
end
