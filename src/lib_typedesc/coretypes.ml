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

open Typed_type_desc

open Coretype

type _ t =
  | C : Coretype.desc * [`coretype] configs -> 'typ t

let to_desc : 'x t -> desc = function
  | C (desc, _) -> desc

let configs : 'x t -> [`coretype] configs = function
  | C (_, configs) -> configs

let to_coretype : 'x t -> coretype =
  fun ct ->
  let ct_desc, ct_configs = ct |> dup |> to_desc // configs in
  { ct_desc; ct_configs }

let with_config : [`coretype] configs -> 'x t -> 'x t =
  fun configs -> function
    | C (desc, _)  -> C (desc, configs)

module Prims = struct
  let unit : unit t = C (prim `unit, Configs.empty)
  let bool : bool t = C (prim `bool, Configs.empty)
  let int : int t = C (prim `int, Configs.empty)
  let int53p : int53p t = C (prim `int53p, Configs.empty)
  let float : float t = C (prim `float, Configs.empty)
  let string : string t = C (prim `string, Configs.empty)
  let uchar : Uchar.t t = C (prim `uchar, Configs.empty)
  let byte : int t = C (prim `byte, Configs.empty)
  let bytes: bytes t = C (prim `bytes, Configs.empty)
end

let ident : ?codec:Coretype.codec -> string  -> 'any t =
  fun ?(codec = `default) id ->
  C (ident ~codec id, Configs.empty)

let ident' : ?codec:Coretype.codec -> 'x typed_type_decl -> 'x t =
  fun ?codec ttd -> ident ?codec (Typed.decl ttd).td_name

let uninhabitable : Kxclib.null t =
  C (Uninhabitable, Configs.empty)

let option : 'v t -> 'v option t =
  fun ct -> C (to_desc ct |> option, Configs.empty)

let tuple_tag = Obj.tag (Obj.repr (0, 0))
let is_tuple : 'x. 'x -> bool =
  fun (type x) (tup : x) ->
  let tup = Obj.magic tup in
  Obj.is_block tup && Obj.tag tup = tuple_tag

module Tuple = struct
  let tuple_unsafe : 'tup_t -> 'tup t =
    fun tup ->
    let r = Obj.repr tup in
    if not (is_tuple r) then (
      failwith "Bindoj.Coretypes.tuple: argument is not a tuple of Coretypes.t values");
    let ds =
      iota (Obj.size r) |&> Obj.field r
      |&> (fun pos -> Obj.obj pos |> to_desc) in
    C (tuple ds, Configs.empty)

  let tup2 : ('v1 t * 'v2 t) -> ('v1 * 'v2) t = tuple_unsafe
  let tup3 : ('v1 t * 'v2 t * 'v3 t) -> ('v1 * 'v2 * 'v3) t = tuple_unsafe
  let tup4 : ('v1 t * 'v2 t * 'v3 t * 'v4 t) -> ('v1 * 'v2 * 'v3 * 'v4) t = tuple_unsafe
  let tup5 : ('v1 t * 'v2 t * 'v3 t * 'v4 t * 'v5) -> ('v1 * 'v2 * 'v3 * 'v4 * 'v5) t = tuple_unsafe
  let tup6 : ('v1 t * 'v2 t * 'v3 t * 'v4 t * 'v5 t * 'v6 t) -> ('v1 * 'v2 * 'v3 * 'v4 * 'v5 * 'v6) t = tuple_unsafe
  let tup7 : ('v1 t * 'v2 t * 'v3 t * 'v4 t * 'v5 t * 'v6 t * 'v7 t) -> ('v1 * 'v2 * 'v3 * 'v4 * 'v5 * 'v6 * 'v7) t = tuple_unsafe
  let tup8 : ('v1 t * 'v2 t * 'v3 t * 'v4 t * 'v5 t * 'v6 t * 'v7 t * 'v8 t) -> ('v1 * 'v2 * 'v3 * 'v4 * 'v5 * 'v6 * 'v7 * 'v8) t = tuple_unsafe
  let tup9 : ('v1 t * 'v2 t * 'v3 t * 'v4 t * 'v5 t * 'v6 t * 'v7 t * 'v8 t * 'v9 t) -> ('v1 * 'v2 * 'v3 * 'v4 * 'v5 * 'v6 * 'v7 * 'v8 * 'v9) t = tuple_unsafe
end

let list : 'v t -> 'v list t =
  fun ct -> C (to_desc ct |> list, Configs.empty)

module Map = struct
  let string_map : 'v t -> (string * 'v) list t =
    fun ct -> C (to_desc ct |> map `string, Configs.empty)
end

module Enum = struct
  type 't poly = 't constraint 't = [>]
  let string_enum : ('tags poly * Coretype.string_enum_case) list -> 'tags t =
    fun ts -> C (StringEnum (ts |&> snd), Configs.empty)
end

let rec to_refl_coretype' :
    'x. (* assert polymorphism *)
    ?env:tdenv
    -> ?self:'x Refl.t
    -> ?name:string
    -> 'x t
    -> [`Alias | `Self ] * 'x Refl.t =
  fun ?(env = Type_decl_environment.empty)
      ?self ?name ct ->
  let module Helpers = struct
      type 't res = ('t -> Expr.t) * (Expr.t -> 't option)
      let return_alias : 'a res -> [`Alias | `Self] * 'b Refl.t = fun conv ->
        let (get, mk) : 'b res = Obj.magic conv in
        `Alias, Reflects.reflect_of_alias get mk
      let to_get_mk : [`Alias | `Self] * 'a Refl.t -> 'a res = fun refl ->
        match refl with
        | `Alias, lazy (Alias { get; mk; }) -> get, mk
        | `Self, refl ->
          (fun v -> Expr.Refl (refl, v)), (function
            | Expr.Refl (_, v) -> Some (Obj.magic v)
            | _ -> None)
        | _ -> failwith' "panic @%s" __LOC__
    end in
  let open Helpers in
  let subname s = match name with
    | None -> None
    | Some n -> sprintf "%s.%s" n s |> some in
  let name_debug_msg_for = name >? sprintf " for coretype %S" |? "" in
  let name_debug_msg_paren = name >? sprintf " (coretype %S)" |? "" in
  (match self, ct with
  | _, C (Prim `unit, _) -> Expr.(of_unit, to_unit) |> return_alias
  | _, C (Prim `bool, _) -> Expr.(of_bool, to_bool) |> return_alias
  | _, C (Prim `int, _) -> Expr.(of_int, to_int) |> return_alias
  | _, C (Prim `int53p, _) -> Expr.(of_int53p, to_int53p) |> return_alias
  | _, C (Prim `float, _) -> Expr.(of_float, to_float) |> return_alias
  | _, C (Prim `string, _) -> Expr.(of_string, to_string) |> return_alias
  | _, C (Prim `uchar, _) -> Expr.(of_uchar, to_uchar) |> return_alias
  | _, C (Prim `byte, _) -> Expr.(of_byte, to_byte) |> return_alias
  | _, C (Prim `bytes, _) -> Expr.(of_bytes, to_bytes) |> return_alias
  | _, C (Uninhabitable, _) ->
    let throw msg =
      failwith' "impossible %s: inhabitable type%s"
        msg name_debug_msg_paren in
    (throw "Refl.Alias.get", throw "Refl.Alias.mk") |> return_alias
  | _, (C (Ident { id_name; _ }, _) : 'y t) ->
    ident_to_typed_type_decl_opt env id_name
    >? (to_refl_type_decl &> fun refl -> Expr.(of_refl refl, to_refl refl) |> return_alias)
    |?! fun() ->
        failwith' "Bindoj.Coretypes.to_refl: cannot find type named %S in provided tdenv%s"
          id_name name_debug_msg_for
  | _, (C (Option desc, _) : 'y t) ->
    let o, t = to_refl_coretype' ~env (C (desc, Configs.empty)) |> to_get_mk in
    (Expr.(of_option o, to_option t) : 'y option res) |> return_alias
  | _, (C (List desc, _) : 'y t) ->
    let o, t = to_refl_coretype' ~env (C (desc, Configs.empty)) |> to_get_mk in
    (Expr.(of_list o, to_list t) : 'y list res) |> return_alias
  | _, C (Tuple ds, _) ->
    let refls =
      Array.of_list ds
      |> Array.mapi (fun i desc ->
        let subname = sprintf "_%d" i |> subname in
        to_refl_coretype' ~env ?name:subname (C (desc, Configs.empty))) in
    let len = Array.length refls in
    let get tup =
      if not (is_tuple tup) then (
        invalid_arg' "Refl.Alias.get: expecting a tuple of length %d%s"
          len name_debug_msg_for
      );
      let r = Obj.repr tup in
      let es =
        iota len |&> (fun i ->
          (to_get_mk refls.(i) |> fst) (Obj.obj (Obj.field r i))
        ) in
      Expr.Tuple es in
    let mk : Expr.t -> _ option = function
      | Tuple vs ->
        let vs = Array.of_list vs in
        if Array.length vs <> len then (
          invalid_arg' "Refl.Alias.get: bad Expr.Tuple length: expected %d, got %d%s"
            len (Array.length vs) name_debug_msg_for);
        let r = Obj.new_block tuple_tag len in
        let open MonadOps(Option) in
        iota len |+&> Array.get vs |&> (fun (i, e) ->
          (to_get_mk refls.(i) |> snd) e
        ) |> sequence_list
        >? (fun vs ->
          vs |> List.iteri (fun i v ->
            Obj.set_field r i v);
          Obj.obj r)
      | Refl (_, x) -> some (Obj.magic x)
      | _ ->
        invalid_arg' "Refl.Alias.get: expecting an Expr.Tuple of length %d%s"
          len name_debug_msg_for
    in
    (get, mk) |> return_alias
  | _, C (Map (`string, desc0), _) ->
    let refl0: _ * 'e Refl.t =
      to_refl_coretype' ~env ?name:(subname "elem") (C (desc0, Configs.empty)) in
    let get : (string*'e) list -> Expr.t =
      fun fs -> Expr.Map (fs |&> fun (k, (v : 'e)) -> k, (to_get_mk refl0 |> fst) v) in
    let mk : Expr.t -> (string*'e) list option = function
      | Expr.Refl (_, x) -> some (Obj.magic x)
      | Map fs ->
        let open MonadOps(Option) in
        fs |&> (fun (k, e) -> (to_get_mk refl0 |> snd) e >? (fun v -> k, v))
        |> sequence_list
      | _ ->
        invalid_arg' "Refl.Alias.mk: expecting an Expr.Map%s" name_debug_msg_for
    in
    (get, mk) |> return_alias
  | _, C (StringEnum tags, _) ->
    let tags =
      tags
      |&> (fun (name, _, _) -> name)
      |+&> Obj.magic % Kxclib.Obj.hash_variant
      |&> swap in
    let tags' = tags |> List.rev_map swap in
    let get x = Expr.StringEnum (List.assoc x tags) in
    let mk = function
      | Expr.Refl (_, x) -> some (Obj.magic x)
      | StringEnum s ->
        List.assoc_opt s tags' >? Obj.magic
      | _ ->
        invalid_arg' "Refl.Alias.mk: expecting an Expr.StringEnum%s" name_debug_msg_for
    in
    (get, mk) |> return_alias
  | None, (C (Self, _)) ->
    failwith' "Bindoj.Coretypes.to_refl: unsound recursive coretype%s" name_debug_msg_for
  | Some refl, (C (Self, _)) -> (`Self, refl)
  ) /> Obj.magic

and to_refl_type_decl : 'x. 'x typed_type_decl -> 'x Refl.t =
  fun (type x) ((module Td) : x typed_type_decl) -> Td.reflect

and ident_to_typed_type_decl_opt (env: tdenv) (id_name: string): 'x typed_type_decl option =
  let module Tdenv = Type_decl_environment in
  StringMap.find_opt id_name env.alias_ident_typemap
  >? (fun (Boxed td) -> (Obj.magic td : 'x typed_type_decl))

let to_refl : ?env:Typed_type_desc.tdenv -> ?self:'x Refl.t -> 'x t -> 'x Refl.t =
  fun ?env ?self ct -> to_refl_coretype' ?env ?self ct |> snd

let of_typed_type_decl :
    ?codec:Coretype.codec ->
    'x Typed_type_desc.typed_type_decl ->
    'x t =
  fun ?codec (ttd) ->
  match (Typed.decl ttd) with
  | { td_kind=Alias_decl { ct_desc; ct_configs }; _ } ->
    if Option.is_some codec then
      Log0.warn "Bindoj.Coretypes.of_typed_type_decl: 'codec' was specified as an argument, but its value is not used."
    ;
    C (ct_desc, ct_configs)
  | { td_name; _ } ->
    ident ?codec td_name

let of_ = of_typed_type_decl

let to_typed_type_decl :
    ?env:Typed_type_desc.tdenv
    -> ?configs: [`type_decl] configs
    -> ?self:'x Refl.t
    -> string
    -> 'x t
    -> 'x Typed_type_desc.typed_type_decl =
  fun ?(env=Type_decl_environment.empty) ?configs ?self name ct ->
  let decl = alias_decl ?configs name (to_coretype ct) in
  let refl = to_refl ~env ?self ct in
  Typed_type_desc.Typed.mk decl refl
