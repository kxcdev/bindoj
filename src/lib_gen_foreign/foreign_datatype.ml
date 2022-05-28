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

open Bindoj_typedesc.Type_desc
open Bindoj_gen.Json_codec

type ('ann0, 'ann1) fwrt_desc = {
  fd_name : string;
  fd_kind_fname : string;
  fd_parent : string option;
  fd_children : string list;
  fd_fields : 'ann1 fwrt_field_desc with_docstr list;
  fd_annot : 'ann0;
}
and 'ann fwrt_field_desc = {
  ff_name : string;
  ff_type : string list;
  ff_annot : 'ann;
}

module FwrtTypeEnv = struct
  module StringMap = Map.Make (String)
  type ('ann0, 'ann1) t =  ('ann0, 'ann1) fwrt_desc with_docstr StringMap.t
  let init : ('ann0, 'ann1) t = StringMap.empty
  let bind :
    ?doc:([`docstr of string | `nodoc]) -> ?parent:(string option) -> ?kind_fname:(string) ->
    annot:'ann0 -> string -> 'ann1 fwrt_field_desc with_docstr list -> ('ann0, 'ann1) t ->
    ('ann0, 'ann1) t =
    fun ?(doc = `nodoc) ?(parent = None) ?(kind_fname = default_kind_fname) ~annot name fields env ->
    let register_child parent child env =
      match StringMap.find_opt parent env with
      | None -> failwith "the parent does not exist"
      | Some ({ fd_children; _; } as desc, doc) ->
        StringMap.add
          parent
          ({ desc with fd_children = child :: fd_children; }, doc)
          env in
    let add_new_type env =
      StringMap.add
        name
        ({ fd_name = name;
           fd_kind_fname = kind_fname;
           fd_parent = parent;
           fd_children = [];
           fd_fields = fields;
           fd_annot = annot; }, doc)
        env in
    match parent with
    | None -> add_new_type env
    | Some parent_name ->
      register_child parent_name name env
      |> add_new_type
  let lookup : string -> ('ann0, 'ann1) t -> ('ann0, 'ann1) fwrt_desc with_docstr =
    StringMap.find
  let lookup_opt : string -> ('ann0, 'ann1) t -> ('ann0, 'ann1) fwrt_desc with_docstr option  =
    StringMap.find_opt
  let bindings = StringMap.bindings
  let annotate : string -> ('ann0 * 'ann0) -> ('ann1 * 'ann1) -> (unit, unit) t -> ('ann0, 'ann1) t =
    fun typ (ann0, ann0') (ann1, ann1') env ->
    let ({ fd_fields; _; } as desc, doc) = lookup typ env in
    bindings env |@> (init, fun (acc, (name, ({ fd_fields; _; } as desc, doc))) ->
        StringMap.add
          name
          ({ desc with
             fd_fields = (fd_fields |&> fun (field, field_doc) ->
                 ({ field with ff_annot = ann1' }, field_doc));
             fd_annot = ann0' }, doc)
          acc) |>
    StringMap.add
      typ
      ({ desc with
         fd_annot = ann0;
         fd_fields = fd_fields |&> fun (field, field_doc) ->
             ({ field with ff_annot = ann1 }, field_doc); },
       doc)
end

type ('ann0, 'ann1) fwrt_type_env = ('ann0, 'ann1) FwrtTypeEnv.t

type ('ann0, 'ann1) fwrt_decl = string * ('ann0, 'ann1) fwrt_type_env

type flavor = variant_type_flavor

module TypeMap = struct
  module StringMap = Map.Make (String)
  type t = string StringMap.t
  let empty : t = StringMap.empty
  let add_convertion : string -> string -> t -> t = StringMap.add
  let add_fixed_point : string -> t -> t = fun typ type_map ->
    add_convertion typ typ type_map
  let convert_type : t -> string -> string = fun type_map typ ->
    match StringMap.find_opt typ type_map with
    | None -> typ
    | Some typ -> typ
end

type type_map = TypeMap.t

let fwrt_decl_of_type_decl : flavor -> type_decl -> (unit, unit) fwrt_decl =
  function
  | `flat_kind -> begin function
      | { td_name; td_kind=(Record_kind record, doc); _ } ->
        let fields =
          record |&> fun ({ rf_name; rf_type; _; }, rf_doc) ->
            ({ ff_name = rf_name;
               ff_type = [rf_type];
               ff_annot = (); }, rf_doc) in
        (td_name,
         FwrtTypeEnv.init
         |> FwrtTypeEnv.bind ~doc ~annot:() td_name fields)
      | { td_name; td_kind=(Variant_kind variant, doc); _ } ->
        let get_fnames flvconfigs =
          FlavorConfigs.find_or_default (function
            | Flvconfig_flat_kind { kind_fname; arg_fname; _ } ->
              Some (kind_fname_value kind_fname, arg_fname_value arg_fname)
            | _ -> None
          ) flvconfigs ~default:(default_kind_fname, default_arg_fname)
        in
        let cstrs =
          variant |&> function
            | Cstr_tuple { ct_name; ct_args; ct_flvconfigs; _; }, ct_doc ->
              let kind_fname, arg_fname = get_fnames ct_flvconfigs in
              let fields = match ct_args with
                | [] -> []
                | _ -> [{ ff_name = arg_fname; ff_type = ct_args; ff_annot = (); }, ct_doc] in
              (ct_name, kind_fname, fields, ct_doc)
            | Cstr_record { cr_name; cr_fields; cr_flvconfigs; _; }, cr_doc ->
              let kind_fname, _ = get_fnames cr_flvconfigs in
              let fields =
                cr_fields |&> fun ({ rf_name; rf_type; _; }, rf_doc) ->
                  ({ ff_name = rf_name;
                     ff_type = [rf_type];
                     ff_annot = (); }, rf_doc) in
              (cr_name, kind_fname, fields, cr_doc) in
        (td_name,
         FwrtTypeEnv.init
         |> FwrtTypeEnv.bind ~doc ~annot:() td_name []
         |> fun env ->
         List.rev cstrs |@> (env, fun (acc, (name, kind_fname, fields, doc)) ->
             FwrtTypeEnv.bind
               ~doc ~annot:() ~kind_fname ~parent:(Some td_name) name fields acc))
    end
