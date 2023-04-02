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
open Bindoj_base.Type_desc
open Bindoj_runtime

[@@@alert "-equal_configs"]

type 't ignore_order_list = 't list [@@deriving show]
let equal_ignore_order_list equal_t xs ys =
  List.equal equal_t (List.sort compare xs) (List.sort compare ys)

type ('ann0, 'ann1) fwrt_desc = {
  fd_name : string;
  fd_parent : string option;
  fd_kind : 'ann1 fwrt_desc_kind;
  fd_annot : 'ann0;
  fd_doc : doc;
} [@@deriving show,eq]

and 'ann fwrt_desc_kind =
  | Fwrt_object of {
      fo_fields: 'ann fwrt_field_desc ignore_order_list;
      fo_children : string ignore_order_list;
      fo_configs: [`type_decl] configs
    }
  | Fwrt_alias of {
      fa_type: coretype;
      fa_configs: [`type_decl] configs
    }
  | Fwrt_constructor of {
      fc_args: coretype list;
      fc_fields: 'ann fwrt_field_desc ignore_order_list;
      fc_configs: [`variant_constructor] configs
    }
  [@@deriving show,eq]

and 'ann fwrt_field_desc = {
  ff_name : string;
  ff_type : coretype;
  ff_configs: [`record_field] configs;
  ff_annot : 'ann;
  ff_doc : doc;
} [@@deriving show,eq]

module FwrtTypeEnv = struct
  type ('ann0, 'ann1) t =  ('ann0, 'ann1) fwrt_desc StringMap.t

  let init : ('ann0, 'ann1) t = StringMap.empty

  let field ?(doc = `nodoc) ?(configs=Configs.empty) ~annot ff_name ff_type =
    { ff_name; ff_type; ff_configs = configs; ff_annot = annot; ff_doc = doc }

  let bind ?(doc = `nodoc) ?parent ~annot name kind env =
    let register_child parent child env =
      match StringMap.find_opt parent env with
      | None -> failwith "the parent does not exist"
      | Some desc ->
        match desc.fd_kind with
        | Fwrt_object o ->
          let fd_kind = Fwrt_object { o with fo_children = List.sort String.compare (child :: o.fo_children) } in
          StringMap.add parent { desc with fd_kind } env
        | Fwrt_alias _ | Fwrt_constructor _ ->
          failwith' "the type '%s' cannot have children" desc.fd_name
    in
    let add_new_type env =
      StringMap.add
        name
        { fd_name = name;
          fd_parent = parent;
          fd_kind = kind;
          fd_annot = annot;
          fd_doc = doc }
        env in
    match parent with
    | None -> add_new_type env
    | Some parent_name ->
      register_child parent_name name env
      |> add_new_type

  let compare_field (f1: _ fwrt_field_desc) (f2: _ fwrt_field_desc) =
    compare f1.ff_name f2.ff_name

  let bind_object ?doc ?parent ?(configs = Configs.empty) ~annot name fields env =
    let kind =
      Fwrt_object {
        fo_fields = List.sort compare_field fields;
        fo_configs = configs;
        fo_children = [];
      }
    in
    bind ?doc ?parent ~annot name kind env

  let bind_alias ?doc ?parent ?(configs=Configs.empty) ~annot name ty env =
    let kind =
      Fwrt_alias {
        fa_type = ty;
        fa_configs = configs;
      }
    in
    bind ?doc ?parent ~annot name kind env

  let bind_constructor ?doc ?parent ?(configs=Configs.empty) ~annot ?(args=[]) ?(fields=[]) name env =
    let kind =
      Fwrt_constructor {
        fc_args = args;
        fc_fields = List.sort compare_field fields;
        fc_configs = configs;
      }
    in
    bind ?doc ?parent ~annot name kind env

  let lookup : string -> ('ann0, 'ann1) t -> ('ann0, 'ann1) fwrt_desc =
    StringMap.find

  let lookup_opt : string -> ('ann0, 'ann1) t -> ('ann0, 'ann1) fwrt_desc option  =
    StringMap.find_opt

  let bindings = StringMap.bindings

  let annotate_kind ann = function
    | Fwrt_alias a -> Fwrt_alias a
    | Fwrt_constructor c ->
      let fc_fields =
        c.fc_fields |> List.map (fun field -> { field with ff_annot = ann }) |> List.sort compare_field
      in
      Fwrt_constructor { c with fc_fields }
    | Fwrt_object o ->
      let fo_fields =
        o.fo_fields |> List.map (fun field -> { field with ff_annot = ann }) |> List.sort compare_field
      in
      Fwrt_object { o with fo_fields }

  let annotate typ (ann0_of_type, ann0_of_other) (ann1_of_typ, ann1_of_other) (env: _ t) =
    env
    |> StringMap.mapi (fun name desc ->
      let ann0, ann1 =
        if name = typ then ann0_of_type, ann1_of_typ
        else ann0_of_other, ann1_of_other
      in
      { desc with
          fd_kind = annotate_kind ann1 desc.fd_kind;
          fd_annot = ann0; })

end

type ('ann0, 'ann1) fwrt_type_env = ('ann0, 'ann1) FwrtTypeEnv.t

type ('ann0, 'ann1) fwrt_decl = string * ('ann0, 'ann1) fwrt_type_env

let pp_fwrt_decl ann0 ann1 ppf (name, env) =
  let fd = FwrtTypeEnv.lookup name env in
  pp_fwrt_desc ann0 ann1 ppf fd

let show_fwrt_decl ann0 ann1 (name, env) =
  let fd = FwrtTypeEnv.lookup name env in
  show_fwrt_desc ann0 ann1 fd

let equal_fwrt_decl ann0 ann1 (name1, env1) (name2, env2) =
  let fd1 = FwrtTypeEnv.lookup name1 env1 in
  let fd2 = FwrtTypeEnv.lookup name2 env2 in
  equal_fwrt_desc ann0 ann1 fd1 fd2

let fwrt_decl_of_type_decl : type_decl -> (unit, unit) fwrt_decl =
  let conv_fields fields =
    fields |&> fun { rf_name; rf_type; rf_configs; rf_doc; } -> {
      ff_name = rf_name;
      ff_type = rf_type;
      ff_configs = rf_configs;
      ff_annot = ();
      ff_doc = rf_doc
    }
  in
  fun { td_name; td_kind; td_configs = configs; td_doc = doc } ->
    match td_kind with
    | Alias_decl typ ->
      td_name,
      FwrtTypeEnv.init |> FwrtTypeEnv.bind_alias ~doc ~annot:() ~configs td_name typ
    | Record_decl fields ->
      td_name,
      FwrtTypeEnv.init |> FwrtTypeEnv.bind_object ~doc ~annot:() ~configs td_name (conv_fields fields)
    | Variant_decl ctors ->
      let add_ctor parent acc { vc_name; vc_param; vc_configs=configs; vc_doc=doc } =
        match vc_param with
        | `no_param ->
          acc |> FwrtTypeEnv.bind_constructor ~doc ~parent ~configs ~annot:() vc_name
        | `tuple_like args ->
          acc |> FwrtTypeEnv.bind_constructor ~doc ~parent ~configs ~annot:() ~args vc_name
        | `inline_record fields ->
          let fields = conv_fields fields in
          acc |> FwrtTypeEnv.bind_constructor ~doc ~parent ~configs ~annot:() ~fields vc_name
      in
      td_name,
      FwrtTypeEnv.init
      |> FwrtTypeEnv.bind_object ~doc ~annot:() ~configs td_name []
      |> fun env ->
        ctors |> List.fold_left (add_ctor td_name) env
