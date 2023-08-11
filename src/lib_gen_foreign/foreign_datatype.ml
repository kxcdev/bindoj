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
open Bindoj_base.Type_desc
open Bindoj_runtime

[@@@alert "-equal_configs"]

type 't ignore_order_list = 't list [@@deriving show]
let equal_ignore_order_list equal_t xs ys =
  List.equal equal_t (List.sort compare xs) (List.sort compare ys)

type codec = Coretype.codec
type coretype_codec = [
    | `default
    | `in_module of string
    | `open_ of string
  ] [@@deriving show, eq]
let pp_codec = pp_coretype_codec
let equal_codec = equal_coretype_codec

type 'ann fwrt_field_desc = {
  ff_name : string;
  ff_type : [ `direct of coretype | `nested of string * codec ];
  ff_configs: [`record_field] configs;
  ff_annot : 'ann;
  ff_doc : doc;
}
  [@@deriving show, eq]

type 'ann fwrt_variant_argument_desc = {
  fva_type : [ `direct of coretype | `nested of string * codec ];
  fva_configs : [`variant_tuple_argument] configs;
  fva_annot : 'ann;
  fva_doc : doc;
} [@@deriving show, eq]

type ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_desc = {
  fd_name : string;
  fd_parent : string option;
  fd_kind : ('ann_f, 'ann_va, 'ann_k) fwrt_desc_kind;
  fd_annot : 'ann_d;
  fd_doc : doc;
}
  constraint 'ann_k = _*_*_

and ('ann_f, 'ann_va, 'ann_k) fwrt_desc_kind =
  | Fwrt_object of {
      fo_fields: 'ann_f fwrt_field_desc list;
      fo_children : string list;
      fo_configs: [`type_decl] configs;
      fo_annot : 'ann_ko;
    }
  | Fwrt_alias of {
      fa_type: coretype;
      fa_configs: [`type_decl] configs;
      fa_annot : 'ann_ka;
    }
  | Fwrt_constructor of {
      fc_args: 'ann_va fwrt_variant_argument_desc list;
      fc_fields: 'ann_f fwrt_field_desc list;
      fc_configs: [`variant_constructor] configs;
      fc_annot : 'ann_kc;
    }
  constraint 'ann_k = 'ann_ko * 'ann_ka * 'ann_kc

module Format = struct
  type ('ann_d, 'ann_f, 'ann_va, 'ann_ko, 'ann_ka, 'ann_kc) fwrt_desc = {
    fd_name : string;
    fd_parent : string option;
    fd_kind : ('ann_f, 'ann_va, 'ann_ko, 'ann_ka, 'ann_kc) fwrt_desc_kind;
    fd_annot : 'ann_d;
    fd_doc : doc;
  } [@@deriving show,eq]

  and ('ann_f, 'ann_va, 'ann_ko, 'ann_ka, 'ann_kc) fwrt_desc_kind =
    | Fwrt_object of {
        fo_fields: 'ann_f fwrt_field_desc ignore_order_list;
        fo_children : string ignore_order_list;
        fo_configs: [`type_decl] configs;
        fo_annot : 'ann_ko;
      }
    | Fwrt_alias of {
        fa_type: coretype;
        fa_configs: [`type_decl] configs;
        fa_annot : 'ann_ka;
      }
    | Fwrt_constructor of {
        fc_args: 'ann_va fwrt_variant_argument_desc list;
        fc_fields: 'ann_f fwrt_field_desc ignore_order_list;
        fc_configs: [`variant_constructor] configs;
        fc_annot : 'ann_kc;
      }
    [@@deriving show,eq]
end

let fwrt_desc_kind_to_format = function
  | Fwrt_object { fo_fields; fo_children; fo_configs; fo_annot } ->
    Format.Fwrt_object { fo_fields; fo_children; fo_configs; fo_annot }
  | Fwrt_alias { fa_type; fa_configs; fa_annot } ->
    Format.Fwrt_alias { fa_type; fa_configs; fa_annot }
  | Fwrt_constructor { fc_args; fc_fields; fc_configs; fc_annot } ->
    Format.Fwrt_constructor { fc_args; fc_fields; fc_configs; fc_annot }

let fwrt_desc_to_format { fd_name; fd_parent; fd_kind; fd_annot; fd_doc } =
  Format.{
    fd_name;
    fd_parent;
    fd_kind = fwrt_desc_kind_to_format fd_kind;
    fd_annot;
    fd_doc;
  }

let pp_fwrt_desc pp_ann_d pp_ann_f pp_ann_va pp_ann_ko pp_ann_ka pp_ann_kc ppf =
  fwrt_desc_to_format
  &> Format.pp_fwrt_desc pp_ann_d pp_ann_f pp_ann_va pp_ann_ko pp_ann_ka pp_ann_kc ppf

let show_fwrt_desc pp_ann_d pp_ann_f pp_ann_va pp_ann_ko pp_ann_ka pp_ann_kc =
  fwrt_desc_to_format
  &> Format.show_fwrt_desc pp_ann_d pp_ann_f pp_ann_va pp_ann_ko pp_ann_ka pp_ann_kc

let equal_fwrt_desc pp_ann_d pp_ann_f pp_ann_va pp_ann_ko pp_ann_ka pp_ann_kc d1 d2 =
  Format.equal_fwrt_desc pp_ann_d pp_ann_f pp_ann_va pp_ann_ko pp_ann_ka pp_ann_kc
    (fwrt_desc_to_format d1) (fwrt_desc_to_format d2)

type ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_type_env = ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_desc StringMap.t
constraint 'ann_k = _*_*_


module FwrtTypeEnv = struct
  type ('ann_d, 'ann_f, 'ann_va, 'ann_k) t =  ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_type_env

  let init : ('ann_d, 'ann_f, 'ann_va, 'ann_k) t = StringMap.empty

  let field ?(doc = `nodoc) ?(configs=Configs.empty) ~annot ff_name ct =
    { ff_name; ff_type = `direct ct; ff_configs = configs; ff_annot = annot; ff_doc = doc }

  let field_nested ?(doc = `nodoc) ?(configs=Configs.empty) ?(codec=`default) ~annot ff_name nested =
    { ff_name; ff_type = `nested (nested, codec); ff_configs = configs; ff_annot = annot; ff_doc = doc }

  let variant_argument ?(doc = `nodoc) ?(configs=Configs.empty) ~annot ct =
    { fva_type = `direct ct; fva_configs = configs; fva_annot = annot; fva_doc = doc }

  let variant_argument_nested ?(doc = `nodoc) ?(configs=Configs.empty) ?(codec=`default) ~annot nested =
    { fva_type = `nested (nested, codec); fva_configs = configs; fva_annot = annot; fva_doc = doc }

  let bind :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> annot_d:'ann_d
    -> string
    -> ('ann_f, 'ann_va, 'ann_k) fwrt_desc_kind
    -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) t -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) t
    =
    fun ?(doc = `nodoc) ?parent ~annot_d name kind env ->
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
          fd_annot = annot_d;
          fd_doc = doc }
        env in
    match parent with
    | None -> add_new_type env
    | Some parent_name ->
      register_child parent_name name env
      |> add_new_type

  let compare_field (f1: _ fwrt_field_desc) (f2: _ fwrt_field_desc) =
    compare f1.ff_name f2.ff_name

  let bind_object ?doc ?parent ?(configs = Configs.empty) ~annot_d ~annot_ko name fields (env: ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) t)
    : ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) t =
    let kind =
      Fwrt_object {
        fo_fields = List.sort compare_field fields;
        fo_configs = configs;
        fo_children = [];
        fo_annot = annot_ko;
      }
    in
    bind ?doc ?parent ~annot_d:(annot_d kind) name kind env

  let bind_alias ?doc ?parent ?(configs=Configs.empty) ~annot_d ~annot_ka name ty env =
    let kind =
      Fwrt_alias {
        fa_type = ty;
        fa_configs = configs;
        fa_annot = annot_ka
      }
    in
    bind ?doc ?parent ~annot_d:(annot_d kind) name kind env

  let bind_constructor ?doc ?parent ?(configs=Configs.empty) ~annot_d ~annot_kc ?(args=[]) ?(fields=[]) name env =
    let kind =
      Fwrt_constructor {
        fc_args = args;
        fc_fields = List.sort compare_field fields;
        fc_configs = configs;
        fc_annot = annot_kc;
      }
    in
    bind ?doc ?parent ~annot_d:(annot_d kind) name kind env

  let lookup : string -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) t -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_desc =
    StringMap.find

  let lookup_opt : string -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) t -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_desc option  =
    StringMap.find_opt

  let bindings = StringMap.bindings

  let annotate_kind ann_f ann_va = function
    | Fwrt_alias a -> Fwrt_alias a
    | Fwrt_constructor c ->
      let fc_args =
        c.fc_args |&> (fun arg -> { arg with fva_annot = ann_va })
      in
      let fc_fields =
        c.fc_fields |> List.map (fun field -> { field with ff_annot = ann_f }) |> List.sort compare_field
      in
      Fwrt_constructor { c with fc_args; fc_fields }
    | Fwrt_object o ->
      let fo_fields =
        o.fo_fields |> List.map (fun field -> { field with ff_annot = ann_f }) |> List.sort compare_field
      in
      Fwrt_object { o with fo_fields }

  let annotate
    typ
    (ann_d_of_type, ann_d_of_other)
    (ann_f_of_typ, ann_f_of_other)
    (ann_va_of_typ, ann_va_of_other)
    (env: _ t) =
    env
    |> StringMap.mapi (fun name desc ->
      let ann_d, ann_f, ann_va =
        if name = typ then
          ann_d_of_type, ann_f_of_typ, ann_va_of_typ
        else
          ann_d_of_other, ann_f_of_other, ann_va_of_other
      in
      { desc with
          fd_kind = annotate_kind ann_f ann_va desc.fd_kind;
          fd_annot = ann_d; })

  let map :
    (('ann_d1, 'ann_f1, 'ann_va1, 'ann_k1) fwrt_desc -> ('ann_d2, 'ann_f2, 'ann_va2, 'ann_k2) fwrt_desc)
    -> ('ann_d1, 'ann_f1, 'ann_va1, 'ann_k1) t
    -> ('ann_d2, 'ann_f2, 'ann_va2, 'ann_k2) t
    = StringMap.map
end

module FwrtTypeEnv'(D : sig
  type annot_d
  type annot_f
  type annot_va
  type annot_ko
  type annot_ka
  type annot_kc

  val default_annot_d : annot_d
  val default_annot_f : annot_f
  val default_annot_va : annot_va
  val default_annot_ko : annot_ko
  val default_annot_ka : annot_ka
  val default_annot_kc : annot_kc

  val default_annot_d_f : (annot_f, annot_va, annot_ko*annot_ka*annot_kc) fwrt_desc_kind -> annot_d
end) = struct
  type t = (D.annot_d, D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_type_env

  let init : t = StringMap.empty

  let field ?doc ?configs ?(annot=D.default_annot_f) =
    FwrtTypeEnv.field ?doc ?configs ~annot

  let field_nested ?doc ?configs ?codec ?(annot=D.default_annot_f) =
    FwrtTypeEnv.field_nested ?doc ?configs ?codec ~annot

  let variant_argument ?doc ?configs ?(annot=D.default_annot_va) =
    FwrtTypeEnv.variant_argument ?doc ?configs ~annot

  let variant_argument_nested ?doc ?configs ?codec ?(annot=D.default_annot_va) =
    FwrtTypeEnv.variant_argument_nested ?doc ?configs ?codec ~annot

  let bind ?doc ?parent ?(annot_d=D.default_annot_d) =
    FwrtTypeEnv.bind ?doc ?parent ~annot_d

  let bind_object ?doc ?parent ?configs ?(annot_d=D.default_annot_d_f) ?(annot_ko=D.default_annot_ko) =
    FwrtTypeEnv.bind_object ?doc ?parent ?configs ~annot_d ~annot_ko

  let bind_alias ?doc ?parent ?configs ?(annot_d=D.default_annot_d_f) ?(annot_ka=D.default_annot_ka) =
    FwrtTypeEnv.bind_alias ?doc ?parent ?configs ~annot_d ~annot_ka

  let bind_constructor ?doc ?parent ?configs ?(annot_d=D.default_annot_d_f) ?(annot_kc=D.default_annot_kc) =
    FwrtTypeEnv.bind_constructor ?doc ?parent ?configs ~annot_d ~annot_kc

  let lookup : string -> t -> (D.annot_d, D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_desc =
    FwrtTypeEnv.lookup

  let lookup_opt : string -> t -> (D.annot_d, D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_desc option =
    FwrtTypeEnv.lookup_opt

  let bindings : t -> (string * (D.annot_d, D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_desc) list =
    FwrtTypeEnv.bindings

  let annotate :
    string
    -> (D.annot_d * D.annot_d)
    -> (D.annot_f * D.annot_f)
    -> (D.annot_va * D.annot_va)
    -> t
    -> t = FwrtTypeEnv.annotate
end

type ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_decl = string * ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_type_env

let pp_fwrt_decl :
  (ppf -> 'ann_d -> unit)
  -> (ppf -> 'ann_f -> unit)
  -> (ppf -> 'ann_va -> unit)
  -> (ppf -> 'ann_ko -> unit)
  -> (ppf -> 'ann_ka -> unit)
  -> (ppf -> 'ann_kc -> unit)
  -> ppf
  -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) fwrt_decl
  -> unit
  = fun ann_d ann_f ann_va ann_ko ann_ka ann_kc ppf (name, env) ->
  let fd = FwrtTypeEnv.lookup name env in
  pp_fwrt_desc ann_d ann_f ann_va ann_ko ann_ka ann_kc ppf fd

let show_fwrt_decl :
  (ppf -> 'ann_d -> unit)
  -> (ppf -> 'ann_f -> unit)
  -> (ppf -> 'ann_va -> unit)
  -> (ppf -> 'ann_ko -> unit)
  -> (ppf -> 'ann_ka -> unit)
  -> (ppf -> 'ann_kc -> unit)
  -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) fwrt_decl
  -> string
  = fun ann_d ann_f ann_va ann_ko ann_ka ann_kc (name, env) ->
  let fd = FwrtTypeEnv.lookup name env in
  show_fwrt_desc ann_d ann_f ann_va ann_ko ann_ka ann_kc fd

let equal_fwrt_decl :
  ('ann_d -> 'ann_d -> bool)
  -> ('ann_f -> 'ann_f -> bool)
  -> ('ann_va -> 'ann_va -> bool)
  -> ('ann_ko -> 'ann_ko -> bool)
  -> ('ann_ka -> 'ann_ka -> bool)
  -> ('ann_kc -> 'ann_kc -> bool)
  -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) fwrt_decl
  -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) fwrt_decl
  -> bool
  = fun ann_d ann_f ann_va ann_ko ann_ka ann_kc (name1, env1) (name2, env2) ->
  let fd1 = FwrtTypeEnv.lookup name1 env1 in
  let fd2 = FwrtTypeEnv.lookup name2 env2 in
  equal_fwrt_desc ann_d ann_f ann_va ann_ko ann_ka ann_kc fd1 fd2

type ('ann_d, 'ann_f, 'ann_va, 'ann_k) simple_annotator = {
  annotate_decl : type_decl -> ('ann_f, 'ann_va, 'ann_k) fwrt_desc_kind -> 'ann_d;
  annotate_kind_object :
    fields:('ann_f fwrt_field_desc list)
    -> children:(string list)
    -> configs:([`type_decl] configs)
    -> 'ann_ko;
  annotate_kind_alias :
    type_:coretype
    -> configs:([`type_decl] configs)
    -> 'ann_ka;
  annotate_kind_constructor :
    param:[
      | `no_param
      | `tuple_like of variant_tuple_argument list
      | `inline_record of record_field list
      | `reused_inline_record of type_decl
    ]
    -> configs:([`variant_constructor] configs)
    -> 'ann_kc;
  annotate_field :
    name:string
    -> type_:[`direct of coretype | `nested of type_decl * Coretype.codec]
    -> configs:([`record_field] configs)
    -> 'ann_f;
  annotate_variant_argument :
    type_:[`direct of coretype | `nested of type_decl * Coretype.codec]
    -> configs:([`variant_tuple_argument] configs)
    -> 'ann_va;
} constraint 'ann_k = 'ann_ko * 'ann_ka * 'ann_kc

let fwrt_decl_of_type_decl' :
  annotator:('ann_d, 'ann_f, 'ann_va, 'ann_k) simple_annotator
  -> type_decl
  -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_decl
  =
  let of_nested = function
    | `nested({ td_name; _ }, codec) -> `nested(td_name, codec)
    | (`direct _) as x -> x
  in
  fun
    ~annotator:(
    { annotate_decl;
      annotate_kind_object;
      annotate_kind_alias;
      annotate_kind_constructor;
      annotate_field;
      annotate_variant_argument; }) ->
    let conv_fields fields =
      fields |&> fun { rf_name; rf_type; rf_configs; rf_doc; } ->
      { ff_name = rf_name;
        ff_type = of_nested rf_type;
        ff_configs = rf_configs;
        ff_annot = (annotate_field ~name:rf_name ~type_:rf_type ~configs:rf_configs);
        ff_doc = rf_doc
      }
    in
    let rec go env decl =
      let fold_nested_types accessor =
        Fn.flip (List.fold_left (fun env x ->
          match accessor x with
          | `nested (td, _) -> go env td
          | _ -> env
        ))
      in
      let { td_name; td_kind; td_configs = configs; td_doc = doc } = decl in
      match td_kind with
      | Alias_decl typ ->
        env
        |> FwrtTypeEnv.bind_alias
          ~doc ~annot_d:(annotate_decl decl)
          ~annot_ka:(annotate_kind_alias ~type_:typ ~configs)
          ~configs td_name typ
      | Record_decl fields ->
        env
        |> begin
          let fields = conv_fields fields in
          FwrtTypeEnv.bind_object
            ~doc ~annot_d:(annotate_decl decl)
            ~annot_ko:(annotate_kind_object ~fields ~children:[] ~configs)
            ~configs td_name fields
        end
        |> fold_nested_types (fun f -> f.rf_type) fields
      | Variant_decl ctors ->
        let add_ctor parent acc ctor =
          let { vc_name; vc_param; vc_configs=configs; vc_doc=doc } = ctor in
          let args, fields =
            match vc_param with
            | `no_param -> [], []
            | `tuple_like args -> (args |&> fun { va_type; va_configs; va_doc } -> {
              fva_type = of_nested va_type;
              fva_configs = va_configs;
              fva_annot = (annotate_variant_argument ~type_:va_type ~configs:va_configs);
              fva_doc = va_doc;
            }), []
            | `inline_record fields ->
              let fields = conv_fields fields in
              [], fields
            | `reused_inline_record decl ->
              let fields = decl.td_kind |> function
                | Record_decl fields -> conv_fields fields
                | _ -> failwith' "panic - type decl of reused inline record '%s' muts be record decl." vc_name
              in
              [], fields
          in
          acc
          |> FwrtTypeEnv.bind_constructor ~doc ~parent ~configs
            ~annot_d:(annotate_decl decl)
            ~annot_kc:(annotate_kind_constructor ~param:vc_param ~configs)
            ~args ~fields vc_name
        in
        env
        |> FwrtTypeEnv.bind_object ~doc ~annot_d:(annotate_decl decl)
          ~annot_ko:(annotate_kind_object ~fields:[] ~children:(ctors |&> (fun { vc_name; _ } -> vc_name)) ~configs)
          ~configs td_name []
        |> Fn.flip (List.fold_left (add_ctor td_name)) ctors
        |> Fn.flip (List.fold_left (fun env vc ->
          match vc.vc_param with
          | `tuple_like args ->
            fold_nested_types (fun a -> a.va_type) args env
          | `inline_record fields
          | `reused_inline_record { td_kind = Record_decl fields; _ } ->
            fold_nested_types (fun f -> f.rf_type) fields env
          | `reused_inline_record _ ->
            failwith' "type decl of reused inline record '%s' must be record decl." vc.vc_name
          | _ -> env
        )) ctors
    in
    fun decl -> decl.td_name, go FwrtTypeEnv.init decl

let fwrt_decl_of_type_decl : type_decl -> (unit, unit, unit, unit*unit*unit) fwrt_decl =
  fun decl ->
    fwrt_decl_of_type_decl'
      ~annotator:{
        annotate_decl = (fun _ _ -> ());
        annotate_kind_object = (fun ~fields:_ ~children:_ ~configs:_ -> ());
        annotate_kind_alias = (fun ~type_:_ ~configs:_ -> ());
        annotate_kind_constructor = (fun ~param:_ ~configs:_ -> ());
        annotate_field = (fun ~name:_ ~type_:_ ~configs:_ -> ());
        annotate_variant_argument = (fun ~type_:_ ~configs:_ -> ())
      } decl
