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
open Bindoj_typedesc.Type_desc

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

and 'ann fwrt_field_desc = {
  ff_name : string;
  ff_type : [ `direct of coretype | `nested of string * Coretype.codec ];
  ff_configs: [`record_field] configs;
  ff_annot : 'ann;
  ff_doc : doc;
}
and 'ann fwrt_variant_argument_desc = {
  fva_type : [ `direct of coretype | `nested of string * Coretype.codec ];
  fva_configs : [`variant_tuple_argument] configs;
  fva_annot : 'ann;
  fva_doc : doc;
}

type ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_type_env
constraint 'ann_k = _*_*_

module FwrtTypeEnv : sig
  type ('ann_d, 'ann_f, 'ann_va, 'ann_k) t = ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_type_env

  val init : ('ann_d, 'ann_f, 'ann_va, 'ann_k) t

  val field :
       ?doc:([`docstr of string | `nodoc])
    -> ?configs:[`record_field] configs
    -> annot:'ann_f
    -> string
    -> coretype
    -> 'ann_f fwrt_field_desc

  val field_nested :
       ?doc:([`docstr of string | `nodoc])
    -> ?configs:[`record_field] configs
    -> ?codec:Coretype.codec
    -> annot:'ann_f
    -> string
    -> string
    -> 'ann_f fwrt_field_desc

  val variant_argument :
       ?doc:([`docstr of string | `nodoc])
    -> ?configs:[`variant_tuple_argument] configs
    -> annot:'ann_va
    -> coretype
    -> 'ann_va fwrt_variant_argument_desc

  val variant_argument_nested :
       ?doc:([`docstr of string | `nodoc])
    -> ?configs:[`variant_tuple_argument] configs
    -> ?codec:Coretype.codec
    -> annot:'ann_va
    -> string
    -> 'ann_va fwrt_variant_argument_desc

  val bind :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> annot_d:'ann_d
    -> string
    -> ('ann_f, 'ann_va, 'ann_k) fwrt_desc_kind
    -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) t -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) t

  val bind_object :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> ?configs:[`type_decl] configs
    -> annot_d:(('ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) fwrt_desc_kind -> 'ann_d)
    -> annot_ko:'ann_ko
    -> string
    -> 'ann_f fwrt_field_desc list
    -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) t -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) t

  val bind_alias :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> ?configs:[`type_decl] configs
    -> annot_d:(('ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) fwrt_desc_kind -> 'ann_d)
    -> annot_ka:'ann_ka
    -> string
    -> coretype
    -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) t -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) t

  val bind_constructor :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> ?configs:[`variant_constructor] configs
    -> annot_d:(('ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) fwrt_desc_kind -> 'ann_d)
    -> annot_kc:'ann_kc
    -> ?args:'ann_va fwrt_variant_argument_desc list
    -> ?fields:'ann_f fwrt_field_desc list
    -> string
    -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) t -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) t

  val lookup : string -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) t -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_desc

  val lookup_opt : string -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) t -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_desc option

  val annotate :
    string
    -> ('ann_d * 'ann_d)
    -> ('ann_f * 'ann_f)
    -> ('ann_va * 'ann_va)
    -> (_, _, _, 'ann_k) t
    -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) t

  val bindings : ('ann_d, 'ann_f, 'ann_va, 'ann_k) t -> (string * ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_desc) list

  val map :
    (('ann_d1, 'ann_f1, 'ann_va1, 'ann_k1) fwrt_desc -> ('ann_d2, 'ann_f2, 'ann_va2, 'ann_k2) fwrt_desc)
    -> ('ann_d1, 'ann_f1, 'ann_va1, 'ann_k1) t
    -> ('ann_d2, 'ann_f2, 'ann_va2, 'ann_k2) t
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
end) : sig
  type t = (D.annot_d, D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_type_env

  val init : t

  val field :
       ?doc:([`docstr of string | `nodoc])
    -> ?configs:[`record_field] configs
    -> ?annot:D.annot_f
    -> string
    -> coretype
    -> D.annot_f fwrt_field_desc

  val field_nested :
       ?doc:([`docstr of string | `nodoc])
    -> ?configs:[`record_field] configs
    -> ?codec:Coretype.codec
    -> ?annot:D.annot_f
    -> string
    -> string
    -> D.annot_f fwrt_field_desc

  val variant_argument :
       ?doc:([`docstr of string | `nodoc])
    -> ?configs:[`variant_tuple_argument] configs
    -> ?annot:D.annot_va
    -> coretype
    -> D.annot_va fwrt_variant_argument_desc

  val variant_argument_nested :
       ?doc:([`docstr of string | `nodoc])
    -> ?configs:[`variant_tuple_argument] configs
    -> ?codec:Coretype.codec
    -> ?annot:D.annot_va
    -> string
    -> D.annot_va fwrt_variant_argument_desc

  val bind :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> ?annot_d:D.annot_d
    -> string
    -> (D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_desc_kind
    -> t -> t

  val bind_object :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> ?configs:[`type_decl] configs
    -> ?annot_d:((D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_desc_kind -> D.annot_d)
    -> ?annot_ko:D.annot_ko
    -> string
    -> D.annot_f fwrt_field_desc list
    -> t -> t

  val bind_alias :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> ?configs:[`type_decl] configs
    -> ?annot_d:((D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_desc_kind -> D.annot_d)
    -> ?annot_ka:D.annot_ka
    -> string
    -> coretype
    -> t -> t

  val bind_constructor :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> ?configs:[`variant_constructor] configs
    -> ?annot_d:((D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_desc_kind -> D.annot_d)
    -> ?annot_kc:D.annot_kc
    -> ?args:D.annot_va fwrt_variant_argument_desc list
    -> ?fields:D.annot_f fwrt_field_desc list
    -> string
    -> t -> t

  val lookup : string -> t -> (D.annot_d, D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_desc

  val lookup_opt : string -> t -> (D.annot_d, D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_desc option

  val annotate :
    string
    -> (D.annot_d * D.annot_d)
    -> (D.annot_f * D.annot_f)
    -> (D.annot_va * D.annot_va)
    -> t
    -> t

  val bindings : t -> (string * (D.annot_d, D.annot_f, D.annot_va, D.annot_ko*D.annot_ka*D.annot_kc) fwrt_desc) list
end

type ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_decl = string * ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_type_env

val pp_fwrt_decl :
  (ppf -> 'ann_d -> unit)
  -> (ppf -> 'ann_f -> unit)
  -> (ppf -> 'ann_va -> unit)
  -> (ppf -> 'ann_ko -> unit)
  -> (ppf -> 'ann_ka -> unit)
  -> (ppf -> 'ann_kc -> unit)
  -> ppf
  -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) fwrt_decl
  -> unit

val show_fwrt_decl :
  (ppf -> 'ann_d -> unit)
  -> (ppf -> 'ann_f -> unit)
  -> (ppf -> 'ann_va -> unit)
  -> (ppf -> 'ann_ko -> unit)
  -> (ppf -> 'ann_ka -> unit)
  -> (ppf -> 'ann_kc -> unit)
  -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) fwrt_decl
  -> string

val equal_fwrt_decl :
  ('ann_d -> 'ann_d -> bool)
  -> ('ann_f -> 'ann_f -> bool)
  -> ('ann_va -> 'ann_va -> bool)
  -> ('ann_ko -> 'ann_ko -> bool)
  -> ('ann_ka -> 'ann_ka -> bool)
  -> ('ann_kc -> 'ann_kc -> bool)
  -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) fwrt_decl
  -> ('ann_d, 'ann_f, 'ann_va, 'ann_ko*'ann_ka*'ann_kc) fwrt_decl
  -> bool

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
    -> type_:[ `direct of coretype | `nested of type_decl * Coretype.codec ]
    -> configs:([`record_field] configs)
    -> 'ann_f;
  annotate_variant_argument :
    type_:[ `direct of coretype | `nested of type_decl * Coretype.codec ]
    -> configs:([`variant_tuple_argument] configs)
    -> 'ann_va;
} constraint 'ann_k = 'ann_ko * 'ann_ka * 'ann_kc

val fwrt_decl_of_type_decl' :
  annotator:('ann_d, 'ann_f, 'ann_va, 'ann_k) simple_annotator
  -> type_decl -> ('ann_d, 'ann_f, 'ann_va, 'ann_k) fwrt_decl

val fwrt_decl_of_type_decl : type_decl -> (unit, unit, unit, unit*unit*unit) fwrt_decl
