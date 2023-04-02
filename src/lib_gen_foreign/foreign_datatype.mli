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

type ('ann0, 'ann1) fwrt_desc = {
  fd_name : string;
  fd_parent : string option;
  fd_kind : 'ann1 fwrt_desc_kind;
  fd_annot : 'ann0;
  fd_doc : doc;
}

and 'ann fwrt_desc_kind =
  | Fwrt_object of {
      fo_fields: 'ann fwrt_field_desc list;
      fo_children : string list;
      fo_configs: [`type_decl] configs
    }
  | Fwrt_alias of {
      fa_type: coretype;
      fa_configs: [`type_decl] configs
    }
  | Fwrt_constructor of {
      fc_args: coretype list;
      fc_fields: 'ann fwrt_field_desc list;
      fc_configs: [`variant_constructor] configs
    }

and 'ann fwrt_field_desc = {
  ff_name : string;
  ff_type : coretype;
  ff_configs: [`record_field] configs;
  ff_annot : 'ann;
  ff_doc : doc;
}

module FwrtTypeEnv : sig
  type ('ann0, 'ann1) t

  val init : ('ann0, 'ann1) t

  val field :
       ?doc:([`docstr of string | `nodoc])
    -> ?configs:[`record_field] configs
    -> annot:'ann0
    -> string
    -> coretype
    -> 'ann0 fwrt_field_desc

  val bind :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> annot:'ann0
    -> string
    -> 'ann1 fwrt_desc_kind
    -> ('ann0, 'ann1) t -> ('ann0, 'ann1) t

  val bind_object :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> ?configs:[`type_decl] configs
    -> annot:'ann0
    -> string
    -> 'ann1 fwrt_field_desc list
    -> ('ann0, 'ann1) t -> ('ann0, 'ann1) t

  val bind_alias :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> ?configs:[`type_decl] configs
    -> annot:'ann0
    -> string
    -> coretype
    -> ('ann0, 'ann1) t -> ('ann0, 'ann1) t

  val bind_constructor :
       ?doc:([`docstr of string | `nodoc])
    -> ?parent:string
    -> ?configs:[`variant_constructor] configs
    -> annot:'ann0
    -> ?args:coretype list
    -> ?fields:'ann1 fwrt_field_desc list
    -> string
    -> ('ann0, 'ann1) t -> ('ann0, 'ann1) t

  val lookup : string -> ('ann0, 'ann1) t -> ('ann0, 'ann1) fwrt_desc

  val lookup_opt : string -> ('ann0, 'ann1) t -> ('ann0, 'ann1) fwrt_desc option

  val annotate : string -> ('ann0 * 'ann0) -> ('ann1 * 'ann1) -> (_, _) t -> ('ann0, 'ann1) t

  val bindings : ('ann0, 'ann1) t -> (string * ('ann0, 'ann1) fwrt_desc) list
end

type ('ann0, 'ann1) fwrt_type_env = ('ann0, 'ann1) FwrtTypeEnv.t

type ('ann0, 'ann1) fwrt_decl = string * ('ann0, 'ann1) fwrt_type_env

val pp_fwrt_decl : (ppf->'a->unit) -> (ppf->'b->unit) -> ppf -> ('a, 'b) fwrt_decl -> unit
val show_fwrt_decl : (ppf->'a->unit) -> (ppf->'b->unit) -> ('a, 'b) fwrt_decl -> string
val equal_fwrt_decl : ('a->'a->bool) -> ('b->'b->bool) -> ('a, 'b) fwrt_decl -> ('a, 'b) fwrt_decl -> bool

val fwrt_decl_of_type_decl : type_decl -> (unit, unit) fwrt_decl
