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
open Bindoj_base
open Typed_type_desc

type method_bundle_caml_style = [ `module_ | `object_ ]

type ('pos, 'kind) config +=
  | Config_objintf_caml_style :
    method_bundle_caml_style -> ([ `method_bundle_brideable_decl ], [ `objintf_caml_style ]) config
  | Config_idents_of_type_decls :
    type_decl list -> ([ `coretype ], [`idents_of_type_decls]) config
    (** Specifying this [config] when creating [Bindoj_typedesc.Type_desc.Ident]
        for [Bindoj_typedesc.Type_desc.type_decl] will automatically output
        an encoder/decoder for [Bindoj_typedesc.Type_desc.type_decl].

        {3 Example}
        {[
          Coretype.(mk_list
            ~configs:[
              Objintf_config.ident_of_type_decls [ student; teacher ]
            ]
            & tuple [
              ident student.td_name;
              ident teacher.id_name;
              ident "custom_type"; ])
        ]}
     *)

val default_caml_style : method_bundle_caml_style
val caml_style : method_bundle_caml_style -> ([ `method_bundle_brideable_decl ], [ `objintf_caml_style ]) config
val get_caml_style : [ `method_bundle_brideable_decl ] configs -> method_bundle_caml_style

val ident_of_type_decls : type_decl list -> ([ `coretype ], [`idents_of_type_decls]) config
val get_ident_of_type_decls : [ `coretype ] configs -> type_decl list
