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
module Versioned = struct
  module V0 = struct
    module Type_desc = Bindoj_base.Type_desc

    open struct
      let loc = Location.none
      let mknoloc = Location.mknoloc
      let lid str = Ppxlib.Longident.parse str |> mknoloc
    end

    module Caml = struct
      module Ppxlib = Ppxlib
      module Astlib = Astlib
      module Emitter = Bindoj_gen.Emitter

      module CommonTypes = struct
        type signature_item = Ppxlib.signature_item
        type signature = Ppxlib.signature

        type structure_item = Ppxlib.structure_item
        type structure = structure_item list

        type value_binding = Ppxlib.value_binding
        type type_declaration = Ppxlib.type_declaration
        type core_type = Ppxlib.core_type
      end
      include CommonTypes

      module Structure = struct
        type elt = structure_item
        type t = structure
        type rec_flag = Ppxlib.rec_flag
        open Ppxlib.Ast_builder.Default

        let binding : ?rec_flag:rec_flag -> value_binding -> structure_item =
          fun ?rec_flag:(rf=Nonrecursive) item ->
          pstr_value ~loc rf [item]

        let declaration : ?rec_flag:rec_flag -> type_declaration -> structure_item =
          fun ?rec_flag:(rf=Recursive) item ->
          pstr_type ~loc rf [item]

        let type_alias : string -> core_type -> structure_item =
          fun name core_type ->
          let open Ppxlib_ast.Ast_helper in
          pstr_type ~loc Nonrecursive [Type.mk (mknoloc name) ~manifest:core_type]

        let modul : string -> structure -> structure_item =
          fun module_name items ->
          let open Ppxlib_ast.Ast_helper in
          Mod.structure items |> Mb.mk (module_name |> mknoloc %some)
          |> Str.module_

        let modul' : string -> structure list -> structure_item =
          modul %% List.flatten

        let open_utils : structure -> structure_item =
          let open Ppxlib_ast.Ast_helper in
          Str.open_ % Opn.mk % Mod.structure

        let open_utils' : structure list -> structure_item =
          open_utils % List.flatten

        let pp_caml : Format.formatter -> t -> unit = Emitter.structure
      end

      module Signature = struct
        type elt = signature_item
        type t = signature

        let pp_caml : Format.formatter -> t -> unit = Emitter.signature
      end

    end

    module Caml_gen = struct
      open Ppxlib_ast.Ast_helper
      open Caml.CommonTypes

      module Caml = Caml
      module Datatype = Bindoj_gen.Caml_datatype
      module Json_codec = Bindoj_gen.Json_codec

      module Type_module = struct
        let datatype_module'
              ?module_name
              ?(gen_json_codec=false)
              type_decl
            : structure_item*core_type =
          let open Caml.Structure in
          let ty = Datatype.type_declaration_of_type_decl ~type_name:"t" type_decl in
          let module_name = module_name |? String.capitalize_ascii type_decl.td_name in
          let may_append cond gfunc (items : structure) =
            if cond then items @ (gfunc()) else items in
          let tyref = Typ.constr (lid (module_name^".t")) [] in
          let items =
            [ (* type t = ... *)
              declaration ty;

              ( (* open struct type [td_name] = t end *)
                [ declaration
                   (Type.mk ~manifest:(Typ.constr (lid "t") [])
                      (mknoloc type_decl.td_name)) ]
                |> Caml.Structure.open_utils);
            ]
            |> may_append gen_json_codec Json_codec.(fun() ->
                let codec = `in_module module_name in
                let add_item x xs = x :: xs in
                [ gen_json_shape_explanation type_decl ~codec |> binding;
                  gen_json_encoder type_decl ~codec |> binding;
                  gen_json_decoder_result type_decl
                    ~json_shape_explanation_style:(`reference)
                    ~codec |> binding;
                  gen_json_decoder_option type_decl ~codec |> binding;
                  [%stri let jv_codec = to_json, of_json']; ]
                |> (match type_decl.td_kind with
                  | Variant_decl _ -> add_item (gen_discriminator_value_accessor ~codec type_decl |> binding)
                  | _ -> identity))
          in
          let modul =
            Mod.structure items
            |> Mb.mk (module_name |> mknoloc % some)
            |> Str.module_ in
          modul, tyref
        let datatype_module
              ?module_name
              ?gen_json_codec
              type_decl
            : structure_item =
          datatype_module' ?module_name ?gen_json_codec type_decl |> fst
        let datatype_module_and_binding
              ?module_name
              ?gen_json_codec
              ?(binding_name:string option)
              type_decl
            : structure =
          let modul, tyref = datatype_module' ?module_name ?gen_json_codec type_decl in
          let name = binding_name |? type_decl.td_name in
          [ modul; Caml.Structure.type_alias name tyref ]
      end
    end

    module TypeScript_gen = struct
      module Datatype = Bindoj_gen_ts.Typescript_datatype
    end
  end
end

include Versioned.V0
