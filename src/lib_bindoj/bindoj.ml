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

module Versioned = struct
  module V0 = struct
    module Type_desc = Bindoj_base.Type_desc

    module Caml = struct
      module Ppxlib = Ppxlib
      module Astlib = Astlib
      module Pprintast = Astlib.Pprintast

      module CommonTypes = struct
        type structure_item = Ppxlib.structure_item
        type structure = structure_item list
        type value_binding = Ppxlib.value_binding
        type type_declaration = Ppxlib.type_declaration
      end
      include CommonTypes

      module Structure = struct
        type elt = structure_item
        type t = structure
        type rec_flag = Ppxlib.rec_flag
        open Ppxlib.Ast_builder.Default
        open struct
          let loc = Location.none
        end

        let binding : ?rec_flag:rec_flag -> value_binding -> structure_item =
          fun ?rec_flag:(rf=Nonrecursive) item ->
          pstr_value ~loc rf [item]

        let declaration : ?rec_flag:rec_flag -> type_declaration -> structure_item =
          fun ?rec_flag:(rf=Recursive) item ->
          pstr_type ~loc rf [item]

        let pp_caml : Format.formatter -> t -> unit = Pprintast.structure
      end
    end

    module Caml_gen = struct
      module Caml = Caml
      module Datatype = Bindoj_gen.Caml_datatype
      module Json_codec = Bindoj_gen.Json_codec
    end

    module TypeScript_gen = struct
      module Datatype = Bindoj_gen_ts.Typescript_datatype
    end
  end
end

include Versioned.V0
