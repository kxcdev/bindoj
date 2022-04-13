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
