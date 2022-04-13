open Ppxlib
open Ast_builder.Default

let locmk = Located.mk
let lidloc ~loc x = locmk ~loc (lident x)
let typcons ~loc x = ptyp_constr ~loc (lidloc ~loc x) []

let doc_attributes = function
  | `docstr doc ->
    let loc = Location.none in
    [attribute ~loc
       ~name:(Located.mk ~loc "ocaml.doc")
       ~payload:(PStr [pstr_eval ~loc (estring ~loc doc) []])]
  | `nodoc -> []
  | _ -> failwith "unknown polymorphic variant for docstr"
let warning_attributes str =
  let loc = Location.none in
  [attribute ~loc
     ~name:(Located.mk ~loc "warning")
     ~payload:(PStr [pstr_eval ~loc (estring ~loc str) []])]

let sprintf fmt = Format.asprintf fmt
