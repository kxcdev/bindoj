open Ppxlib

module Gu = Gen_lib.Utils

let loc = Location.none

let () =
  let postfix = Gu.sprintf " from %s" __FILE__ in
  Pprintast.structure Format.std_formatter
    [%str let greeting x = "Hello "^x^[%e Ast_builder.Default.(estring ~loc postfix)]]
