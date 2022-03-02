open Ppxlib

let loc = Location.none

let () =
  Pprintast.structure Format.std_formatter
    [%str let () = print_endline "hello ast"]
