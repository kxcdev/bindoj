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
open Bindoj_typedesc.Type_desc
open Bindoj_objintf_shared
open Utils

open struct
  let cty_unit = Coretype.mk_prim `unit
  let cty_int = Coretype.mk_prim `int
  let cty_string = Coretype.mk_prim `string
  let cty_bytes = Coretype.mk_prim `bytes

  let byte_source_state = variant_decl "byte_source_state" [
      variant_constructor "data" (`tuple_like [
        variant_argument cty_bytes;
        variant_argument (Coretype.(mk_string_enum [ string_enum_case "eof"; string_enum_case "maybe_more" ]))
      ]);
      variant_constructor "wait" `no_param;
      variant_constructor "eof" `no_param;
    ] ~configs:[ Caml_config.variant_type `polymorphic ]

end

let caml_resolution_strategy td _ =
  match td with
  | { td_kind = Alias_decl _; _} -> `inline_type_definition
  | ({ td_kind = Variant_decl _; td_configs; _ })
    when Caml_config.get_variant_type td_configs = `polymorphic ->
      `inline_type_definition
  |_ -> `no_resolution

let ts_type_decl_resolution_strategy _ _ = `inline_type_definition
let ts_ident_resolution_strategy _ = failwith "unexpected ident"

type bridgeable_ident = [
  | `byte_source'
  | `logger
  | `byte_source
  | `output_channel
  | `system_io
]

module Bridgeable_decls = struct
  let byte_source' =
    simple_bridgeable & sole_method_bridgeable &
      simple_method "byte_source'"
        ~largs:[
          labeled_argument_regular ~optional:true "max" (`direct cty_int)
            ~doc:(doc "max argument.");
        ]
        ~pargs:[
          positional_argument_regular (`direct cty_unit)
            ~doc:(doc "argument at 0.");
        ]
        (method_return_type_regular & `nested (byte_source_state, `default))
        ~doc:(doc "byte_source' bridgeable")

  let logger =
    simple_bridgeable & method_bundle_bridgeable "Logger" [
        simple_method "info"
          ~pargs:[
            positional_argument_regular (`direct cty_string)
              ~doc:(doc "argument at 0.");
          ]
          (method_return_type_regular (`direct cty_unit))
          ~doc:(doc "info method.");
        simple_method "error"
          ~largs:[
            labeled_argument_regular ~optional:true "exn" (`direct cty_string)
              ~doc:(doc "exn argument.");
          ]
          ~pargs:[
            positional_argument_regular (`direct cty_string)
            ~doc:(doc "argument at 0.");
          ]
          (method_return_type_regular (`direct cty_unit))
          ~doc:(doc "error method.");
      ] ~configs:[
        Objintf_config.caml_style `module_
      ] ~doc:(doc "Logger bridgeable.")

  let byte_source =
    simple_bridgeable & method_bundle_bridgeable "byte_source" [
        simple_method "bytes_left"
          ~pargs:[
            positional_argument_regular (`direct cty_unit)
              ~doc:(doc "argument at 0.");
          ]
          (method_return_type_regular (`direct cty_int))
          ~doc:(doc "bytes_left method.");
        simple_method "next_block"
          ~largs:[
            labeled_argument_regular ~optional:true "max" (`direct cty_int)
              ~doc:(doc "max argument.");
          ]
          ~pargs:[
            positional_argument_regular (`direct cty_unit)
            ~doc:(doc "argument at 0.");
          ]
          (method_return_type_regular (`direct cty_bytes))
          ~doc:(doc "next_block method.");
      ] ~configs:[
        Objintf_config.caml_style `object_
      ] ~doc:(doc "byte_source bridgeable.")

  let output_channel =
    complex_bridgeable & method_bundle_bridgeable "output_channel" [
      simple_method "channel_name"
        (method_return_type_regular (`direct cty_string))
        ~doc:(doc "channel_name method.");
      simple_method "write"
        ~pargs: [ positional_argument_regular (`direct cty_bytes) ]
        (method_return_type_regular (`direct cty_unit))
        ~doc:(doc "write method.");
      complex_method "write_bulk"
        ~pargs:[
          positional_argument_regular (`bridgeable (`endemic, `byte_source))
            ~doc:(doc "argument at 0.");
        ]
        (method_return_type_regular (`direct cty_unit))
        ~doc:(doc "write_bulk method.");
      complex_method "write_async"
        ~pargs:[
          positional_argument_regular (`bridgeable (`endemic, `byte_source'))
            ~doc:(doc "argument at 0.");
        ]
        (method_return_type_regular (`direct cty_unit))
        ~doc:(doc "write_async method.");
    ] ~configs:[
      Objintf_config.caml_style `object_
    ] ~doc:(doc "output_channel bridgeable.")

  let system_io =
    complex_bridgeable & method_bundle_bridgeable "System_io" [
      complex_method "stdout"
        (method_return_type_regular & `bridgeable (`peer, `output_channel))
        ~doc:(doc "stdout method");
      complex_method "stderr"
        (method_return_type_regular & `bridgeable (`peer, `output_channel))
        ~doc:(doc "stderr method");
      complex_method "open_file_wo"
        ~largs:[
          labeled_argument_regular ~optional:false "path" (`direct cty_string)
          ~doc:(doc "path argument");
        ]
        (method_return_type_regular & `bridgeable (`peer, `output_channel))
        ~doc:(doc "open_file_wo method");
      complex_method "open_file_ro"
        ~largs:[
          labeled_argument_regular ~optional:false "path" (`direct cty_string)
            ~doc:(doc "path argument");
        ]
        (method_return_type_regular & `bridgeable (`peer, `byte_source))
        ~doc:(doc "open_file_ro method");
    ] ~configs:[
      Objintf_config.caml_style `module_
    ] ~doc:(doc "System_io bridgeable")
end

let bridgeable_ident_resolver = Bridgeable_decls.(function
  | `byte_source' -> byte_source'
  | `logger -> logger
  | `byte_source -> byte_source
  | `output_channel -> output_channel
  | `system_io -> system_io)

let module_name = "Ex_objintf_system_io"

let objintf_decl polarity: bridgeable_ident sameworld_objintf =
  sameworld_objintf ~name:module_name ~polarity Bridgeable_decls.[
    byte_source';
    logger;
    byte_source;
    output_channel;
    system_io;
  ] ~named_objects:[
    named_object_decl "system_io"
      ~party:`peer
      ~typ:(`bridgeable(`peer, `system_io))
      ~doc:(doc "system_io object");
    named_object_decl "my_logger"
      ~party:`endemic
      ~typ:(`bridgeable(`endemic, `logger))
      ~doc:(doc "my_logger object");
  ] ~object_registries:[
    object_registry_decl "logger"
      ~coordinate_desc:[
        "name", `prim `string;
        "variant", `string_enum Coretype.[
          string_enum_case "persistent";
          string_enum_case "transient";
        ]
      ]
      ~party:`peer
      ~typ:(`bridgeable(`peer, `logger))
      ~doc:(doc "logger registry");
    object_registry_decl "logger"
      ~coordinate_desc:[
        "id", `prim `string
      ]
      ~party:`endemic
      ~typ:(`bridgeable(`endemic, `logger))
      ~doc:(doc "logger registry");
  ] ~doc:(doc & module_name ^ " objintf")
