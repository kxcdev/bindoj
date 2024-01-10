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
open Ppxlib
open Ast_helper
open Ast_builder.Default
open Bindoj_common
open Bindoj_typedesc.Typed_type_desc
open Bindoj_objintf_gen_utils
open Bindoj_ppxlib_utils
open Bindoj_gen_config
open Bindoj_gen
open Bindoj_objintf_shared
open Bindoj_objintf_gen.Caml_bridge
open Bindoj_objintf_gen_jsoo_config

module Bridge_labels = struct
  include Bindoj_objintf_gen.Caml_bridge.Bridge_labels

  module Js = Bindoj_objintf_shared_js.Bridge_labels

  let unwrap_endemic = "unwrap_endemic"
  let wrap_peer = "wrap_peer"

  module Codec = struct
    let get_encoder_name ?(codec=`default) name =
      match codec with
      | `default -> None, sprintf "encode_%s_to_js" name
      | `open_ m -> Some m, sprintf "encode_%s_to_js" name
      | `in_module m -> None, sprintf "%s.encode_to_js" m
    let get_decoder_name ?(codec=`default) name =
      match codec with
      | `default -> None, sprintf "decode_%s_of_js" name
      | `open_ m -> Some m, sprintf "decode_%s_of_js" name
      | `in_module m -> None, sprintf "%s.decode_of_js" m
  end
end

let e_js_callback ?(loc=Location.none) ident args body =
  match args with
  | [] -> 
    [%expr ([%e ident]).callback (fun () -> [%e body])]
  | [ _ ] ->
    [%expr ([%e ident]).callback [%e efun args body]]
  | _ ->
    [%expr ([%e ident]).callback_with_arity [%e eint ~loc (List.length args)] [%e efun args body]]

let mk_js_object fields =
  let loc = Location.none in
  [%expr Js_of_ocaml.Js.(
    Unsafe.obj [%e
    fields
    |&> (function
      | `method_ (label, args, body) ->
        Exp.tuple [
          estring ~loc label;
          [%expr Unsafe.inject [%e e_js_callback [%expr Unsafe] args body]]
        ]
      | `val_ (label, body) ->
        Exp.tuple [ estring ~loc label; [%expr Unsafe.inject [%e body]] ]
      ) |> Exp.array
    ]
  )]

let ejs_unsafe_get ~loc label body =
  [%expr Js.Unsafe.get [%e body] [%e label]]

module Builtin_codecs = struct
  open struct
    let loc = Location.none
  end

  let unit = {
    encoder = [%expr fun () -> Js_of_ocaml.Js.null];
    decoder = [%expr fun _ -> ()];
  }

  let bool = {
    encoder = [%expr Js_of_ocaml.Js.bool];
    decoder = [%expr Js_of_ocaml.Js.to_bool];
  }

  let int = {
    encoder = [%expr float_of_int];
    decoder = [%expr fun jv ->
      if Float.is_integer jv then (int_of_float jv)
      else Format.kasprintf failwith "expecting an integer but the given is '%f'" jv];
  }

  let int53p = {
    encoder = [%expr Kxclib.Int53p.to_float];
    decoder = [%expr Kxclib.Int53p.of_float];
  }

  let float = {
    encoder = [%expr fun x -> x];
    decoder = [%expr fun x -> x];
  }

  let string = {
    encoder = [%expr Js_of_ocaml.Js.string];
    decoder = [%expr Js_of_ocaml.Js.to_string];
  }

  let uchar = {
    encoder = [%expr fun x ->
      Js_of_ocaml.Js.string (String.of_seq (List.to_seq [Uchar.to_char x]))];
    decoder = [%expr fun x ->
      let x = Js_of_ocaml.Js.to_string x in
      if String.length x = 1 then Uchar.of_char (String.get x 0)
      else Format.kasprintf failwith "string '%s' is not a valid uchar value" x];
  }

  let byte = {
    encoder = [%expr fun x -> float_of_int (int_of_char x)];
    decoder = [%expr fun jv ->
      let x = int_of_float jv in
      if 0 <= x && x <= 255 then char_of_int x
      else Format.kasprintf failwith "number '%d' is not a valid byte value" x];
  }

  let bytes = {
    encoder = [%expr
      Js_of_ocaml.(
        let encoder = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "TextEncoder") [||] in
            fun bytes ->
              Js.Unsafe.meth_call encoder "encode"
                [| Js.Unsafe.inject (Js.string (Bytes.to_string bytes)) |]
      )];
    decoder = [%expr
      Js_of_ocaml.(
        let decoder = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "TextDecoder") [||] in
        fun jv ->
          Js.Unsafe.meth_call decoder "decode" [| jv |]
          |> Js.to_string
          |> Bytes.of_string
      )];
  }

  let option = {
    encoder = [%expr fun js_of_t x ->
      Js_of_ocaml.Js.Opt.(map (option x) js_of_t)];
    decoder = [%expr fun js_to_t jv ->
      Js_of_ocaml.Js.Opt.to_option jv |> Option.map js_to_t];
  }

  let list = {
    encoder = [%expr fun js_of_t xs ->
        Array.of_list xs
        |> Array.map js_of_t
        |> Js_of_ocaml.Js.array
      ];
    decoder = [%expr fun js_to_t jv ->
      jv |> Js_of_ocaml.Js.to_array |> Array.map js_to_t
    ];
  }

  let uninhabitable = {
    encoder = [%expr fun () -> Js_of_ocaml.Js.null];
    decoder = [%expr fun _ -> ()];
  }

  let map = {
    encoder = [%expr fun key_to_string js_of_v fields ->
      fields
      |> List.map (fun (k, v) -> (key_to_string k, js_of_v v))
      |> Array.of_list
      |> Js_of_ocaml.Js.Unsafe.obj
    ];
    decoder = [%expr fun key_of_string js_to_v jv ->
        let open Js_of_ocaml in
        let fields = Js.Unsafe.fun_call "Object.entries" [| Js.Unsafe.inject jv |] in
        fields
        |> Js.to_array
        |> Array.map (fun elem ->
          let [| k; v |] = Js.to_array elem [@@warning "-8"] in
          (key_of_string (Js.to_string k), js_to_v v))
        |> Array.to_list
      ];
  }

  let all = [
    "unit", unit;
    "bool", bool;
    "int", int;
    "int53p", int53p;
    "float", float;
    "string", string;
    "uchar", uchar;
    "byte", byte;
    "bytes", bytes;
    "option", option;
    "list", list;
    "uninhabitable", uninhabitable;
    "map", map;
  ]
end

let builtin_codecs = Builtin_codecs.all

let builtin_codecs_map =
  builtin_codecs |> StringMap.of_list

let codec_of_coretype
  ?self_codec
  ~get_custom_codec
  ~get_name
  ~map_key_converter
  ~tuple_case
  ~string_enum_case
  (ct: coretype) =
  let loc = Location.none in
  match get_custom_codec ct.ct_configs with
  | Some coder -> evar coder
  | None ->
    let evar_name ?codec name =
      let open_, name = get_name ?codec name in
      evar ?open_ name
    in
    let rec go = function
      | Coretype.Prim p ->
        [%expr [%e evar_name (Coretype.string_of_prim p)]]
      | Uninhabitable -> evar_name "uninhabitable"
      | Ident i ->
        evar_name ~codec:i.id_codec i.id_name
      | Option (Option _) -> failwith "Nested option is not supported."
      | Option t -> [%expr [%e evar_name "option"] [%e go t]]
      | List t -> [%expr [%e evar_name "list"] [%e go t]]
      | Map (k, v) -> [%expr [%e evar_name "map"] [%e map_key_converter k] [%e go v]]
      | Tuple ts -> tuple_case ct.ct_configs go ts
      | StringEnum cs -> string_enum_case cs
      | Self -> self_codec |?! (fun () -> failwith "Cannot create codec of a recursive type because the argument 'self_codec' is not specified.")
    in
    go ct.ct_desc

let encoder_of_coretype =
  let encoder_of_objtuple ?additional_field ~loc vari to_expr =
    (function
    | [] -> additional_field |?! (fun () -> [%expr [||]])
    | ts ->
      let es = ts |> List.mapi (fun i t ->
        let label = estring ~loc (Json_config.tuple_index_to_field_name i) in
        let encoded, is_optional = to_expr i t in
        let efield = [%expr ([%e label], [%e encoded])] in
        efield, is_optional
      ) in
      let has_optional = List.exists snd es in
      let fields =
        es |> List.mapi (fun i (efield, is_optional) ->
          if is_optional then
            let v = vari i in
            [%expr Option.map (fun [%p pvar v] -> [%e efield]) [%e evar v]]
          else if has_optional then
            [%expr Some [%e efield]]
          else efield
        )
        |> Exp.array
      in
      let e =
        if has_optional then
          [%expr List.filter_map Kxclib.identity [%e fields]]
        else fields
      in
      match additional_field with
      | None -> e
      | Some fields -> [%expr Array.append [%e fields] [%e e]]
    )
  in
  let open Coretype in
  let vari i = "x"^string_of_int i in
  let loc = Location.none in
  let tuple_case (configs: [`coretype] configs) (go: desc -> expression) (ts: desc list) =
    let args =
      ts |> List.mapi (fun i _ -> pvar (vari i)) |> Pat.tuple
    in
    let body =
      match Json_config.get_tuple_style configs with
      | `obj `default ->
        let fields =
          encoder_of_objtuple ~loc vari (fun i -> function
            | Option (Option _) -> failwith "unsupported"
            | Option t ->
              [%expr [%e go t] [%e evar (vari i)]], true
            | t ->
              [%expr [%e go t] [%e evar (vari i)]], false
          ) ts
        in
        [%expr Js_of_ocaml.Js.Unsafe.obj [%e fields]]
      | `arr ->
        [%expr Js_of_ocaml.Js.array [%e
          ts |> List.mapi (fun i t -> [%expr [%e go t] [%e evar (vari i)]])
             |> Exp.array]]
    in
    Exp.fun_ Nolabel None args body
  in
  let map_key_converter: map_key -> _ = function `string -> [%expr fun k -> k] in

  let string_enum_case ~base_mangling_style (cs: string_enum_case list) =
    cs |&> (fun ((name, _, _) as c) ->
      let js_name = Json_config.get_mangled_name_of_string_enum_case ~inherited:base_mangling_style c in
      let pat = Pat.variant (escape_as_constructor_name name) None in
      let expr = [%expr Js.string [%e estring ~loc js_name]] in
      Exp.case pat expr
    )
    |> Exp.function_
    |> fun e -> [%expr let open Js_of_ocaml in [%e e]]
  in

  fun ?self_codec base_mangling_style ct ->
    codec_of_coretype
      ~get_custom_codec:Objintf_config.get_custom_encoder
      ~get_name:Bridge_labels.Codec.get_encoder_name
      ~tuple_case
      ~map_key_converter
      ~string_enum_case:(string_enum_case ~base_mangling_style)
      ?self_codec ct

let decoder_of_coretype =
  let open Coretype in
  let vari i = "x"^string_of_int i in
  let loc = Location.none in
  let tuple_case (configs: [`coretype] configs) (go: desc -> expression) (ts: desc list) =
    let body = match Json_config.get_tuple_style configs with
      | `arr ->
        let args = ts |> List.mapi (fun i _ -> pvar (vari i)) in
        let tuple_length_error_message =
          sprintf "expecting a tuple of length %d, but the given has a length of %%d" (List.length ts)
        in
        [%expr
          match Js_of_ocaml.Js.to_array jv with
          | [%p Pat.array args] ->
            [%e Exp.tuple (
              ts |> List.mapi (fun i t ->
                [%expr [%e go t] [%e evar (vari i)]])
            )]
          | xs -> Format.kasprintf failwith [%e estring ~loc tuple_length_error_message] (Array.length xs)
        ]
      | `obj `default ->
        ts |> List.mapi (fun i t ->
          let label_name = Json_config.tuple_index_to_field_name i in
          let error_message = sprintf "mandatory field '%s' does not exist" label_name in
          [%expr
            let open Js_of_ocaml in
            let x = Js.Unsafe.get jv [%e estring ~loc label_name] in
            [%e
              match t with
              | Option (Option _) -> failwith "unsupported"
              | Option t ->
                [%expr Js.Optdef.to_option x |> Option.map ([%e go t])]
              | t ->
                [%expr Js.Optdef.case x (fun () -> failwith [%e estring ~loc error_message])
                  [%e go t]]
            ]
          ]
        ) |> Exp.tuple
    in
    Exp.fun_ Nolabel None [%pat? jv] body
  in
  let map_key_converter = function | `string -> [%expr fun (s: string) -> Some s] in
  let string_enum_case ~base_mangling_style (cs: string_enum_case list) =
    (cs |&> (fun ((name, _, _) as c) ->
      let json_name = Json_config.get_mangled_name_of_string_enum_case ~inherited:base_mangling_style c in
      let pat = pstring ~loc json_name in
      let expr = Exp.variant (escape_as_constructor_name name) None in
      Exp.case pat [%expr [%e expr]]
    )) @ [
      let error_message =
        (cs |&> (Json_config.get_mangled_name_of_string_enum_case ~inherited:base_mangling_style &> sprintf "'%s'")
        |> String.concat ", ")
        |> sprintf "given string '%%s' is not one of [ %s ]"
      in
      Exp.case [%pat? s] [%expr Format.kasprintf failwith [%e estring ~loc error_message] s]
    ]
    |> fun cases -> [%expr fun jv -> [%e Exp.match_ [%expr Js_of_ocaml.Js.to_string jv] cases]]
  in
  fun ?self_codec base_mangling_style ct ->
    codec_of_coretype
      ~get_custom_codec:Objintf_config.get_custom_decoder
      ~get_name:Bridge_labels.Codec.get_decoder_name
      ~tuple_case
      ~map_key_converter
      ~string_enum_case:(string_enum_case ~base_mangling_style)
      ?self_codec ct

let jsoo_full_bridge_impl : type bridgeable_ident.
  resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:bridgeable_ident bridgeable_ident_resolver
  -> bridgeable_ident sameworld_objintf
  -> (module Full_bridge_impl with type bridgeable_ident = bridgeable_ident) =
  fun ~resolution_strategy ~bridgeable_ident_resolver objintf ->
    let objintf =
      validate_objintf ~bridgeable_ident_resolver objintf
      |> function
      | Ok x -> x
      | Error s -> failwith s
    in
    let polarity = objintf.so_polarity in
    let loc = Location.none in
    let open Bridge_labels.Codec in

    let encoder_of_complex_type :
      ?base_mangling_style:Json_config.json_mangling_style
      -> ?with_party:bool
      -> ?self_bridgeable:([> `complex | `simple ], bridgeable_ident) bridgeable_descriptor
      -> bridgeable_ident complex_type
      -> _ =
      fun ?(base_mangling_style=`default) ?(with_party=true) ?self_bridgeable typ ->
        let bridgeable_to_encoder party bd =
          let name = name_of_bridgeable bd in
          let encoder () =
            let open_, name = get_encoder_name name in
            evar ?open_ ~loc name
          in
          if with_party then
            match party_with_polarity ~polarity party with
            | `peer -> [%expr (fun (Bindoj_objintf_shared.Peer_object { raw_object; _ }) -> raw_object)]
            | `endemic ->
              [%expr (fun x -> ([%e encoder ()] ([%e evar Bridge_labels.unwrap_endemic] x)))]
          else
            encoder ()
        in
        match typ with
        | `direct ct | `nested({ td_kind = Alias_decl ct; _ }, _) as typ ->
          if is_self_coretype typ then failwith "Coretype.Self is not supported.";
          encoder_of_coretype
            base_mangling_style ct
        | `nested ({ td_name; _ } as td, codec) ->
          let codec =
            match resolution_strategy td codec with
            | `inline_type_definition -> `default
            | _ -> codec
          in
          let open_, name = Json_config.get_encoder_name ~resolution_strategy:codec td_name in
          [%expr Js_of_ocaml.(fun x ->
            [%e evar ?open_ name] x
            |> Kxclib.Json.to_yojson
            |> Yojson.Safe.to_string
            |> Js.string
            |> (fun x -> Js._JSON##parse x)
          )]
        | `bridgeable (party, bi) ->
          bi
          |> bridgeable_ident_resolver
          |> complex_descriptor_of_bridgeable
          |> bridgeable_to_encoder party
        | `self_bridgeable party ->
          bridgeable_to_encoder party (self_bridgeable |?! (fun () -> failwith "self_bridgeable is not specified."))
    and decoder_of_complex_type :
      ?base_mangling_style:Json_config.json_mangling_style
      -> ?with_party:bool
      -> ?self_bridgeable:([> `complex | `simple ], bridgeable_ident) bridgeable_descriptor
      -> bridgeable_ident complex_type
      -> _ =
      fun ?(base_mangling_style=`default) ?(with_party=true) ?self_bridgeable typ ->
        let decoder_of_bridgealble party bd =
          let name = name_of_bridgeable bd in
          let decoder =
            let open_, name = get_decoder_name name in
            evar ?open_ ~loc name
          in
          if with_party then
            match party_with_polarity ~polarity party with
            | `peer -> [%expr [%e evar Bridge_labels.wrap_peer] [%e decoder]]
            | `endemic -> [%expr (fun x -> [%e evar Bridge_labels.bridge_endemic] ([%e decoder] x))]
          else decoder
        in
        match typ with
        | `direct ct | `nested({ td_kind = Alias_decl ct; _ }, _) as typ ->
          if is_self_coretype typ then failwith "Coretype.Self is not supported.";
          decoder_of_coretype
            base_mangling_style ct
        | `nested ({ td_name; _ } as td, codec) ->
          let codec =
            match resolution_strategy td codec with
            | `inline_type_definition -> `default
            | _ -> codec
          in
          let open_, name = Json_config.get_decoder_name ~resolution_strategy:codec td_name in
          [%expr Js_of_ocaml.(fun x ->
            Js._JSON##stringify x
            |> to_string
            |> Yojson.Safe.from_string
            |> Kxclib.Json.of_yojson
            |> [%e evar ?open_ name]
            |> (function
              | Error e -> failwith (Bindoj_runtime.OfJsonResult.Err.to_string e)
              | Ok result -> result)
          )]
        | `bridgeable (party, bi) ->
          bridgeable_ident_resolver bi
          |> complex_descriptor_of_bridgeable
          |> decoder_of_bridgealble party
        | `self_bridgeable party ->
          decoder_of_bridgealble party (self_bridgeable |?! (fun () -> failwith "self_bridgeable is not specified."))
        
    in

    (module (struct
      type nonrec bridgeable_ident = bridgeable_ident

      let get_module_name = sprintf "%s_with_jsoo"

      let get_encoder_name = Bridge_labels.Codec.get_encoder_name &> snd
      let get_decoder_name = Bridge_labels.Codec.get_decoder_name &> snd

      let gen_ensure_peer_setup = fun () -> [%expr
        let open Js_of_ocaml in
        if not (Js.Unsafe.get [%e evar Bridge_labels.peer_full_bridge] [%e estring ~loc Bridge_labels.Js.setup_called]) then
          Js.Unsafe.meth_call [%e evar Bridge_labels.peer_full_bridge] [%e estring ~loc Bridge_labels.Js.setup] [|
            Js.Unsafe.inject ([%e mk_js_object [ `method_ (Bridge_labels.Js.setup_called, [], ebool ~loc true)]])
          |]
      ]

      let gen_setup_common_codecs () = [
        Vb.mk
          ~attrs:(warning_attribute "-32") (* suppress 'unused variable' warning *)
          (pvar Bridge_labels.unwrap_endemic) [%expr
          fun (Bindoj_objintf_shared.Endemic_object { bridge = [%p pvar Bridge_labels.bridge_marker_ctor]; underlying }) ->
            underlying
        ];
        Vb.mk
          ~attrs:(warning_attribute "-32") (* suppress 'unused variable' warning *)
          (pvar Bridge_labels.wrap_peer) [%expr
          fun access raw_object ->
            Bindoj_objintf_shared.Peer_object { bridge = [%e evar Bridge_labels.bridge_marker_ctor]; access; raw_object }
        ];
      ]

      let gen_setup_endemic_objects = fun _ -> [%expr
          let open Js_of_ocaml in
          Js.Unsafe.set
            ([%e (evar Bridge_labels.peer_full_bridge) |> ejs_unsafe_get ~loc (estring ~loc Bridge_labels.Js.instance)])
            [%e estring ~loc Bridge_labels.Js.peer_objects]
            ([%e mk_js_object (
              objintf.so_named_objects
              |&?> (function
                | { nod_name; nod_party; nod_typ; _ }
                  when party_with_polarity ~polarity nod_party = `endemic ->
                    some & `val_ (Bridge_labels.Js.mangle_field_name nod_name, [%expr
                      [%e encoder_of_complex_type ~with_party:false (nod_typ :> _ complex_type)] [%e evar (Bridge_labels.functor_parameter_var^"."^nod_name)]
                    ])
                | _ -> None)
            )])
        ]

      let gen_builtin_codec label =
        builtin_codecs_map
        |> StringMap.find_opt label

      let gen_type_decl_encoder td codec =
        match resolution_strategy td codec with
        | `inline_type_definition ->
          let name, body = Json_codec.gen_json_encoder' ~self_contained:true td in
          let body =
            pexp_constraint ~loc body
              [%type: [%t type_of_type_decl ~resolution_strategy td codec] -> Kxclib.Json.jv]
          in
          Some (Vb.mk ~attrs:(warning_attribute "-39") (pvar name) body)
        | _ -> None

      let gen_type_decl_decoder td codec =
        match resolution_strategy td codec with
        | `inline_type_definition ->
          let name, body = Json_codec.gen_json_decoder_result' ~self_contained:true ~codec td in
          let body =
            pexp_constraint ~loc body
              [%type: [%t type_of_type_decl ~resolution_strategy td codec] Bindoj_runtime.json_full_decoder]
          in
          Some (Vb.mk ~attrs:(warning_attribute "-39") (pvar name) body)
        | _ -> None

      open struct
        let argi i = function
        | None -> "__arg"^string_of_int i
        | Some name -> name
      end

      let gen_encoder : ([> `simple | `complex ], bridgeable_ident) bridgeable_descriptor -> expression =
        let gen_encoded_method ?self_bridgeable ~is_module_bundle caml_method =
          fun { name; positional_arguments; labeled_arguments; return_type; _ } ->
            let name_js = Bridge_labels.Js.mangle_field_name name in
            let var = caml_method name in
            let ret_typ = match return_type with
              | Mret_regular typ -> typ
            in
            let all_largs_are_optional =
              labeled_arguments |> List.for_all (function
                | Larg_regular { optional; _ } -> optional
                | Larg_optional_with_default _ -> true)
            in
            let args_js =
              let pargs_js =
                positional_arguments
                |> Bindoj_list.skip_tail_while (function
                  | Parg_regular { typ; _ } -> is_unit_type typ)
                |> List.mapi (fun i -> function
                | Parg_regular { name; _ } ->
                  (Nolabel, None, pvar & argi i name))
              in
              pargs_js @ (match labeled_arguments with
                | [] -> []
                | _ -> [ (Nolabel, None, pvar Bridge_labels.Js.labeledArgs) ])
            in
            let get_larg label optional encoder =
              if all_largs_are_optional then
                [%expr Js_of_ocaml.Js.(
                  Optdef.bind [%e evar Bridge_labels.Js.labeledArgs] (fun la ->
                    Optdef.map (Unsafe.get la [%e estring ~loc label]) [%e encoder])
                    |> Optdef.to_option)
                ]
              else if optional then
                [%expr Js_of_ocaml.Js.(
                  Optdef.map (Unsafe.get [%e evar Bridge_labels.Js.labeledArgs] [%e estring ~loc label]) [%e encoder]
                  |> Optdef.to_option)
                ]
              else
                [%expr Js_of_ocaml.Js.(
                  [%e encoder] (Unsafe.get [%e evar Bridge_labels.Js.labeledArgs] [%e estring ~loc label]))
                ]
            in
            let body =
              let wrap_unit_type typ body =
                if is_unit_type typ then [%expr ()]
                else body
              in
              (labeled_arguments |&> (function
                | Larg_regular { label; optional; typ; _ } ->
                  (if optional then Optional label else Labelled label),
                  (wrap_unit_type typ & get_larg label optional & decoder_of_complex_type ?self_bridgeable typ)
                | Larg_optional_with_default { label; typ; codec; default; _ } ->
                  let typ' = `nested(Typed.decl typ, codec) in
                  Labelled label, (
                    wrap_unit_type typ' [%expr
                      [%e get_larg label true & decoder_of_complex_type ?self_bridgeable typ']
                      |> Option.value ~default:[%e e_runtime_refl ~loc (Typed.reflect typ) default]])))
              @ (positional_arguments |> List.mapi (fun i -> function
                | Parg_regular { name; typ; _ } ->
                  Nolabel,
                  (wrap_unit_type typ [%expr [%e decoder_of_complex_type ?self_bridgeable typ] [%e evar & argi i name]])
              ))
              |> (function
                | [] when is_module_bundle -> [ Nolabel, [%expr ()] ]
                | args -> args)
              |> Exp.apply var
              |> fun e -> [%expr [%e encoder_of_complex_type ?self_bridgeable ret_typ] [%e e]]
            in
            `method_(name_js, args_js, body)
        in
        function
        | Sole_method_bridgeable desc ->
          let { name; _ } as  desc = body_of_method_descriptor desc in
          let arg_name = "__caml_obj" in
          let caml_method = constant (evar arg_name) in
          let body = match gen_encoded_method ~is_module_bundle:false caml_method desc with
            | `method_ (_, args, body) -> e_js_callback [%expr Js_of_ocaml.Js.Unsafe] args body
          in
          let arg = Pat.constraint_ (pvar arg_name) (typcons name) in
          [%expr fun [%p arg] -> [%e body]]
          
        | (Method_bundle_bridgeable { name; descs; configs; _ }) as self_bridgeable ->
          let caml_style = Objintf_config.get_caml_style configs in
          let arg, caml_method =
            match caml_style with
            | `object_ ->
              let arg_name = "__caml_obj" in
              Pat.constraint_ (pvar arg_name) (typcons name),
              (fun name -> evar & arg_name^"#"^name)
            | `module_ ->
              let arg_name = "M" in
              (Pat.constraint_ (Pat.unpack (locmk & some arg_name)) (typpack name)),
              (fun name -> evar & arg_name^"."^name)
          in
          descs
          |&> (body_of_method_descriptor &> gen_encoded_method ~self_bridgeable ~is_module_bundle:(caml_style = `module_) caml_method)
          |> mk_js_object
          |> Exp.fun_ Nolabel None arg

      let gen_decoder : (_, bridgeable_ident) bridgeable_descriptor -> expression =
        let arg_name = "__js_obj" in
        let gen_decoded_method ?self_bridgeable ~is_module_bundle call_method =
          fun { positional_arguments; labeled_arguments; return_type; _ } ->
            let last_argument_is_optional =
              List.empty positional_arguments
              && not (List.empty labeled_arguments)
              && (match List.last labeled_arguments with
                | Larg_regular { optional; _ } -> optional
                | Larg_optional_with_default _ -> true
              )
            in
            let args =
              (labeled_arguments |&> (function
                | Larg_regular { label; optional; _ } ->
                  ((if optional then Optional label else Labelled label), None, pvar label)
                | Larg_optional_with_default { label; _ } ->
                  Optional label, None, pvar label
                ))
              @ (positional_arguments |> List.mapi (fun i -> function
                | Parg_regular { typ; _ } when is_unit_type typ ->
                  (Nolabel, None, [%pat? ()])
                | Parg_regular { name; _ } ->
                  (Nolabel, None, pvar & argi i name)
              ))
              |> function
                | [] when is_module_bundle -> [ Nolabel, None, [%pat? ()]]
                | args -> args
            in
            let body =
              (positional_arguments |> List.mapi (fun i -> function
                | Parg_regular { typ; _ } when is_unit_type typ -> None
                | Parg_regular { name; typ; _ } ->
                  some [%expr [%e encoder_of_complex_type ?self_bridgeable typ] [%e evar & argi i name]]
              ) |&?> identity)
              @ (match labeled_arguments with
                | [] -> []
                | largs ->
                  largs |&> (fun larg ->
                    let label, body = match larg with
                      | Larg_regular { label; typ; optional=false; _ } ->
                        (label, [%expr [%e encoder_of_complex_type ?self_bridgeable typ] [%e evar label]])
                      | Larg_regular { label; typ; optional=true; _ } ->
                        (label, [%expr
                          Js.Opt.(map (option [%e evar label]) ([%e encoder_of_complex_type ?self_bridgeable typ]))
                        ])
                      | Larg_optional_with_default { label; typ; codec; default; _} ->
                        let typ' = `nested(Typed.decl typ, codec) in
                        (label, [%expr [%e encoder_of_complex_type ?self_bridgeable typ'] (
                          [%e evar label] |> Option.value ~default:[%e
                            e_runtime_refl ~loc (Typed.reflect typ) default
                          ]
                        )])
                    in
                    `val_ (Bridge_labels.Js.mangle_field_name label, body))
                  |> mk_js_object
                  |> List.return
              )
              |&> (fun e -> [%expr Js.Unsafe.inject [%e e]])
              |> Exp.array
            in
            let ret_typ = match return_type with
              | Mret_regular typ -> typ
            in
            let attrs =
              if last_argument_is_optional then
                Some (warning_attribute "-16")
              else
                None
            in
            efun args [%expr Js_of_ocaml.(
              [%e decoder_of_complex_type ?self_bridgeable ret_typ] ([%e call_method body])
            )],
            attrs
        in
        function
        | Sole_method_bridgeable desc ->
          let body, attrs =
            desc
            |> body_of_method_descriptor
            |> gen_decoded_method ~is_module_bundle:false (fun body ->
              [%expr Js.Unsafe.fun_call [%e evar arg_name] [%e body]]
            )
          in
          Exp.fun_ ?attrs Nolabel None (pvar arg_name) body
        | (Method_bundle_bridgeable { name; descs; configs; _}) as self_bridgeable ->
          let caml_style = Objintf_config.get_caml_style configs in
          let methods =
            descs |&> (
              body_of_method_descriptor
              &> (fun ({ name; _ } as desc) ->
                let js_name = Bridge_labels.Js.mangle_field_name name in
                name, desc |> gen_decoded_method ~self_bridgeable ~is_module_bundle:(caml_style = `module_) (fun body ->
                  [%expr Js.Unsafe.meth_call [%e evar arg_name] [%e estring ~loc js_name] [%e body]]
                )))
          in
          let body = match caml_style with
            | `object_ ->
              methods
              |&> (fun (label, (expr, attrs)) -> Cf.method_ ?attrs (locmk label) Public (Cfk_concrete (Fresh, expr)))
              |> Cstr.mk (Pat.any ())
              |> Exp.object_
            | `module_ ->
              (methods |&> (fun (label, (expr, attrs)) -> Vb.mk ?attrs (pvar label) expr))
              |> Str.value Nonrecursive
              |> List.return
              |> Mod.structure
              |> Exp.pack
              |> Fn.flip Exp.constraint_ (typpack name)
          in
          Exp.fun_ Nolabel None (pvar arg_name) body

      open struct
        let coordinate_arg_name = "coordinate"
        let args_of_coordinate_desc coordinate_desc earg =
          coordinate_desc
          |&> (fun (label, desc) ->
            Labelled label, [%expr
              [%e
                decoder_of_complex_type
                  (`direct (ord_coordinate_to_coretype desc))]
              (Js.Unsafe.get [%e earg] [%e estring ~loc label])
            ])
      end

      let gen_peer_object = fun { nod_name; nod_typ; _ } -> [%expr 
          let open Js_of_ocaml in
          (Js.Unsafe.get
            (Js.Unsafe.get
              (Js.Unsafe.get [%e evar Bridge_labels.peer_full_bridge] [%e estring ~loc Bridge_labels.Js.instance])
              [%e estring ~loc Bridge_labels.Js.endemic_objects])
          [%e estring ~loc (Bridge_labels.Js.mangle_field_name nod_name)])
          |> [%e decoder_of_complex_type (nod_typ :> _ complex_type)]
      ]

      let gen_setup_peer_object_registry = fun () ->
        let peer_object_registries =
          objintf.so_object_registries
          |?> (fun { ord_party; _ } -> party_with_polarity ~polarity ord_party = `peer)
        in
        let get_instance =
          evar Bridge_labels.peer_full_bridge
          |> ejs_unsafe_get ~loc (estring ~loc Bridge_labels.Js.instance)
        in
        let body = [%expr
          Js.Unsafe.set
            [%e get_instance]
            [%e estring ~loc Bridge_labels.Js.endemic_object_registry]
            [%e mk_js_object (
              let value = "value" in
              peer_object_registries
              |&> (fun { ord_name; ord_coordinate_desc; ord_typ; _ } ->
                `val_(Bridge_labels.Js.mangle_field_name ord_name, mk_js_object [
                  `method_(
                    Bridge_labels.Js.EndemicObjectRegistry.register,
                    [Nolabel, None, pvar coordinate_arg_name; Nolabel, None, pvar value],
                    Exp.apply (evar & Bridge_labels.get_register_name ord_name)
                      & (args_of_coordinate_desc ord_coordinate_desc (evar coordinate_arg_name)
                        @ [ Nolabel, [%expr Some([%e decoder_of_complex_type (ord_typ :> _ complex_type)] [%e evar value])]])
                  );
                  `method_(
                    Bridge_labels.Js.EndemicObjectRegistry.deregister,
                    [Nolabel, None, pvar coordinate_arg_name],
                    Exp.apply (evar & Bridge_labels.get_register_name ord_name)
                      & (args_of_coordinate_desc ord_coordinate_desc (evar coordinate_arg_name)
                        @ [ Nolabel, [%expr None]])
                  );
                ]))
            )]
        ]
        in
        let js_registry = "js_registry" in
        [%expr
          let open Js_of_ocaml in
          let [%p pvar js_registry] =
            [%e get_instance
            |> ejs_unsafe_get ~loc (estring ~loc Bridge_labels.Js.endemic_object_registry)]
          in
          [%e (peer_object_registries |&> (fun { ord_name; ord_coordinate_desc; ord_typ; _ } ->
              [%expr
                [%e
                  evar js_registry
                  |> ejs_unsafe_get ~loc (estring ~loc & Bridge_labels.Js.mangle_field_name ord_name)
                  |> ejs_unsafe_get ~loc (estring ~loc & Bridge_labels.Js.EndemicObjectRegistry.initial_objects)
                ]
                |> Js.to_array
                |> Array.iter (fun x ->
                  let [%p pvar coordinate_arg_name] = Js.Unsafe.get x [%e estring ~loc Bridge_labels.Js.EndemicObjectRegistry.coordinate] in
                  match Js.to_string (Js.Unsafe.get x "kind") with
                  | [%p pstring ~loc Bridge_labels.Js.EndemicObjectRegistry.register] ->
                    [%e Exp.apply (evar & Bridge_labels.get_register_name ord_name)
                        & (args_of_coordinate_desc ord_coordinate_desc (evar coordinate_arg_name) @ [
                          Nolabel, [%expr Some ([%e decoder_of_complex_type (ord_typ :> _ complex_type)] (Js.Unsafe.get x "value"))]
                        ])]
                  | [%p pstring ~loc Bridge_labels.Js.EndemicObjectRegistry.deregister] ->
                    [%e Exp.apply (evar & Bridge_labels.get_register_name ord_name)
                        & (args_of_coordinate_desc ord_coordinate_desc (evar coordinate_arg_name) @ [
                          Nolabel, [%expr None]
                        ])]
                  | kind -> failwith ("unexpected kind: "^kind)
                )
              ]
            )) |> Fn.flip (List.fold_right Exp.sequence) body];
        ]

      let gen_setup_endemic_object_registry = fun () -> [%expr
          let open Js_of_ocaml in
          Js.Unsafe.set
            (Js.Unsafe.get
              [%e evar Bridge_labels.peer_full_bridge]
              [%e estring ~loc Bridge_labels.Js.instance])
            [%e estring ~loc Bridge_labels.Js.peer_object_registry]
            [%e mk_js_object (
              objintf.so_object_registries
              |&?> (function
                | { ord_name; ord_party; ord_coordinate_desc; ord_typ; _ }
                  when party_with_polarity ~polarity ord_party = `endemic ->
                    some & `method_(Bridge_labels.Js.lookup_peer_object ord_name, [Nolabel, None, pvar coordinate_arg_name], [%expr
                      [%e Exp.apply (evar & Bridge_labels.get_lookup_name ord_name)
                          & args_of_coordinate_desc ord_coordinate_desc (evar coordinate_arg_name)]
                      |> function
                      | None -> Js.Unsafe.inject Js.null
                      | Some obj -> Js.Unsafe.inject ([%e encoder_of_complex_type ~with_party:false (ord_typ :> _ complex_type)] obj)
                    ])
                | _ -> None)
            )]
      ]

      let gen_endemic_full_bridge =
        fun ~setup_called ~setup ->
          [%expr ([%e
            mk_js_object [
              `method_ (Bridge_labels.Js.setup_called, [], setup_called);
              `method_ (
                Bridge_labels.Js.setup,
                [ Nolabel, None,
                  Pat.constraint_
                    (pvar Bridge_labels.peer_full_bridge)
                    (typcons "Bindoj_objintf_shared.endemic_full_bridge_reference")
                ],
                setup);
            ]] |> fun x -> (Obj.magic x: Bindoj_objintf_shared.endemic_full_bridge_reference))]

    end) : Full_bridge_impl with type bridgeable_ident = bridgeable_ident)

let gen_full_bridge_impl = gen_full_bridge_impl ~impl:jsoo_full_bridge_impl

let gen_structure : type bridgeable_ident.
  ?resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:bridgeable_ident bridgeable_ident_resolver
  -> bridgeable_ident sameworld_objintf
  -> Ppxlib.structure
  = fun ?resolution_strategy ~bridgeable_ident_resolver objintf ->
    gen_structure ~generators:[ gen_full_bridge_impl ]
    ?resolution_strategy ~bridgeable_ident_resolver objintf
