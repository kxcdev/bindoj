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
open Bindoj_typedesc.Typed_type_desc
open Bindoj_objintf_shared
open Bindoj_gen_ts.Typescript_datatype
open Bindoj_common
open Bindoj_objintf_gen_utils

module Bridge_labels = struct
  include Bindoj_objintf_shared_js.Bridge_labels

  let bindoj = "bindoj"

  let bytes = bindoj^".bytes"

  let generated_bridge = "GeneratedBridge"
  let interfaces = "Interfaces"
  let concrete_bridge = "ConcreteBridge"
  let peer_full_bridge_reference = "PeerFullBridgeReference"

  let endemic_setup_params = "EndemicSetupParams"

  let setup_less_full_bridge = "FullBridge_setupless"
  let endemic_setup_only_full_bridge = "FullBrdge_endemicSetupOnly"
  let peer_setup_only_full_bridge = "FullBridge_peerSetupOnly"
  let dual_setup_full_bridge = "FullBridge_dualSetup"

  let initially_registered_objects = "initiallyRegisteredObjects"
  let endemic_bjects = "endemicObjects"

  let peer_full_bridge = "peerFullBridge"
  let bridge_async = "bridgeAsync"
  let get_bridge = "getBridge"

  let init_params = "initParams"

  let coordinate = "coordinate"
  let obj = "obj"

  let register = "register"
  let deregister = "deregister"

  module Runtime = struct
    let init_endemic_object_registry = bindoj^".Internal.initEndemicObjectRegistry"
    let create_full_bridge_peer_setup_only = bindoj^".Internal.createFullBridgePeerSetupOnlyImpl"
    let create_full_bridge_endemic_setup_only = bindoj^".Internal.createFullBridgeEndemicSetupOnlyImpl"
    let create_full_bridge_dual_setup = bindoj^".Internal.createFullBridgeDualSetupImpl"
  end
end

type type_decl_resolution_strategy = [
  | `import_location of string
  | `inline_type_definition
  | `infile_type_definition
]

type ident_resolution_strategy = [
  | `import_location of string
  | `inline_type_definition of ts_type_desc
  | `infile_type_definition of ts_type_desc
]

let validate_resolution_strategy ~(type_decl_resolution_strategy: _ -> _ -> type_decl_resolution_strategy) td codec = 
  let strategy = type_decl_resolution_strategy td codec in
  let () = match strategy with
  | `import_location _
  | `infile_type_definition -> ()
  | `inline_type_definition when is_recursive td -> failwith "Recursive type cannot be defined inilne."
  | `inline_type_definition -> ()
  in
  strategy

let resolve_type_decls_and_idents
  ~(type_decl_resolution_strategy: _ -> _ -> type_decl_resolution_strategy)
  ~(ident_resolution_strategy: _ -> ident_resolution_strategy) =
  let choose_td = function
    | `nested (td, codec) -> Some (td, codec)
    | `direct _ -> None
  in
  let rec go tds ids = function
    | [] -> tds, ids
    | [] :: ys -> go tds ids ys
    | ((td, codec) as x :: xs) :: ys ->
      let resolution = validate_resolution_strategy ~type_decl_resolution_strategy td codec in
      begin match resolution, td.td_kind with
        | `import_location _, _ -> go tds ids ys
        | _, Alias_decl _ ->
          let id = nested_type_to_ident_opt (`nested x) |> Option.to_list in
          go (x :: tds) (id @ ids) (xs :: ys)
        | _, Record_decl fields ->
          let ids =
            (fields |&?> ((fun r -> r.rf_type) &> nested_type_to_ident_opt)) @ ids
          in
          let typs =
            fields |&?> ((fun r -> r.rf_type) &> choose_td)
          in
          go (x :: tds) ids (typs :: xs :: ys)
        | _, Variant_decl ctors ->
          let typs, ids' =
            ctors |&> (fun c ->
            match c.vc_param with
            | `no_param -> [], []
            | `tuple_like args ->
              args |&?> ((fun a -> a.va_type) &> choose_td),
              args |&?> ((fun a -> a.va_type) &> nested_type_to_ident_opt)
            | `inline_record fields ->
              fields |&?> ((fun r -> r.rf_type) &> choose_td),
              fields |&?> ((fun r -> r.rf_type) &> nested_type_to_ident_opt)
            | `reused_inline_record td when
              Ts_config.(get_reused_variant_inline_record_style_opt c.vc_configs
                |? default_reused_variant_inline_record_style) = `intersection_type
              -> [ (td, `default) ], []
            | `reused_inline_record _ -> [], []
            )
            |> List.unzip
            |> fun (tds, ids) -> List.concat tds, List.concat ids
          in
          go (x :: tds) (ids' @ ids) (typs :: xs :: ys)
      end
  in
  fun objintf ->
    let tds, ids = go [] (extract_idents objintf) [ extract_type_decls objintf ] in
    let tds, ids = tds |> Bindoj_list.uniq_by identity, ids |> Bindoj_list.uniq_by fst in
    let imports =
      (tds |&?> (fun (td, codec) ->
        match type_decl_resolution_strategy td codec with
        | `import_location location -> some (Ts_config.get_mangled_name_of_type ~escaping_charmap:Mangling.charmap_js_identifier td |> fst, location)
        | _ -> None))
      @ (ids |&?> (fun (id, mangling_style) ->
        match ident_resolution_strategy id with
        | `import_location location -> some (
          Ts_config.mangled `type_name mangling_style id.id_name |> Mangling.(escape ~charmap:charmap_js_identifier), location)
        | _ -> None))
      |> List.group_by snd
      |&> (fun (loc, names) ->
        `import {
          tsi_from = loc;
          tsi_import_items = names |&> (fun (tsii_name, _) -> { tsii_name; tsii_alias = None; })
        })
    in
    let infile_decls =
      (tds |&?> (fun (td, codec) ->
        match type_decl_resolution_strategy td codec with
        | `infile_type_definition ->
          some &
            td |> ts_fwrt_decl_of_type_decl ~export:true ~readonly:true
            |> ts_ast_of_fwrt_decl
        | _ -> None))
      @ (ids |&?> (fun (id, mangling_style) ->
        match ident_resolution_strategy id with
        | `infile_type_definition tsa_type_desc ->
          some [`type_alias_declaration {
            tsa_modifiers = [ `export ];
            tsa_name = Ts_config.mangled `type_name mangling_style id.id_name |> Mangling.(escape ~charmap:charmap_js_identifier);
            tsa_type_parameters = [];
            tsa_type_desc;
          }]
        | _ -> None))
      |> List.concat
    in
    imports, infile_decls 


let ts_type_desc_of_type_decl :
  type_decl_resolution_strategy:(type_decl -> Coretype.codec -> type_decl_resolution_strategy)
  -> type_decl
  -> Coretype.codec
  -> ts_type_desc =
  fun ~type_decl_resolution_strategy td codec ->
    match type_decl_resolution_strategy td codec with
    | `import_location _
    | `infile_type_definition ->
      `type_reference (Ts_config.get_mangled_name_of_type ~escaping_charmap:Mangling.charmap_js_identifier td |> fst)
    | `inline_type_definition ->
      td
      |> ts_fwrt_decl_of_type_decl ~export:true ~readonly:true
      |> ts_type_desc_of_fwrt_decl

let ts_type_desc_of_ident :
  ident_resolution_strategy:(Coretype.ident -> ident_resolution_strategy)
  -> Ts_config.json_mangling_style
  -> Coretype.ident
  -> ts_type_desc =
  fun ~ident_resolution_strategy mangling_style ident ->
    match ident_resolution_strategy ident with
    | `import_location _
    | `infile_type_definition _ ->
      `type_reference (Ts_config.mangled `type_name mangling_style ident.id_name |> Mangling.(escape ~charmap:charmap_js_identifier))
    | `inline_type_definition desc -> desc

let ts_type_literal_of_ord_coordinates
  ?(base_mangling_style = `default)
  (ord_coordinate_desc: (string * coordinate_type_desc) list) =
  `type_literal (
    ord_coordinate_desc
    |&> (fun (name, typ) ->
      { tsps_modifiers = [ `readonly ];
        tsps_name = Bridge_labels.mangle_field_name name;
        tsps_optional = false;
        tsps_type_desc =
          (match typ with
          | `prim p -> type_of_coretype base_mangling_style  & Coretype.(mk_prim (p :> prim))
          | `string_enum cases -> type_of_coretype base_mangling_style  & Coretype.mk_string_enum cases); })
    )

let ts_ast_of_objintf :
  type_decl_resolution_strategy:(type_decl -> Coretype.codec -> type_decl_resolution_strategy)
  -> ident_resolution_strategy:(Coretype.ident -> ident_resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> bindoj_runtime_import_location:string
  -> 'bridgeable_ident sameworld_objintf
  -> ts_ast =
  fun ~type_decl_resolution_strategy ~ident_resolution_strategy ~bridgeable_ident_resolver ~bindoj_runtime_import_location objintf ->
    let polarity = objintf.so_polarity in
    let ts_type_desc_of_complex_type :
      ?unwrap_bridgeable:bool
      -> ?self_bridgeable:([> `complex | `simple ], 'a) bridgeable_descriptor
      -> 'bridgeable_ident complex_type
      -> ts_type_desc =
      fun ?(unwrap_bridgeable=false) ?self_bridgeable ->
        let bridgeable_to_type_desc f party bd =
          let name =
            name_of_bridgeable bd
            |> Mangling.(snake_to_upper_camel ~preserve_version_substring:true &> escape ~charmap:charmap_js_identifier)
            |> f
          in
          let type_desc = `type_reference name in
          let party =
            Bridge_labels.bindoj^"."^(match party_with_polarity ~polarity party with
            | `endemic -> "endemic"
            | `peer -> "peer")
          in
          if unwrap_bridgeable then type_desc
          else `type_construct(party, [ type_desc ])
        in
        function
        | (`direct _ | `nested _) as kind when is_self_coretype kind ->
            failwith "Coretype.Self is not supported."
        | `direct { ct_desc = Prim `unit; _ }
        | `nested ({ td_kind = Alias_decl { ct_desc = Prim `unit; _ }; _ }, _) -> `special `null
        | `direct { ct_desc = Prim `bytes; _ }
        | `nested ({ td_kind = Alias_decl { ct_desc = Prim `bytes; _ }; _ }, _) -> `type_reference Bridge_labels.bytes
        | (`direct { ct_desc = Ident _; _ }
        | `nested ({ td_kind = Alias_decl { ct_desc = Ident _; _ }; _ }, _)) as typ ->
          let (ident, mangling_style) = nested_type_to_ident_opt typ |> Option.get in
          ts_type_desc_of_ident ~ident_resolution_strategy mangling_style ident
        | `direct ct -> type_of_coretype `default ct
        | `nested (td, codec) -> ts_type_desc_of_type_decl ~type_decl_resolution_strategy td codec
        | `bridgeable (party, bi) ->
          bridgeable_ident_resolver bi
          |> (fun desc ->
            bridgeable_to_type_desc
              (match desc with
              | Simple_bridgeable _ -> identity
              | Complex_bridgeable _ -> sprintf "%s.%s" Bridge_labels.interfaces)
              party (complex_descriptor_of_bridgeable desc))
        | `self_bridgeable party ->
          begin match self_bridgeable with
          | None -> failwith "self_bridgeable is not specified."
          | Some bd ->
            bridgeable_to_type_desc identity party bd
          end
    in
    let ts_statement_of_bridgeable_desc :
      ([ `complex | `simple ], 'bridgeable_ident) bridgeable_descriptor
      -> ts_statement =
      let argi i name =
        match name with
        | None -> "__arg"^string_of_int i
        | Some s -> Bridge_labels.mangle_field_name s
      in
      let ts_type_desc_of_method_body : ?self_bridgeable: _ ->  'bridgeable_ident complex_type method_descriptor_body -> ts_type_desc =
        fun ?self_bridgeable { positional_arguments; labeled_arguments; return_type; _ } ->
          let pargs =
            positional_arguments
            |> Bindoj_list.skip_tail_while (function
              | Parg_regular { typ; _ } -> is_unit_type typ)
            |> List.mapi (fun i -> function
              | Parg_regular { name; typ; _ } ->
                { tsp_name = argi i name;
                  tsp_optional = false;
                  tsp_type_desc = ts_type_desc_of_complex_type ?self_bridgeable typ; })
          in
          let larg =
            labeled_arguments
            |&?> (function
              | Larg_regular { typ; _ } when is_unit_type typ -> None
              | la -> Some la)
            |> function
            | [] -> []
            | ls ->
              let all_largs_are_optional =
                ls |> List.for_all (function
                  | Larg_regular { optional; _ } -> optional
                  | Larg_optional_with_default _ -> true)
              in
              [ { tsp_name = Bridge_labels.labeledArgs;
                  tsp_optional = all_largs_are_optional;
                  tsp_type_desc = `type_literal (
                    ls |&?> (function
                      | Larg_regular { label; optional; typ; _ } ->
                        Some
                          { tsps_modifiers = [ `readonly ];
                            tsps_name = Bridge_labels.mangle_field_name label;
                            tsps_optional = optional;
                            tsps_type_desc = ts_type_desc_of_complex_type ?self_bridgeable typ }
                      | Larg_optional_with_default { label; typ; codec; _ } ->
                        Some
                          { tsps_modifiers = [ `readonly ];
                            tsps_name = Bridge_labels.mangle_field_name label;
                            tsps_optional = true;
                            tsps_type_desc = ts_type_desc_of_complex_type ?self_bridgeable & `nested (Typed.decl typ, codec) })
                  ) }]
          in
          let tsft_type_desc = match return_type with
            | Mret_regular typ when is_unit_type typ -> `special `void
            | Mret_regular typ -> ts_type_desc_of_complex_type ?self_bridgeable typ
          in
          `func_type { tsft_parameters = pargs @ larg; tsft_type_desc }
      in
      function
      | Sole_method_bridgeable desc ->
        let desc = body_of_method_descriptor desc in
        `type_alias_declaration {
          tsa_modifiers = [ `export ];
          tsa_name = Mangling.(snake_to_upper_camel ~preserve_version_substring:true desc.name |> escape ~charmap:charmap_js_identifier);
          tsa_type_parameters = [];
          tsa_type_desc = ts_type_desc_of_method_body desc;
        }
      | (Method_bundle_bridgeable { name; descs; _ }) as self_bridgeable ->
        `type_alias_declaration {
          tsa_modifiers = [ `export ];
          tsa_name = Mangling.(snake_to_upper_camel ~preserve_version_substring:true name |> escape ~charmap:charmap_js_identifier);
          tsa_type_parameters = [];
          tsa_type_desc =
            `type_literal (
              descs |&> (fun desc ->
                let desc = body_of_method_descriptor desc in
                { tsps_modifiers = [ `readonly ];
                  tsps_name = Mangling.snake_to_lower_camel ~preserve_version_substring:true desc.name;
                  tsps_optional = false;
                  tsps_type_desc = ts_type_desc_of_method_body ~self_bridgeable desc }
              ));
        }
    in
    let (imports, infile_decls) : ts_ast * ts_ast =
      resolve_type_decls_and_idents
          ~type_decl_resolution_strategy
          ~ident_resolution_strategy
          objintf
    and simple_interfaces =
      objintf |> map_bridgeables_of `simple ts_statement_of_bridgeable_desc
    and generated_bridge_body : ts_ast =
      let complex_interfaces =
        objintf |> map_bridgeables_of `complex ts_statement_of_bridgeable_desc
      and peer_object_registry =
        objintf.so_object_registries
        |?> (fun { ord_party; _ } -> party_with_polarity ~polarity ord_party = `peer)
      and peer_objects =
        objintf.so_named_objects
        |?> (fun { nod_party; _ } -> party_with_polarity ~polarity nod_party = `peer)
      and endemic_object_registries =
        objintf.so_object_registries
        |?> (fun{ ord_party; _ } ->
          party_with_polarity ~polarity ord_party = `endemic
        )
      and endemic_objects =
        objintf.so_named_objects
        |?> (fun { nod_party; _ } -> party_with_polarity ~polarity nod_party = `endemic)
      in
      let peer_object_registry_properties =
        peer_object_registry
        |&> (fun { ord_name; ord_coordinate_desc; ord_typ; _ } ->
            let () =
              if List.empty ord_coordinate_desc then
                failwith "'ord_coordinate_desc' must have at least one element."
            in
            { tsps_modifiers = [ `readonly ];
              tsps_name = Bridge_labels.lookup_peer_object ord_name;
              tsps_optional = false;
              tsps_type_desc = `func_type {
                tsft_parameters = [
                  { tsp_name = Bridge_labels.coordinate;
                    tsp_optional = false;
                    tsp_type_desc = ts_type_literal_of_ord_coordinates ord_coordinate_desc; }
                ];
                tsft_type_desc = `union [
                  ts_type_desc_of_complex_type (ord_typ :> _ complex_type);
                  `special `null;
                ]
              };
            })
      and peer_objects_properties =
        peer_objects
        |&> (fun { nod_name; nod_typ; _ } ->
              { tsps_modifiers = [ `readonly ];
                tsps_name = Bridge_labels.mangle_field_name nod_name;
                tsps_optional = false;
                tsps_type_desc = ts_type_desc_of_complex_type (nod_typ :> _ complex_type); })
      and endemic_object_registries_properties =
        endemic_object_registries
        |&> (fun { ord_name; ord_coordinate_desc; ord_typ; _ } ->
          let coordinate_parameter =
            { tsp_name = Bridge_labels.coordinate;
              tsp_optional = false;
              tsp_type_desc = ts_type_literal_of_ord_coordinates ord_coordinate_desc; }
          in
          { tsps_modifiers = [ `readonly ];
            tsps_name = Bridge_labels.mangle_field_name ord_name;
            tsps_optional = false;
            tsps_type_desc = `type_literal [
              { tsps_modifiers = [ `readonly ];
                tsps_name = Bridge_labels.register;
                tsps_optional = false;
                tsps_type_desc = `func_type {
                  tsft_parameters = [
                    coordinate_parameter;
                    { tsp_name = Bridge_labels.obj;
                      tsp_optional = false;
                      tsp_type_desc = ts_type_desc_of_complex_type (ord_typ :> _ complex_type)} ];
                  tsft_type_desc = `special `void;
                };
              };
              { tsps_modifiers = [ `readonly ];
                tsps_name = Bridge_labels.deregister;
                tsps_optional = false;
                tsps_type_desc = `func_type {
                  tsft_parameters = [ coordinate_parameter ];
                  tsft_type_desc = `special `void;
                };
              };
            ];
          })
      in
      let no_empty_type_literal_property name = function
        | [] -> None
        | xs -> Some {
          tsps_modifiers = [ `readonly ];
          tsps_name = name;
          tsps_optional = false;
          tsps_type_desc = `type_literal xs }
      in
      let endemic_setup_params_properties =
        [ 
          (endemic_object_registries
          |&> (fun { ord_name; ord_coordinate_desc; ord_typ; _ } ->
                let () =
                  if List.empty ord_coordinate_desc then
                    failwith "'ord_coordinate_desc' must have at least one element."
                in
                { tsps_modifiers = [ `readonly ];
                  tsps_optional = false;
                  tsps_name = Mangling.(snake_to_lower_camel ord_name |> escape ~charmap:charmap_js_identifier);
                  tsps_type_desc =
                    `array (`tuple [
                      ts_type_literal_of_ord_coordinates ord_coordinate_desc;
                      ts_type_desc_of_complex_type ~unwrap_bridgeable:true (ord_typ :> _ complex_type);
                    ]);
                }
          ) |> no_empty_type_literal_property Bridge_labels.initially_registered_objects);
          (endemic_objects
          |&> (fun { nod_name; nod_typ; _ } ->
              { tsps_modifiers = [ `readonly ];
                tsps_optional = false;
                tsps_name = Mangling.(snake_to_lower_camel nod_name |> escape ~charmap:charmap_js_identifier);
                tsps_type_desc = ts_type_desc_of_complex_type ~unwrap_bridgeable:true (nod_typ :> _ complex_type);
              })
          |> no_empty_type_literal_property Bridge_labels.endemic_bjects);
        ] |&?> identity
      in

      let setup_kind =
        match (endemic_object_registries_properties, peer_objects_properties), endemic_setup_params_properties with
        | ([], []), [] -> `setup_less
        | _, [] -> `peer_setup_only
        | ([], []), _ -> `endemic_setup_only
        | _ -> `dual_setup
      in
      let init_params = Bridge_labels.init_params in
      let endemic_setup_param () =
        { tsp_name = init_params;
          tsp_optional = false;
          tsp_type_desc = `type_reference Bridge_labels.endemic_setup_params }
      in
      let full_bridge_type_name, full_bridge_type_desc =
        let fullbridge_with_setup params properties =
          let desc = `type_literal (
            properties @ [
              { tsps_modifiers = [ `readonly ];
                tsps_name = Bridge_labels.setup;
                tsps_optional = false;
                tsps_type_desc =
                  `func_type {
                    tsft_parameters = [
                      { tsp_name = Bridge_labels.peer_full_bridge;
                        tsp_optional = false;
                        tsp_type_desc = `type_construct Bridge_labels.((bindoj^"."^peer_full_bridge_reference), [`type_reference concrete_bridge])}
                    ];
                    tsft_type_desc = `special `void;
                  }}
            ];
          )
          in
          match params with
          | [] -> desc
          | params ->
            `func_type {
              tsft_parameters = params;
              tsft_type_desc = desc;
            }
        in
        let peer_setup_properties () =
          [ { tsps_modifiers = [ `readonly ];
              tsps_name = Bridge_labels.bridge_async;
              tsps_optional = false;
              tsps_type_desc = `type_construct ("Promise", [ `type_reference Bridge_labels.concrete_bridge ])};
            { tsps_modifiers = [ `readonly ];
              tsps_name = Bridge_labels.get_bridge;
              tsps_optional = false;
              tsps_type_desc =
                `func_type {
                  tsft_parameters = [];
                  tsft_type_desc = `union [
                    `type_reference Bridge_labels.concrete_bridge;
                    `special `null
                  ];
                }; } ]
        in
        match setup_kind with
        | `setup_less ->
          Bridge_labels.setup_less_full_bridge,
          `type_reference Bridge_labels.concrete_bridge
        | `peer_setup_only ->
          Bridge_labels.peer_setup_only_full_bridge,
          fullbridge_with_setup [] & peer_setup_properties ()
        | `endemic_setup_only ->
          Bridge_labels.endemic_setup_only_full_bridge,
          fullbridge_with_setup [ endemic_setup_param () ] & [
            { tsps_modifiers = [ `readonly ];
              tsps_name = "bridge";
              tsps_optional = false;
              tsps_type_desc = `type_reference Bridge_labels.concrete_bridge } ]
        | `dual_setup ->
          Bridge_labels.dual_setup_full_bridge,
          fullbridge_with_setup [ endemic_setup_param () ] & peer_setup_properties ()
      in
      let peer_full_bridge_alias_decl =
        Some(`type_alias_declaration {
          tsa_modifiers  = [ `export ];
          tsa_name = Bridge_labels.peer_full_bridge_reference;
          tsa_type_parameters = [];
          tsa_type_desc = `type_construct Bridge_labels.(bindoj^"."^peer_full_bridge_reference, [ `type_reference concrete_bridge ])
        })
      in
      let interfaces_namespace =
        Some (`module_declaration {
          tsm_modifiers = [ `export ];
          tsm_name = Bridge_labels.interfaces;
          tsm_body = complex_interfaces;
        })
      in
      let endemic_setup_params_type_decl =
        match setup_kind with
        | `endemic_setup_only | `dual_setup ->
          Some (`type_alias_declaration {
            tsa_modifiers = [ `export ];
            tsa_name = Bridge_labels.endemic_setup_params;
            tsa_type_parameters = [];
            tsa_type_desc = `type_literal endemic_setup_params_properties;
          })
        | _ -> None
      in
      let concrete_bridge_type_decl =
        Some (
        `type_alias_declaration {
          tsa_modifiers = [ `export ];
          tsa_name = Bridge_labels.concrete_bridge;
          tsa_type_parameters = [];
          tsa_type_desc = `type_literal ([
            no_empty_type_literal_property Bridge_labels.peer_object_registry peer_object_registry_properties;
            no_empty_type_literal_property Bridge_labels.peer_objects peer_objects_properties;
            no_empty_type_literal_property Bridge_labels.endemic_object_registry endemic_object_registries_properties;
          ] |&?> identity);
        })
      in
      let full_bridge_type_decl =
        Some (
          `type_alias_declaration {
            tsa_modifiers = [ `export ];
            tsa_type_parameters = [];
            tsa_name = full_bridge_type_name;
            tsa_type_desc = full_bridge_type_desc;
          })
      in
      let concrete_bridge_value_decl =
        Some (
          let bridge_var_name = "bridge" in
          let no_empty_object_literal_property name = function
            | [] -> []
            | xs ->
              List.return (name, `literal_expression(`object_literal xs))
          in
          let peer_object_registry_decl =
            peer_object_registry
            |&> (fun { ord_name; _ } ->
              Bridge_labels.lookup_peer_object ord_name,
              `arrow_function {
                tsaf_parameters = [];
                tsaf_body = [
                  `throw_statement (`literal_expression (`string_literal "This bridge has not been set up."))
                ]
              }
            )
            |> no_empty_object_literal_property Bridge_labels.peer_object_registry
          and peer_objects_decl =
            peer_objects
            |&> (fun { nod_name; nod_typ; _ } ->
              Bridge_labels.mangle_field_name nod_name,
              `casted_expression(
                `casted_expression(
                  `identifier "undefined",
                  `special `unknown
                ),
                ts_type_desc_of_complex_type (nod_typ :> _ complex_type)
              ))
            |> no_empty_object_literal_property Bridge_labels.peer_objects
          and endemic_object_registries_decl =
            endemic_object_registries
            |&> (fun { ord_name; _} ->
              let name = Bridge_labels.mangle_field_name ord_name in
              name, `call_expression {
                tsce_expression = `identifier Bridge_labels.Runtime.init_endemic_object_registry;
                tsce_arguments = [
                  `identifier (sprintf "%s.%s.%s" init_params Bridge_labels.initially_registered_objects name)
                ];
              })
            |> no_empty_object_literal_property Bridge_labels.endemic_object_registry
          in
          let bridge_decl =
            `value_declaration {
              tsv_modifiers = [];
              tsv_kind =  `const;
              tsv_name = bridge_var_name;
              tsv_type_desc = None;
              tsv_value = `literal_expression (`object_literal ([
                peer_object_registry_decl;
                peer_objects_decl;
                endemic_object_registries_decl;
                (match endemic_objects with
                  | [] -> []
                  | _ ->
                    List.return (Bridge_labels.endemic_bjects,
                      `identifier (init_params^"."^Bridge_labels.endemic_bjects)
                    ));
                ] |> List.concat);
              );
            }
          in
          let create_concrete_bridge =
            `arrow_function {
              tsaf_parameters = (
                match setup_kind with
                | `setup_less | `peer_setup_only -> []
                | `endemic_setup_only | `dual_setup -> [
                  { tsp_name = Bridge_labels.init_params;
                    tsp_optional = false;
                    tsp_type_desc = `type_reference Bridge_labels.endemic_setup_params; }
                ]
              );
              tsaf_body =
                [ List.return bridge_decl;
                  List.return (`return_statement (
                    `casted_expression(
                      `identifier bridge_var_name,
                      `type_reference Bridge_labels.concrete_bridge
                    )
                  ));
                ] |> List.concat;
            }
          in
          `value_declaration {
            tsv_modifiers = [ `export ];
            tsv_kind = `const;
            tsv_name = "FullBridge";
            tsv_type_desc = Some (`type_reference full_bridge_type_name);
            tsv_value =
              let tsce_expression, tsce_arguments = match setup_kind with
                | `setup_less -> create_concrete_bridge, []
                | `peer_setup_only -> `identifier (Bridge_labels.Runtime.create_full_bridge_peer_setup_only), [ create_concrete_bridge ]
                | `endemic_setup_only -> `identifier (Bridge_labels.Runtime.create_full_bridge_endemic_setup_only), [ create_concrete_bridge ]
                | `dual_setup -> `identifier (Bridge_labels.Runtime.create_full_bridge_dual_setup), [ create_concrete_bridge ]
              in
              `call_expression { tsce_expression; tsce_arguments }
          })
      in
      [ peer_full_bridge_alias_decl;
        interfaces_namespace;
        endemic_setup_params_type_decl;
        concrete_bridge_type_decl;
        full_bridge_type_decl;
        concrete_bridge_value_decl;
      ] |&?> identity
    in
    [ `import {
        tsi_from = bindoj_runtime_import_location;
        tsi_import_items = [
          { tsii_name = "objintf";
            tsii_alias = Some Bridge_labels.bindoj; }
        ];
      } ]
    @ imports
    @ infile_decls
    @ simple_interfaces
    @ [ `module_declaration {
      tsm_modifiers = [ `export ];
      tsm_name = Bridge_labels.generated_bridge;
      tsm_body = generated_bridge_body;
    }]

module Rope = Rope
module Internals = Internals

let gen_ts_bridge =
  fun ~type_decl_resolution_strategy ~ident_resolution_strategy ~bridgeable_ident_resolver
      ~bindoj_runtime_import_location objintf ->
    let objintf =
      validate_objintf ~bridgeable_ident_resolver objintf
      |> function
      | Ok x -> x
      | Error s -> failwith s
    in

    ts_ast_of_objintf
      ~type_decl_resolution_strategy
      ~ident_resolution_strategy
      ~bridgeable_ident_resolver
      ~bindoj_runtime_import_location
      objintf
    |> Internals.rope_of_ts_ast
    |> Rope.to_string
    |> sprintf "/* eslint-disable @typescript-eslint/no-namespace */\n%s"
