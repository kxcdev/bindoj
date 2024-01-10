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
open Bindoj_ppxlib_utils
open Bindoj_objintf_gen_utils
open Bindoj_runtime
open Bindoj_objintf_shared
open Bindoj_typedesc.Typed_type_desc
open Bindoj_codec_config
open Bindoj_common

module Bridge_labels = Bridge_labels

type resolution_strategy = [
  | `no_resolution
  | `inline_type_definition
  | `infile_type_definition of [`path of string | `expr of expression] option
]

let validate_resolution_strategy ~resolution_strategy td codec = 
  let strategy = resolution_strategy td codec in
  let () = match strategy with
  | `no_resolution -> ()
  | `infile_type_definition _ -> ()
  | `inline_type_definition when is_recursive td -> failwith "Recursive type cannot be defined inilne."
  | `inline_type_definition ->
    begin match td.td_kind with
    | Alias_decl _ -> ()
    | Variant_decl _ ->
      begin match Caml_config.get_variant_type td.td_configs with
      | `polymorphic -> ()
      | `regular -> failwith' "type_decl '%s' is not polymorphic variant." td.td_name
      end
    | _ -> failwith' "type_decl '%s' cannot be defined inline." td.td_name
    end
  in
  strategy

let type_of_arrow_with_arguments :
  (arg_label * core_type) list
  -> core_type
  -> core_type =
  let rec go args ret =
    match args with
    | [] -> ret
    | (label, typ) :: ts ->
      Typ.arrow label typ ret |> go ts
  in
  fun args -> go (List.rev args)

let type_of_bridgeable_desc :
  ?attrs:attrs
  -> ([> `simple | `complex ], 'bridgeable_ident) bridgeable_descriptor
  -> core_type
  = fun ?attrs bi ->
    bi
    |> function
    | Sole_method_bridgeable (Simple_method { name; _ } | Complex_method { name; _ }) ->
      typcons ?attrs name
    | Method_bundle_bridgeable { name; configs; _ } ->
      begin match Objintf_config.get_caml_style configs with
      | `object_ -> typcons ?attrs name
      | `module_ -> typpack ?attrs name
      end

let type_of_type_decl :
  ?attrs:attrs
  -> resolution_strategy:(type_decl -> Coretype.codec -> [> `inline_type_definition ])
  -> type_decl
  -> Coretype.codec
  -> core_type =
  fun ?attrs ~resolution_strategy ({ td_name; td_kind; _ } as td) codec ->
    match td_kind with
    | Variant_decl ctors when
      Caml_config.get_variant_type td.td_configs = `polymorphic
      && resolution_strategy td codec = `inline_type_definition
      && not (is_recursive td) ->
        type_of_polymorphic_variant ?attrs ctors
    | Alias_decl ct when resolution_strategy td codec = `inline_type_definition ->
      type_of_coretype ct
    | _ -> typcons ?attrs (type_name_with_codec ~codec td_name)

let type_of_complex_type :
  ?unwrap_bridgeable:bool
  -> ?attrs:attrs
  -> ?self_bridgeable:([> `complex | `simple ], 'bridgeable_ident) bridgeable_descriptor
  -> resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> polarity:party_polarity
  -> 'bridgeable_ident complex_type
  -> core_type =
  fun ?(unwrap_bridgeable=false) ?attrs ?self_bridgeable ~resolution_strategy ~bridgeable_ident_resolver ~polarity ->
  let bridgeable_to_core_type kind bd =
    if unwrap_bridgeable then
      type_of_bridgeable_desc ?attrs bd
    else
      party_with_polarity ~polarity kind
      |> (function `endemic -> "endemic" | `peer -> "peer")
      |> typcons ?attrs ~args:[ type_of_bridgeable_desc bd ]
  in
  function
  | `bridgeable (kind, bi) ->
    bridgeable_ident_resolver bi
    |> complex_descriptor_of_bridgeable
    |> bridgeable_to_core_type kind
  | `self_bridgeable kind ->
    bridgeable_to_core_type kind (self_bridgeable |?! (fun () -> failwith "self_bridgeable is not specified."))
  | `nested ({ td_kind = Variant_decl ctors; _ } as td, codec) when
    Caml_config.get_variant_type td.td_configs = `polymorphic
    && resolution_strategy td codec = `inline_type_definition
    && not (is_recursive td)
    ->
      type_of_polymorphic_variant ?attrs ctors
  | `nested (td, codec) as kind ->
    if is_self_coretype kind then
      failwith "Coretype.Self is not supported.";
    type_of_type_decl ~resolution_strategy td codec
  | `direct ct as kind ->
    if is_self_coretype kind then
      failwith "Coretype.Self is not supported.";
    type_of_coretype ?attrs ct

let type_of_coordinate_desc : coordinate_type_desc -> core_type = function
  | `prim p -> type_of_coretype Coretype.(mk_prim (p :> prim))
  | `string_enum cases -> type_of_coretype (Coretype.mk_string_enum cases)

let gen_method_arrow_type_decl :
  ?self_bridgeable:([< `complex | `simple ], 'bridgeable_ident) bridgeable_descriptor
  -> resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> polarity:party_polarity
  -> is_module_bundle : bool
  -> 'bridgeable_ident complex_type method_descriptor_body
  -> core_type =
  fun ?self_bridgeable ~resolution_strategy ~bridgeable_ident_resolver ~polarity ~is_module_bundle { positional_arguments; labeled_arguments; return_type; _ } ->
    let to_core_type typ =
      type_of_complex_type
        ?self_bridgeable
        ~resolution_strategy
        ~bridgeable_ident_resolver
        ~polarity typ
    in
    type_of_arrow_with_arguments
      ((labeled_arguments |&> (function
          | Larg_regular { label; optional = true; typ; _ } ->
            Optional label, to_core_type typ
          | Larg_regular { label; optional = false; typ; _ } ->
            Labelled label, to_core_type typ
          | Larg_optional_with_default { label; typ; codec; _ } ->
            Optional label, type_of_type_decl ~resolution_strategy (Typed.decl typ) codec
        ))
        @ (positional_arguments |&> (function
          | Parg_regular { typ; _ } ->
            Nolabel, to_core_type typ
        ))
      |> function
        | [] when is_module_bundle -> [ Nolabel, typcons "unit" ]
        | args -> args
      )
      (return_type |> fun (Mret_regular ty) -> to_core_type ty)

let bridgeable_to_structure_item = function
  | `solo_method t -> Str.type_ Nonrecursive [ t ]
  | `method_bundle_object t -> Str.class_type [ t ]
  | `method_bundle_module m -> Str.modtype m

let bridgeable_to_signature_item = function
  | `solo_method t -> Sig.type_ Nonrecursive [ t ]
  | `method_bundle_object t -> Sig.class_type [ t ]
  | `method_bundle_module m -> Sig.modtype m

let doc_of_method_descriptor =
  let (>|=) doc (f: string -> string) =
    match doc with
    | `nodoc -> `nodoc
    | `docstr s -> `docstr (f s)
  in
  fun (desc: _ complex_type method_descriptor_body) ->
  let arg_docs : string list =
    (desc.labeled_arguments |> List.map (function
      | Larg_regular { label; doc; _ }
      | Larg_optional_with_default { label; doc;  _ } ->
        doc >|= sprintf "@param %s %s" label))
    @ (desc.positional_arguments |> List.mapi (fun i -> function
      | Parg_regular { name = None; doc; _ } -> doc >|= sprintf "@param arg%d %s" i
      | Parg_regular { name = Some name; doc; _ } -> doc >|= sprintf "@param %s %s" name))
    |&?> (function `nodoc -> None | `docstr s -> Some s)
  in
  let arg_doc () = arg_docs |> String.concat "\n" in
  (match desc.doc, arg_docs with
  | `nodoc, [] -> `nodoc
  | _, [] -> desc.doc
  | `nodoc, _ -> `docstr (arg_doc ())
  | `docstr s, _ ->
    `docstr (sprintf "%s\n%s" s (arg_doc ()))
  )

let gen_sole_method :
  resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> polarity:party_polarity
  -> 'bi complex_type method_descriptor_body
  -> _ =
  fun ~resolution_strategy ~bridgeable_ident_resolver ~polarity ({ name; _ } as desc) ->
    let attrs = doc_attribute (doc_of_method_descriptor desc) in
    Type.mk ~kind:Ptype_abstract
      ~attrs
      ~manifest:(
        gen_method_arrow_type_decl
          ~resolution_strategy ~polarity
          ~bridgeable_ident_resolver
          ~is_module_bundle:false
          desc)
      (locmk name)
    |> fun t -> `solo_method t

let gen_method_bundle :
  resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> polarity:party_polarity
  -> self_bridgeable:([< `complex | `simple ], 'bridgeable_ident) bridgeable_descriptor
  -> name:string
  -> descs:'bi complex_type method_descriptor_body list
  -> caml_style:Objintf_config.method_bundle_caml_style
  -> doc:doc
  -> _ =
  fun ~resolution_strategy ~bridgeable_ident_resolver ~polarity ~self_bridgeable ~name ~descs ~caml_style ~doc ->
    let attrs = doc_attribute doc in
    match caml_style with
    | `module_ ->
      descs
      |&> (fun ({ name; _ } as desc) ->
        Val.mk ~attrs:(doc_attribute & doc_of_method_descriptor desc)
          (locmk name)
          (gen_method_arrow_type_decl
            (* TODO - implement self_bridgeable for module bundle *)
            ~resolution_strategy ~polarity
            ~bridgeable_ident_resolver
            ~is_module_bundle:true desc)
        |> Sig.value)
      |> fun s ->
         Mtd.mk ~attrs ~typ:(Mty.signature s) (locmk name)
      |> fun x -> `method_bundle_module x
    | `object_ ->
      let fields =
        descs
        |&> (fun ({ name; _ } as desc) ->
          Ctf.method_
            ~attrs:(doc_attribute & doc_of_method_descriptor desc)
            (locmk name) Public Concrete
            (gen_method_arrow_type_decl
              ~self_bridgeable
              ~resolution_strategy ~polarity
              ~bridgeable_ident_resolver
              ~is_module_bundle:false desc))
      in
      Csig.mk (Typ.any ()) fields
      |> Cty.signature
      |> Ci.mk ~attrs (locmk name)
      |> fun (x: class_type_declaration) -> `method_bundle_object x

let gen_bridgeables
  ~resolution_strategy
  ~(bridgeable_ident_resolver: 'bridgeable_ident bridgeable_ident_resolver)
  ~polarity
  (bridgeable: ([< `complex | `simple ], 'bi) bridgeable_descriptor) =
  begin match bridgeable with
  | Sole_method_bridgeable desc ->
    gen_sole_method
      ~resolution_strategy ~bridgeable_ident_resolver
      ~polarity
      (body_of_method_descriptor desc)
  | Method_bundle_bridgeable { name; descs; configs; doc } ->
    let descs = descs |&> body_of_method_descriptor in
    gen_method_bundle
      ~resolution_strategy
      ~bridgeable_ident_resolver
      ~polarity
      ~self_bridgeable:bridgeable
      ~name
      ~descs
      ~caml_style:(Objintf_config.get_caml_style configs) ~doc
  end

let gen_interfaces_of :
  resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> [ `simple | `complex ]
  -> 'bi sameworld_objintf
  -> _ =
  fun ~resolution_strategy ~bridgeable_ident_resolver kind objintf ->
    objintf
    |> map_bridgeables_of kind
      (gen_bridgeables
        ~resolution_strategy
        ~bridgeable_ident_resolver
        ~polarity:objintf.so_polarity)

let gen_simple_interfaces ~resolution_strategy ~bridgeable_ident_resolver objintf =
  gen_interfaces_of ~resolution_strategy ~bridgeable_ident_resolver `simple objintf
  |&> bridgeable_to_structure_item
  |> Mod.structure
  |> Mb.mk (locmk & some Bridge_labels.simple_interfaces)
  |> Str.module_
  |> List.return
and gen_complex_interfaces_signature_item ~resolution_strategy ~bridgeable_ident_resolver objintf =
  gen_interfaces_of ~resolution_strategy ~bridgeable_ident_resolver `complex objintf
  |&> bridgeable_to_signature_item
  |> Mty.signature
  |> Md.mk (locmk & some Bridge_labels.complex_interfaces)
  |> Sig.module_
and gen_complex_interfaces_structure_item ~resolution_strategy ~bridgeable_ident_resolver objintf =
  gen_interfaces_of ~resolution_strategy ~bridgeable_ident_resolver `complex objintf
  |&> bridgeable_to_structure_item
  |> Mod.structure
  |> Mb.mk (locmk & some Bridge_labels.complex_interfaces)
  |> Str.module_

let gen_endemic_object_registry_interfaces :
  resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> 'bridgeable_ident sameworld_objintf
  -> Ppxlib.structure option =
  fun ~resolution_strategy ~bridgeable_ident_resolver objintf ->
  objintf.so_object_registries
  |&?> (fun { ord_name; ord_coordinate_desc; ord_party; ord_typ; ord_doc } ->
    match party_with_polarity ~polarity:objintf.so_polarity ord_party with
    | `peer -> None
    | `endemic ->
      let name = Bridge_labels.get_register_name ord_name in
      let args =
        (ord_coordinate_desc |&> begin fun (label, desc) ->
            Labelled label, type_of_coordinate_desc desc
          end)
        @ [ Nolabel, typcons "option" ~args:[
              type_of_complex_type
                ~unwrap_bridgeable:true
                ~resolution_strategy
                ~bridgeable_ident_resolver
                ~polarity:objintf.so_polarity (ord_typ :> _ complex_type)
            ]]
      in
      Val.mk ~attrs:(doc_attribute ord_doc)
        (locmk name)
        (type_of_arrow_with_arguments
          args
          (typcons "unit"))
      |> Sig.value
      |> some
    )
  |> function
  | [] -> None
  | s ->
      Mtd.mk
        ~typ:(Mty.signature s)
        (locmk Bridge_labels.endemic_object_registry_interface)
  |> Str.modtype
  |> List.return
  |> some

and gen_concrete_bridge :
    resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
    -> bridgeable_ident_resolver:'bi bridgeable_ident_resolver
    -> initial_registry_object_registers_is_generated:bool
    -> 'bi sameworld_objintf
    -> Ppxlib.structure =
  fun ~resolution_strategy ~bridgeable_ident_resolver ~initial_registry_object_registers_is_generated objintf ->
    let loc = Location.none in
    let redefine_module name =
      Mty.alias (lidloc name)
      |> Md.mk (locmk & some name)
      |> Sig.module_
    in
    let peer_object_registry =
      objintf.so_object_registries
      |> List.filter_map (fun ord ->
       match ord.ord_party |> party_with_polarity ~polarity:objintf.so_polarity with
       | `endemic -> None
       | `peer ->
        Val.mk ~attrs:(doc_attribute ord.ord_doc) (locmk & Bridge_labels.get_lookup_name ord.ord_name)
          (type_of_arrow_with_arguments
            (ord.ord_coordinate_desc |&> fun (name, desc) ->
              Labelled name, type_of_coordinate_desc desc)
            (type_of_complex_type
              ~resolution_strategy
              ~bridgeable_ident_resolver
              ~polarity:objintf.so_polarity
              (ord.ord_typ :> _ complex_type)
             |> fun t -> typcons "option" ~args: [ t ])
            )
        |> Sig.value
        |> some
      )
    in
    let peer_objects =
      objintf.so_named_objects
      |> List.filter_map (fun nod ->
        match nod.nod_party |> party_with_polarity ~polarity:objintf.so_polarity with
        | `endemic -> None
        | `peer ->
          Val.mk ~attrs:(doc_attribute nod.nod_doc) (locmk nod.nod_name)
            (type_of_complex_type
              ~resolution_strategy
              ~bridgeable_ident_resolver
              ~polarity:objintf.so_polarity
              (nod.nod_typ :> _ complex_type))
          |> Sig.value
          |> some)
    in
    [
      [%sig:
        open Bindoj_objintf_shared

        type br (** marker type for this specific concrete bridge *)

        type 'x peer = ('x, br) peer'
        type 'x endemic = ('x, br) endemic'
      ];
      [ 
        Sig.value (Val.mk (locmk Bridge_labels.access_peer) [%type: 'x peer -> 'x]);
        Sig.value (Val.mk (locmk Bridge_labels.bridge_endemic) [%type: 'x -> 'x endemic]);
      ];
      [
        redefine_module Bridge_labels.simple_interfaces;
        gen_complex_interfaces_signature_item
          ~resolution_strategy
          ~bridgeable_ident_resolver
          objintf;
        (
          let mods = [ Bridge_labels.simple_interfaces; Bridge_labels.complex_interfaces ] in
          (mods |&> fun name ->
            Mod.ident (lidloc name)
            |> Mty.typeof_
            |> Md.mk (locmk & some name)
            |> Sig.module_)
          @ (mods |&> fun name ->
            Mod.ident (lidloc name)
            |> Mty.typeof_
            |> Incl.mk
            |> Sig.include_)
          |> Mty.signature
          |> Md.mk (locmk & some Bridge_labels.interfaces)
          |> Sig.module_
        )
      ];
      (match peer_object_registry, peer_objects with
        | [], [] -> []
        | _ -> [ Sig.open_ (Opn.mk (lidloc Bridge_labels.interfaces)) ]);
      [
        (peer_object_registry
        |> function
        | [] -> None
        | items -> 
          Mty.signature items
          |> Md.mk (locmk & some Bridge_labels.peer_object_registry)
          |> Sig.module_
          |> some);

        (peer_objects
        |> function
        | [] -> None
        | items ->
          Mty.signature items
          |> Md.mk (locmk & some Bridge_labels.peer_objects)
          |> Sig.module_
          |> some);

        (if initial_registry_object_registers_is_generated then
          Mty.ident (lidloc Bridge_labels.endemic_object_registry_interface)
          |> Md.mk (locmk & some Bridge_labels.endemic_object_registry)
          |> Sig.module_
          |> some
        else
          None)
      ] |&?> identity;
    ]
    |> List.concat
    |> fun s ->
      Mtd.mk
        ~attrs:(doc_attribute objintf.so_doc)
        ~typ:(Mty.signature s)
        (locmk Bridge_labels.concrete_bridge)
  |> Str.modtype
  |> List.return

and gen_concrete_bridge_interfaces :
  resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:'bi bridgeable_ident_resolver
  -> 'bi sameworld_objintf
  -> _ =
  fun ~resolution_strategy ~bridgeable_ident_resolver objintf ->
    let loc = Location.none in
    let redefine_module name =
      Mod.ident (lidloc name)
      |> Mb.mk (locmk & some name)
      |> Str.module_
    in
    [ [%str
        open Bindoj_objintf_shared
      ];
      (Str.type_ Nonrecursive [
        Type.mk
          ~attrs:(doc_attribute & `docstr "marker type for this specific concrete bridge")
          ~kind:(Ptype_variant [
            Type.constructor (locmk Bridge_labels.bridge_marker_ctor)
          ])
          (locmk "br")
      ]
      |> List.return);
      [%str
        type 'x peer = ('x, br) peer'
        type 'x endemic = ('x, br) endemic'

        let access x = access x
        let bridge x = bridge_generic ~bridge:Br x
      ];
      [
        redefine_module Bridge_labels.simple_interfaces;
        gen_complex_interfaces_structure_item
          ~resolution_strategy
          ~bridgeable_ident_resolver
          objintf;
        ( let mods = [ Bridge_labels.simple_interfaces; Bridge_labels.complex_interfaces ] in
          (mods |&> fun name ->
            Mod.ident (lidloc name)
            |> Mb.mk (locmk & some name)
            |> Str.module_)
          @ (mods |&> fun name ->
            Mod.ident (lidloc name)
            |> Incl.mk
            |> Str.include_)
          |> Mod.structure
          |> Mb.mk (locmk & some Bridge_labels.interfaces)
          |> Str.module_);
      ];
    ] |> List.concat
    |> Mod.structure
    |> Mb.mk (locmk & some Bridge_labels.concrete_bridge_interfaces)
    |> Str.module_
    |> List.return

and to_full_bridge_kind ?param_name ~resolution_strategy ~bridgeable_ident_resolver objintf =
  let functor_param  =
    let polarity = objintf.so_polarity in
    (objintf.so_named_objects
      |&?> fun nod ->
        match party_with_polarity ~polarity nod.nod_party with
        | `peer -> None
        | `endemic ->
          Val.mk
            ~attrs:(doc_attribute nod.nod_doc)
            (locmk nod.nod_name)
            ( (* Concrete_bridge is not created at this time, so unwrap peer/endemic. *)
              type_of_complex_type ~unwrap_bridgeable:true
                ~resolution_strategy
                ~bridgeable_ident_resolver
                ~polarity
                (nod.nod_typ :> _ complex_type))
          |> some)
      @ ( if objintf.so_object_registries
            |> List.exists (fun { ord_party; _ } ->
              party_with_polarity ~polarity ord_party = `endemic
            )
          then
            Val.mk (locmk Bridge_labels.initially_registry_objects)
            (typpack Bridge_labels.endemic_object_registry_interface
              |> Fn.flip (Typ.arrow Nolabel) (typcons "unit"))
            |> List.return
          else [])
      |> function
      | [] -> None
      | ps ->
        Some (Named(locmk & param_name, Mty.signature (ps |&> Sig.value)))
  in
  match functor_param, peer_setup_needed objintf with
  | None, false -> `setup_less
  | None, true -> `peer_setup_only
  | Some p, false -> `endemic_setup_only p
  | Some p, true -> `dual_setup p

let gen_full_bridge :
  resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
    -> bridgeable_ident_resolver:'bi bridgeable_ident_resolver
    -> 'bi sameworld_objintf
    -> Ppxlib.structure =
  fun ~resolution_strategy ~bridgeable_ident_resolver objintf ->
    let loc = Location.none in
    let name, mty =
      let endemic_full_bridge () =
        Val.mk (locmk Bridge_labels.endemic_full_bridge) [%type: Bindoj_objintf_shared.endemic_full_bridge_reference]
        |> Sig.value
      in
      let peer_setup_bridge () =
        let cb = typpack Bridge_labels.concrete_bridge in
        [ Mtd.mk ~typ:(Mty.ident (lidloc Bridge_labels.concrete_bridge)) (locmk Bridge_labels.bridge)
          |> Sig.modtype ]
        @ [%sig:
          val get_bridge : unit -> [ `await_peer_setup | `bridge of [%t cb] ]
          val get_bridge_async : ([%t cb] -> unit) -> unit
        ] @ [ endemic_full_bridge () ]
        |> Mty.signature
      in
      match to_full_bridge_kind ~resolution_strategy ~bridgeable_ident_resolver objintf with
      | `setup_less -> Bridge_labels.setup_less_full_bridge, Mty.ident (lidloc Bridge_labels.concrete_bridge)
      | `peer_setup_only -> Bridge_labels.peer_setup_only_full_bridge, peer_setup_bridge ()
      | `endemic_setup_only p ->
        Bridge_labels.endemic_setup_only_full_bridge, Mty.functor_ p (
          Mty.signature [
            Mty.ident (lidloc Bridge_labels.concrete_bridge)
            |> Incl.mk
            |> Sig.include_;
            endemic_full_bridge ();
          ]
        )
      | `dual_setup p ->
        Bridge_labels.dual_setup_full_bridge, Mty.functor_ p & peer_setup_bridge ()
    in
    Mtd.mk ~typ:mty (locmk name)
    |> Str.modtype
    |> List.return

let gen_structure :
  ?generators:(
    resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
    -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
    -> 'bridgeable_ident sameworld_objintf
    -> Ppxlib.structure
  ) list
  -> ?resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:'bridgeable_ident bridgeable_ident_resolver
  -> 'bridgeable_ident sameworld_objintf
  -> Ppxlib.structure =
  fun ?(generators=[]) ?resolution_strategy ~bridgeable_ident_resolver objintf ->
    let objintf =
      validate_objintf ~bridgeable_ident_resolver objintf
      |> function
      | Ok x -> x
      | Error s -> failwith s
    in
    let resolution_strategy = resolution_strategy |? (fun _ _ -> `infile_type_definition None) in
    let infile_defined_tds =
      extract_type_decls objintf
      |&?> (fun (td, codec) ->
        match validate_resolution_strategy ~resolution_strategy td codec with
        | `no_resolution
        | `inline_type_definition -> None
        | `infile_type_definition _ -> Some (td, codec, None))
    in
    let initial_registry_object_registers =
      gen_endemic_object_registry_interfaces
        ~resolution_strategy ~bridgeable_ident_resolver
        objintf
    in
    (infile_defined_tds
      |&>> fun (td, codec, type_decl) ->
        Bindoj_gen.(
          Caml_datatype.gen_structure
            ?type_decl
            ~codec
            ~generators:[
              Json_codec.gen_json_codec
                ~self_contained:true
                ~gen_json_shape_explanation:true
                ?json_shape_explanation_resolution:None
                ~discriminator_value_accessor:true;
            ]
            td
        ))
    @ gen_simple_interfaces ~resolution_strategy ~bridgeable_ident_resolver objintf
    @ [ Str.open_
          & Opn.mk ~attrs:(warning_attribute "-33") (* suppress 'unused open' warning *)
            & Mod.ident (lidloc Bridge_labels.simple_interfaces) ] 
    @ (initial_registry_object_registers |? [])
    @ gen_concrete_bridge
      ~resolution_strategy ~bridgeable_ident_resolver
      ~initial_registry_object_registers_is_generated:(Option.is_some initial_registry_object_registers)
      objintf
    @ gen_concrete_bridge_interfaces
      ~resolution_strategy ~bridgeable_ident_resolver
      objintf
    @ [
      Str.open_
        & Opn.mk ~attrs:(warning_attribute "-33") (* suppress 'unused open' warning *)
          & Mod.ident (lidloc Bridge_labels.(concrete_bridge_interfaces^"."^interfaces))
    ]
    @ gen_full_bridge ~resolution_strategy ~bridgeable_ident_resolver objintf
    @ (generators |> List.concat_map(fun gen -> gen ~resolution_strategy ~bridgeable_ident_resolver objintf))

type builtin_codec = {
  encoder: expression;
  decoder: expression;
}

module type Full_bridge_impl = sig
  type bridgeable_ident
  val get_module_name : string -> string
  val get_encoder_name : string -> string
  val get_decoder_name : string -> string

  val gen_ensure_peer_setup : unit -> expression
  val gen_setup_common_codecs : unit -> value_binding list

  val gen_builtin_codec : string -> builtin_codec option
  val gen_type_decl_encoder : type_decl -> Coretype.codec -> value_binding option
  val gen_type_decl_decoder : type_decl -> Coretype.codec -> value_binding option
  val gen_encoder : ([> `simple | `complex ], bridgeable_ident) bridgeable_descriptor -> expression
  val gen_decoder : ([> `simple | `complex ], bridgeable_ident) bridgeable_descriptor -> expression

  val gen_peer_object : ([ `endemic | `peer ], bridgeable_ident) named_object_decl -> expression
  val gen_setup_endemic_objects : unit -> expression
  val gen_setup_peer_object_registry : unit -> expression
  val gen_setup_endemic_object_registry : unit -> expression

  val gen_endemic_full_bridge : setup_called:expression -> setup:expression -> expression
end

type 'bridgeable_ident full_bridge_impl = (module Full_bridge_impl with type bridgeable_ident = 'bridgeable_ident)

type collecting_codec_kind = [ `p2e | `e2p | `encoder | `decoder ]

let gen_full_bridge_impl : type bridgeable_ident.
  resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
  -> bridgeable_ident_resolver:bridgeable_ident bridgeable_ident_resolver
  -> impl:(
    resolution_strategy:(type_decl -> Coretype.codec -> resolution_strategy)
    -> bridgeable_ident_resolver:bridgeable_ident bridgeable_ident_resolver
    -> bridgeable_ident sameworld_objintf
    -> (module Full_bridge_impl with type bridgeable_ident = bridgeable_ident))
  -> bridgeable_ident sameworld_objintf
  -> structure =
  fun ~resolution_strategy ~bridgeable_ident_resolver ~impl objintf ->
    let objintf =
      validate_objintf ~bridgeable_ident_resolver objintf
      |> function
      | Ok x -> x
      | Error s -> failwith s
    in
    let loc = Location.none in
    let polarity = objintf.so_polarity in
    let open (
      val impl ~resolution_strategy ~bridgeable_ident_resolver objintf
      : (Full_bridge_impl with type bridgeable_ident = bridgeable_ident))
    in
    
    let include_concrete_bridge_interfaces =
      Incl.mk (Mod.ident & lidloc Bridge_labels.concrete_bridge_interfaces)
      |> Str.include_
      |> List.return
    in

    let endemic_full_bridge_impl setup_impl =
      [%stri
        let endemic_full_bridge = [%e
          gen_endemic_full_bridge
            ~setup_called:[%expr !ref_setup_called]
            ~setup:
              [%expr
                let () =
                  if !ref_setup_called then
                    raise Bindoj_objintf_shared.Full_Bridge_already_setup;
                  ref_setup_called := true
                in
                [%e setup_impl]
              ]
        ]
      ]
    in

    (* collect codecs to be generated *)
    let builtin_codecs, type_decl_codecs, simple_interfaces_codecs, complex_interfaces_codecs =
      let cons_codecs_of_typ ?self_bridgeable kind party typ (bs, ts, ss, cs) =
        match typ with
        | (`nested _ | `direct _) as typ when is_self_coretype typ ->
          failwith "Coretype.Self is not supported."
        | `nested td ->
          bs, ((kind, party), td) :: ts, ss, cs
        | `direct ct -> ((kind, party), `coretype ct) :: bs, ts, ss, cs
        | `bridgeable (_, bi) ->
          let bd = bridgeable_ident_resolver bi in
          let name = bd |> complex_descriptor_of_bridgeable |> name_of_bridgeable in
          begin match bd with
          | Simple_bridgeable { desc } -> bs, ts, ((kind, party), (name, desc)) :: ss, cs
          | Complex_bridgeable { desc } ->
            let desc =
              ((* TODO - we need type ninja to figure this out. *)
                Obj.magic (desc: ( [< `complex | `simple ], bridgeable_ident ) bridgeable_descriptor)
                  : ([ `complex | `simple ], bridgeable_ident) bridgeable_descriptor)
            in
            bs, ts, ss, ((kind, party), (name, desc)) :: cs
          end
        | `self_bridgeable _ ->
          let desc = self_bridgeable |?! (fun () -> failwith "self_bridgeable is not specified.") in
          let name = name_of_bridgeable desc in
          (* `self_bridgelabe is only allowed for Complex_bridgeable. *)
          bs, ts, ss, ((kind, party), (name, desc)) :: cs
      in
      ([], [], [], [])
      |> Fn.flip (List.foldl (fun state { ord_party; ord_coordinate_desc; ord_typ; _ } ->
        let bs, ts, ss, cs = cons_codecs_of_typ `e2p ord_party ord_typ state in
        let bs =
          (ord_coordinate_desc
            |&> Coretype.(let kind = `decoder, ord_party in function
              | _, `prim p -> kind, `coretype_desc (Prim (p :> prim))
              | _, `string_enum cases -> kind, `coretype_desc (StringEnum cases)))
          @ bs
        in
        bs, ts, ss, cs
      )) objintf.so_object_registries
      (* The types of object registries are collected. *)

      |> Fn.flip (List.foldl (fun state { nod_party; nod_typ; _ } ->
          cons_codecs_of_typ `e2p nod_party nod_typ state
        )) objintf.so_named_objects
      (* The types of named objects are collected. *)

      |> fun (bs, ts, ss, cs) ->
        (* Indirectly referenced types are collected. *)
        let rec go =
          let collect_types_of_bridgeable
            (kind, party)
            (desc: ( [ `complex | `simple ], bridgeable_ident ) bridgeable_descriptor)
            =
            (* Encoded method arguments must be decoded and vice versa. *)
            let arg_codec_kind =
              match (kind: collecting_codec_kind) with
              | `p2e -> `e2p
              | `e2p -> `p2e
              | x -> x
            in
            let method_bodies =
              (match desc with 
              | Sole_method_bridgeable desc -> [ desc ]
              | Method_bundle_bridgeable { descs; _ } -> descs)
              |&> (function
                | Simple_method x -> (x :> bridgeable_ident complex_type method_descriptor_body)
                | Complex_method x -> x)
            in
            List.foldl (fun state { positional_arguments; labeled_arguments; return_type; _ } ->
              let arg_types =
                (positional_arguments |&> (function | Parg_regular { typ; _ } -> typ))
                @ (labeled_arguments |&?> (function
                  | Larg_regular { typ; _ } -> Some typ
                  | Larg_optional_with_default _ -> None))
              in
              let ret_type =
                match return_type with
                | Mret_regular typ -> typ
              in
              state
              |> Fn.flip (List.foldl (fun state typ -> cons_codecs_of_typ ~self_bridgeable:desc arg_codec_kind party typ state)) arg_types
              |> cons_codecs_of_typ ~self_bridgeable:desc kind party ret_type
            ) ([], [], [], []) method_bodies
          in
          let cons_if_not_existed_by f =
            fun x xs ->
            if List.exists (f &> (=) (f x)) xs then xs else x :: xs
          in
          let get_equality_key = fun (k, (l, _)) -> (k, l) in
          fun ((rbs, rts, rss, rcs) as result) (pss, pcs) (ss, cs) ->
            match ss, cs with
            (* If the stack is empty, the process ends. *)
            | [], [] -> result

            (* Types indirectly referenced by bridgeable decl in simple interfaces are collected. *)
            | ((codec_kind, (_, desc)) as s):: ss, _ ->
              let collected_bs, collected_ts, collected_ss, collected_cs =
                collect_types_of_bridgeable codec_kind
                  ((* TODO - we need type ninja to figure this out. *)
                    Obj.magic (desc : ([ `simple ], bridgeable_ident) bridgeable_descriptor)
                      : ([ `simple | `complex], bridgeable_ident) bridgeable_descriptor)
              in

              (* Validation *)
              (match collected_ss, collected_cs with
              | [], [] -> ()
              | _ -> failwith "Simple_bridgeable does not refer to other bridgeable_decl.");

              let rbs, rts = collected_bs @ rbs, collected_ts @ rts in

              let rss = rss |> cons_if_not_existed_by get_equality_key s in

              go (rbs, rts, rss, rcs) (s :: pss, pcs) (ss, cs)
            
            (* Types indirectly referenced by bridgeable decl in complex interfaces are collected. *)
            | [], (((codec_kind, (_, desc)) as c) :: cs) ->
              let collected_bs, collected_ts, collected_ss, collected_cs =
                collect_types_of_bridgeable codec_kind desc
              in
              let append xs appended processed =
                List.foldl (fun xs x ->
                  let k = get_equality_key x in
                  if List.exists (get_equality_key &> (=) k) processed then
                    xs
                  else
                    cons_if_not_existed_by get_equality_key x xs)
                xs appended
              in
              let rbs, rts, ss, cs =
                collected_bs @ rbs,
                collected_ts @ rts,
                append ss collected_ss pss,
                append cs collected_cs pcs
              in
              let rcs = rcs |> cons_if_not_existed_by get_equality_key c in
              go (rbs, rts, rss, rcs) (pss, c :: pcs) (ss, cs)
        in
        let bs, ts, ss, cs = go (bs, ts, [], []) ([], []) (ss, cs) in
        let codec_kind_with_party (kind, party) =
          match party_with_polarity ~polarity party, kind with
          | `peer, `p2e | `endemic, `e2p -> `encoder
          | `peer, `e2p | `endemic, `p2e -> `decoder
          | _, `encoder -> `encoder
          | _, `decoder -> `decoder
        in
        let builtin_codecs =
          bs
          |> List.partition_map (?< codec_kind_with_party &> function
            | `encoder, ct -> Either.left ct
            | `decoder, ct -> Either.right ct)
          |> fun (es, ds) ->
            let to_value_binding get_name get_codec codecs =
              codecs
              |> List.foldl (fun state ct ->
                let json_tuple_style, desc =
                  match ct with
                  | `coretype { Coretype.ct_configs; ct_desc } ->
                    some & Json_config.get_tuple_style ct_configs, ct_desc
                  | `coretype_desc desc -> None, desc
                in
                Json_config.Bindoj_private.collect_coretypes_folder
                  ?json_tuple_style
                  ~including_optional_fields:false
                  ((function
                    | `prim s -> Some s
                    | `uninhabitable -> Some "uninhabitable"
                    | `option -> Some "option"
                    | `list -> Some "list"
                    | `map -> Some "map"
                    | `ident _ | `string_enum | `self -> None)
                    &> Fn.flip Option.bind gen_builtin_codec
                    &> Option.map get_codec)
                  state desc
                ) StringMap.empty
              |> StringMap.to_list
              |&> (fun (label, expr) ->
                Vb.mk
                  ~attrs:(warning_attribute "-32") (* suppress 'unused value' warning. *)
                  (pvar & get_name label)
                  expr)
            in
            (to_value_binding get_encoder_name (fun x -> x.encoder) es)
            @ (to_value_binding get_decoder_name (fun x -> x.decoder) ds)
        in
        let type_decl_codecs =
          ts
          |&> (?< codec_kind_with_party)
          |> Bindoj_list.uniq_by (fun (codec, ({ td_name; _ }, _)) -> (codec, td_name))
          |&?> (function
            | `encoder, (td, codec) -> gen_type_decl_encoder td codec
            | `decoder, (td, codec) -> gen_type_decl_decoder td codec)
        in
        let to_codecs ?attrs codecs : value_binding list =
          codecs
          |&> (?< codec_kind_with_party)
          |> Bindoj_list.uniq_by (fun (codec, (l, _)) -> (codec, l))
          |&> ((function
            | `encoder, (label, bd) -> get_encoder_name label, gen_encoder bd
            | `decoder, (label, bd) -> get_decoder_name label, gen_decoder bd)
            &> fun (label, expr) ->
              Vb.mk ?attrs (pvar label) expr)
        in
        builtin_codecs,
        type_decl_codecs,
        to_codecs
          ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
          ((* TODO - we need type ninja to figure this out. *)
            Obj.magic (ss : ((collecting_codec_kind * [< `endemic | `peer ]) * (label * ([ `simple ], bridgeable_ident) bridgeable_descriptor)) list)
            : ((collecting_codec_kind * [< `endemic | `peer ]) * (label * ([ `complex | `simple ], bridgeable_ident) bridgeable_descriptor)) list),
        to_codecs
          ~attrs:(warning_attribute "-39") (* suppress 'unused rec' warning *)
          cs
    in

    let init_registry_objects ident =
      [%expr
        [%e evar & Bridge_labels.functor_parameter_var ^ "." ^ Bridge_labels.initially_registry_objects]
          [%e Exp.pack (Mod.ident (lidloc & ident))];
      ]
    in

    let gen_object_registy
      ~gen_lookup
      ~gen_register
      party
      gen_setup =
      [%str open Bindoj_runtime]
      @ (objintf.so_object_registries
        |> List.concat_map (function
          | { ord_name; ord_coordinate_desc; ord_party; _ }
            when party_with_polarity ~polarity ord_party = party ->
            let registry_name = Bridge_labels.get_registry_name ord_name in
            let key_expr =
              ord_coordinate_desc |&> (fun (label, desc) ->
                encoded_map_key_expression
                  (coordinate_desc_to_map_key_type_desc desc)
                  (evar label))
                |> function
                | [] -> failwith "At least one key must be specified for the coordinate desc."
                | [ e ] -> e
                | es -> [%expr String.concat "" [%e elist ~loc es]]
            in
            
            let mk_fun_body body =
              let rec go body = function
                | [] -> body
                | (label, _) :: xs ->
                  (Exp.fun_ (Labelled label) None (pvar label) (go body xs))
              in
              go body ord_coordinate_desc
            in
            [%str
              let [%p pvar registry_name] = ref StringMap.empty
            ] @ [
              Str.value Nonrecursive ([
                (if gen_lookup then Some (
                  Vb.mk (pvar & Bridge_labels.get_lookup_name ord_name)
                  ( mk_fun_body [%expr
                  ![%e evar registry_name]
                  |> StringMap.find_opt (Map_key.([%e key_expr]))
                  ])
                ) else None);
                (if gen_register then Some (
                  Vb.mk (pvar & Bridge_labels.get_register_name ord_name)
                  (mk_fun_body [%expr
                    fun value -> [%e evar registry_name] :=
                      ![%e evar registry_name]
                      |> StringMap.update (Map_key.([%e key_expr])) (fun _ -> value)
                  ])
                ) else None)
              ] |&?> identity)
            ]
          | _ -> []))
      @ (match gen_setup with
        | None -> []
        | Some setup -> [
          Str.value Nonrecursive [ Vb.mk [%pat? ()] setup ]
        ])
    in

    let no_empty_value rec_flag = function
      | [] -> None
      | vs -> Some (Str.value rec_flag vs)
    in

    let peer_setup_bridge_impl items init_registry_objects =
      [
        Mtd.mk (locmk Bridge_labels.bridge) ~typ:(Mty.ident (lidloc Bridge_labels.concrete_bridge))
        |> Str.modtype
      ]
      @ [%str
        let ref_setup_called = ref false
        let bridge_opt: (module Bridge) option ref = ref None
        let continuations = ref []

        let get_bridge () =
          match !bridge_opt with
          | None -> `await_peer_setup
          | Some b -> `bridge b

        let get_bridge_async f =
          match !bridge_opt with
          | None -> continuations := f :: !continuations
          | Some b -> f b
      ]
      @ [
        [%expr
          [%e gen_ensure_peer_setup ()];
          let concrete_bridge =
            [%e
              include_concrete_bridge_interfaces
              @ ([
                (* open Interfaces *)
                some & Str.open_
                  & Opn.mk
                    ~attrs:(warning_attribute "-33") (* suppress 'unused open' warning *)
                    (Mod.ident  & lidloc Bridge_labels.interfaces);

                no_empty_value Nonrecursive (gen_setup_common_codecs ());
                no_empty_value Nonrecursive builtin_codecs;
                no_empty_value Recursive type_decl_codecs;
                no_empty_value Recursive simple_interfaces_codecs;
                no_empty_value Recursive complex_interfaces_codecs;

                (* Peer_object_registry *)
                (gen_object_registy
                  ~gen_lookup:true
                  ~gen_register:true
                  `peer (some & gen_setup_peer_object_registry ())
                  |> Mod.structure
                  |> Mb.mk (locmk & some Bridge_labels.peer_object_registry)
                  |> Str.module_
                  |> some)
                ;

                (* Peer_objects *)
                ((objintf.so_named_objects
                  |&?> (function
                    | { nod_name; nod_party; _ } as nod
                      when party_with_polarity ~polarity nod_party = `peer ->
                      some & Vb.mk
                        (pvar nod_name)
                        (gen_peer_object nod)
                    | _ -> None))
                  |> function
                  | [] -> None
                  | vbs ->
                    Str.value Nonrecursive vbs
                    |> List.return
                    |> Mod.structure
                    |> Mb.mk (locmk & some Bridge_labels.peer_objects)
                    |> Str.module_
                    |> some);

              ] |&?> identity) @ items
              |> Mod.structure
              |> Exp.pack
              |> Fn.flip Exp.constraint_ (typpack Bridge_labels.bridge)
            ]
          in
          [%e 
            let set_concrete_bridge =
              [%expr
                bridge_opt := Some concrete_bridge;
                !continuations |> List.iter (fun f -> f concrete_bridge);
                continuations := []]
            in
            match init_registry_objects with
            | None -> set_concrete_bridge
            | Some e -> Exp.sequence e set_concrete_bridge]
        ]
        |> endemic_full_bridge_impl
      ]
    in

    let setup_endemic =
      Exp.sequence
        (gen_setup_endemic_object_registry ()
        |> Exp.open_ (Opn.mk & Mod.ident & lidloc Bridge_labels.endemic_object_registry))
        (gen_setup_endemic_objects ())
    in

    let endemic_object_registry () =
      gen_object_registy
        ~gen_lookup:true
        ~gen_register:true
        `endemic None
      |> Mod.structure
      |> Mb.mk (locmk & some Bridge_labels.endemic_object_registry)
      |> Str.module_
    in

    let mty_name, (str: module_expr) =
      match to_full_bridge_kind ~param_name:Bridge_labels.functor_parameter_var ~resolution_strategy ~bridgeable_ident_resolver objintf with
      | `setup_less ->
        Bridge_labels.setup_less_full_bridge, Mod.structure (include_concrete_bridge_interfaces)
      | `peer_setup_only ->
        Bridge_labels.peer_setup_only_full_bridge, Mod.structure & peer_setup_bridge_impl [] None
      | `endemic_setup_only p ->
        Bridge_labels.endemic_setup_only_full_bridge, Mod.functor_ p (
          include_concrete_bridge_interfaces
          @ ([
            no_empty_value Nonrecursive (gen_setup_common_codecs ());
            no_empty_value Nonrecursive builtin_codecs;
            no_empty_value Recursive type_decl_codecs;
            no_empty_value Recursive simple_interfaces_codecs;
            no_empty_value Recursive complex_interfaces_codecs;
          ] |&?> identity)
          @ [%str
            let ref_setup_called = ref false
          ]
          @ [ endemic_object_registry ();
              setup_endemic
              |> endemic_full_bridge_impl
          ]
          @ [%str
            let () = [%e init_registry_objects Bridge_labels.endemic_object_registry]
          ]
          |> Mod.structure
        )
      | `dual_setup p ->
        Bridge_labels.dual_setup_full_bridge, Mod.functor_ p (
          peer_setup_bridge_impl [ endemic_object_registry (); [%stri let () = [%e setup_endemic]] ]
            (some [%expr
              concrete_bridge |> fun (module B) ->
                [%e init_registry_objects & "B." ^ Bridge_labels.endemic_object_registry]
            ])
          |> Mod.structure
        )
    in

    Mod.constraint_ str (Mty.ident (lidloc mty_name))
    |> Mb.mk (locmk & some (get_module_name "Full_bridge"))
    |> Str.module_
    |> List.return
