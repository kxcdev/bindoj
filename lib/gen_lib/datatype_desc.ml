open Ppxlib
open Ast_builder.Default

type 'x with_docstr = 'x*[ `docstr of string | `nodoc ]

type record_type_desc = record_field_desc with_docstr list
and record_field_desc = {
  rf_name : string;
  rf_type : string;
}

type variant_type_desc = variant_constructor_desc with_docstr list
and variant_constructor_desc =
  | Cstr_tuple of {
      ct_name : string;
      ct_args : string list;
    }
  | Cstr_record of {
      cr_name : string;
      cr_fields : record_type_desc;
    }

type type_decl = {
  td_name : string;
  td_kind : generic_kind with_docstr;
}
and generic_kind =
  | Record_kind of record_type_desc
  | Variant_kind of variant_type_desc

let type_declaration_of_type_decl : type_decl -> type_declaration =
  fun { td_name=name; td_kind=(kind, doc); } ->
  let loc = Location.none in
  let attributes_of_doc = function
    | `docstr doc ->
      let loc = Location.none in
      [attribute ~loc
         ~name:(Located.mk ~loc "ocaml.doc")
         ~payload:(PStr [pstr_eval ~loc (estring ~loc doc) []])]
    | `nodoc -> []
    | _ -> failwith "unknown polymorphic variant for docstr" in
  let kind_type_of_record_type_desc : record_type_desc -> type_kind = fun fields ->
    Ptype_record (List.map (fun ({rf_name; rf_type;}, doc) ->
        { (label_declaration ~loc ~mutable_:Immutable
             ~name:(Located.mk ~loc rf_name)
             ~type_:(ptyp_constr ~loc (Located.mk ~loc (lident rf_type)) []))
          with pld_attributes = attributes_of_doc doc; })
        fields) in
  let kind_type_of_variant_type_desc : variant_type_desc -> type_kind = fun constrs ->
    Ptype_variant (List.map (function
        | Cstr_tuple {ct_name; ct_args;}, doc ->
          { (constructor_declaration ~loc ~res:None
               ~name:(Located.mk ~loc ct_name)
               ~args:(Pcstr_tuple (List.map (fun ct_arg ->
                   ptyp_constr ~loc (Located.mk ~loc (lident ct_arg)) [])
                   ct_args)))
            with pcd_attributes = attributes_of_doc doc; }
        | Cstr_record {cr_name; cr_fields;}, doc ->
          { (constructor_declaration ~loc ~res:None
               ~name:(Located.mk ~loc cr_name)
               ~args:(match kind_type_of_record_type_desc cr_fields with
                   | Ptype_record fields -> Pcstr_record fields
                   | _ -> failwith "impossible type_kind"))
            with pcd_attributes = attributes_of_doc doc; })
        constrs) in
  { (type_declaration ~loc ~params:[] ~cstrs:[] ~private_:Public ~manifest:None
       ~name:(Located.mk ~loc name)
       ~kind:(match kind with
           | Record_kind record -> kind_type_of_record_type_desc record
           | Variant_kind variant -> kind_type_of_variant_type_desc variant))
    with ptype_attributes = attributes_of_doc doc }

let gen_jsonm_encoder : type_decl -> expression =
  fun { td_name=_; td_kind=(kind, _); } ->
  let loc = Location.none in
  match kind with
  | Record_kind record ->
    let fields = List.map fst record in
    let vari i = "__bindoj_record_field_var_"^string_of_int i in
    let evari i = evar ~loc (vari i) in
    let pvari i = pvar ~loc (vari i) in
    let members = List.mapi (fun i { rf_name; rf_type; } ->
        let name = [%expr `Name [%e estring ~loc rf_name]] in
        if rf_type = "int" then
          (name, [%expr `Float (float_of_int [%e evari i])])
        else if rf_type = "float" then
          (name, [%expr `Float [%e evari i]])
        else if rf_type = "string" then
          (name, [%expr `String [%e evari i]])
        else if rf_type = "bool" then
          (name, [%expr `Bool [%e evari i]])
        else failwith "unsupported record field type") fields in
    let obj =
      List.fold_right
        (fun (n, v) e -> [%expr Seq.cons [%e n] (Seq.cons [%e v] [%e e])])
        members
        [%expr Seq.return `Oe]
      |> fun e -> [%expr Seq.cons `Os [%e e]] in
    let rf_binds = List.mapi (fun i { rf_name; rf_type=_ } ->
        (Located.mk ~loc (lident rf_name), pvari i)) fields in
    [%expr fun [%p ppat_record ~loc rf_binds Closed] -> [%e obj]]
  | Variant_kind _ ->
    failwith "encoder of variant is not implemented."
