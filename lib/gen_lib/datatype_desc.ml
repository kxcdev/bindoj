open Ppxlib
open Ast_builder.Default
open Kxclib

let locmk = Located.mk

type 'x with_docstr = 'x*[ `docstr of string | `nodoc ]
let attributes_of_doc = function
  | `docstr doc ->
    let loc = Location.none in
    [attribute ~loc
       ~name:(Located.mk ~loc "ocaml.doc")
       ~payload:(PStr [pstr_eval ~loc (estring ~loc doc) []])]
  | `nodoc -> []
  | _ -> failwith "unknown polymorphic variant for docstr"

type codec = [ `default_codec | `codec_val of string | `codec_in_module of string ]
let encoder_name type_name = function
  | `default_codec -> "encode_"^type_name^"_json"
  | `codec_val v -> v
  | `codec_in_module m -> m^".encode_"^type_name^"_json"
let _decoder_name type_name = function
  | `default_codec -> "decode_"^type_name^"_json"
  | `codec_val v -> v
  | `codec_in_module m -> m^".decode_"^type_name^"_json"

type record_type_desc = record_field_desc with_docstr list
and record_field_desc = {
  rf_name : string;
  rf_type : string;
  rf_codec : codec;
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

type generic_kind =
  | Record_kind of record_type_desc
  | Variant_kind of variant_type_desc

type type_decl = {
  td_name : string;
  td_kind : generic_kind with_docstr;
}

let rec type_declaration_of_type_decl : type_decl -> type_declaration = fun { td_name; td_kind; } ->
  let loc = Location.none in
  let (kind, doc) = td_kind in
  { (type_declaration ~loc ~params:[] ~cstrs:[] ~private_:Public ~manifest:None
       ~name:(locmk ~loc td_name)
       ~kind:(type_kind_of_generic_kind kind))
    with ptype_attributes = attributes_of_doc doc }

and type_kind_of_generic_kind : generic_kind -> type_kind = function
  | Record_kind record ->
    Ptype_record (record |&> fun (field, doc) ->
        { (label_declaration_of_record_field_desc field)
          with pld_attributes = attributes_of_doc doc; })
  | Variant_kind variant ->
    Ptype_variant (variant |&> fun (constructor, doc) ->
        { (constructor_declaration_of_variant_constructor_desc constructor)
          with pcd_attributes = attributes_of_doc doc; })

and label_declaration_of_record_field_desc : record_field_desc -> label_declaration =
  fun { rf_name; rf_type; rf_codec=_; } ->
  let loc = Location.none in
  label_declaration ~loc ~mutable_:Immutable
    ~name:(locmk ~loc rf_name)
    ~type_:(ptyp_constr ~loc (locmk ~loc (lident rf_type)) [])

and constructor_declaration_of_variant_constructor_desc : variant_constructor_desc -> constructor_declaration =
  let loc = Location.none in
  function
  | Cstr_tuple { ct_name; ct_args; } ->
    constructor_declaration ~loc ~res:None
      ~name:(locmk ~loc ct_name)
      ~args:(Pcstr_tuple (ct_args |&> fun arg ->
          ptyp_constr ~loc (locmk ~loc (lident arg)) []))
  | Cstr_record { cr_name; cr_fields; } ->
    constructor_declaration ~loc ~res:None
      ~name:(locmk ~loc cr_name)
      ~args:(match type_kind_of_generic_kind (Record_kind cr_fields) with
          | Ptype_record fields -> Pcstr_record fields
          | _ -> failwith "type_kind but record is invalid in record constructor declaration")

(*
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
    Ptype_record (fields |&> fun ({ rf_name; rf_type; rf_codec=_; }, doc) ->
        { (label_declaration ~loc ~mutable_:Immutable
             ~name:(Located.mk ~loc rf_name)
             ~type_:(ptyp_constr ~loc (Located.mk ~loc (lident rf_type)) []))
          with pld_attributes = attributes_of_doc doc; }) in
  let kind_type_of_variant_type_desc : variant_type_desc -> type_kind = fun constrs ->
    Ptype_variant (constrs |&> function
        | Cstr_tuple { ct_name; ct_args; }, doc ->
          { (constructor_declaration ~loc ~res:None
               ~name:(Located.mk ~loc ct_name)
               ~args:(Pcstr_tuple (ct_args |&> fun ct_arg ->
                   ptyp_constr ~loc (Located.mk ~loc (lident ct_arg)) [])))
            with pcd_attributes = attributes_of_doc doc; }
        | Cstr_record { cr_name; cr_fields; }, doc ->
          { (constructor_declaration ~loc ~res:None
               ~name:(Located.mk ~loc cr_name)
               ~args:(match kind_type_of_record_type_desc cr_fields with
                   | Ptype_record fields -> Pcstr_record fields
                   | _ -> failwith "impossible type_kind"))
            with pcd_attributes = attributes_of_doc doc; }) in
  { (type_declaration ~loc ~params:[] ~cstrs:[] ~private_:Public ~manifest:None
       ~name:(Located.mk ~loc name)
       ~kind:(match kind with
           | Record_kind record -> kind_type_of_record_type_desc record
           | Variant_kind variant -> kind_type_of_variant_type_desc variant))
    with ptype_attributes = attributes_of_doc doc }
*)

let gen_primitive_encoders : codec -> value_binding list = fun codec ->
  let loc = Location.none in
  let bind str expr =
    value_binding ~loc ~pat:(pvar ~loc str) ~expr:expr in
  [bind (encoder_name "null" codec)
     [%expr fun () -> (`null : Kxclib.Json.jv)];
   bind (encoder_name "bool" codec)
     [%expr fun (__bindoj_arg : bool) -> (`bool __bindoj_arg : Kxclib.Json.jv)];
   bind (encoder_name "int" codec)
     [%expr fun (__bindoj_arg : int) -> (`num (float_of_int __bindoj_arg) : Kxclib.Json.jv)];
   bind (encoder_name "float" codec)
     [%expr fun (__bindoj_arg : float) -> (`num __bindoj_arg : Kxclib.Json.jv)];
   bind (encoder_name "string" codec)
     [%expr fun (__bindoj_arg : string) -> (`str __bindoj_arg : Kxclib.Json.jv)]]

let gen_json_encoder : type_decl -> codec -> value_binding =
  fun { td_name; td_kind=(kind, _); } codec ->
  let loc = Location.none in
  let encoder_name = begin match codec with
    | `default_codec -> "encode_"^td_name^"_json"
    | `codec_val codec -> codec
    | `codec_in_module codec -> codec^".encode_"^td_name^"_json"
  end |> (fun name ->
      ppat_constraint ~loc
        (pvar ~loc name)
        [%type: [%t (ptyp_constr ~loc (Located.mk ~loc (lident td_name)) [])] -> Kxclib.Json.jv]) in
  let erecord record =
    let fields = record |&> fst in
    let vari i = "__bindoj_record_field_var_"^string_of_int i in
    let evari i = evar ~loc (vari i) in
    let pvari i = pvar ~loc (vari i) in
    let members = List.mapi (fun i { rf_name; rf_type; rf_codec; } ->
        let name = estring ~loc rf_name in
        let field_encoder_name = match rf_codec with
          | `default_codec -> "encode_"^rf_type^"_json"
          | `codec_val codec -> codec
          | `codec_in_module codec -> codec^".encode_"^rf_type^"_json" in
        (name, [%expr [%e evar ~loc field_encoder_name] [%e evari i]]))
        fields in
    let obj =
      List.fold_right
        (fun (n, v) e -> [%expr ([%e n], [%e v]) :: [%e e]])
        members
        [%expr []] in
    let rf_binds = List.mapi (fun i { rf_name; rf_type=_; rf_codec=_; } ->
        (Located.mk ~loc (lident rf_name), pvari i)) fields in
    [%expr (fun [%p ppat_record ~loc rf_binds Closed] -> (`obj [%e obj] : Kxclib.Json.jv))] in
  match kind with
  | Record_kind record -> value_binding ~loc ~pat:encoder_name ~expr:(erecord record)
  | Variant_kind _variant -> failwith "noimpl: variant type encoder"
(*
    let clauses = variant |&> function
        | Cstr_tuple { ct_name; ct_args; }, _ ->
          case
            ~lhs:(pconstruct { pcd_name = (Located.mk ct_name);
                               pcd_args = Pcstr_tuple (ct_args |&> (fun arg ->
                                   ptyp_constr ~loc (Located.mk ~loc (lident arg)) []));
        | Cstr_record { cr_name; cr_fields; }, _ ->
          value_binding ~loc
            ~pat:encoder_name
            ~expr:[%expr ]
*)

