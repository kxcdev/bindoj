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
      ct_codec : codec;
    }
  | Cstr_record of {
      cr_name : string;
      cr_fields : record_type_desc;
      cr_codec : codec;
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
  | Cstr_tuple { ct_name; ct_args; ct_codec=_; } ->
    constructor_declaration ~loc ~res:None
      ~name:(locmk ~loc ct_name)
      ~args:(Pcstr_tuple (ct_args |&> fun arg ->
          ptyp_constr ~loc (locmk ~loc (lident arg)) []))
  | Cstr_record { cr_name; cr_fields; cr_codec=_; } ->
    constructor_declaration ~loc ~res:None
      ~name:(locmk ~loc cr_name)
      ~args:(match type_kind_of_generic_kind (Record_kind cr_fields) with
          | Ptype_record fields -> Pcstr_record fields
          | _ -> failwith "type_kind but record is invalid in record constructor declaration")

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

let gen_json_encoder : type_decl -> codec -> value_binding = fun { td_name; td_kind=(kind, _); } codec ->
  let loc = Location.none in
  let name = pvar ~loc (encoder_name td_name codec) in
  let vari i = "__bindoj_gen_json_encoder_var_"^string_of_int i in
  let evari i = evar ~loc (vari i) in
  let pvari i = pvar ~loc (vari i) in
  let gen_record_encoder_params : record_field_desc list -> pattern = fun fields ->
    ppat_record ~loc
      (List.mapi (fun i { rf_name; rf_type=_; rf_codec=_; } ->
           (locmk ~loc (lident rf_name), pvari i))
          fields)
      Closed in
  let gen_record_encoder_body : record_field_desc list -> expression = fun fields ->
    let members =
      List.mapi (fun i { rf_name; rf_type; rf_codec; } ->
          [%expr ([%e estring ~loc rf_name], [%e evar ~loc (encoder_name rf_type rf_codec)] [%e evari i])])
        fields in
    List.fold_right (fun e acc -> [%expr [%e e] :: [%e acc]])
      members [%expr []]
    |> fun e -> [%expr `obj [%e e]] in
  let gen_variant_encoder_params : variant_constructor_desc list -> pattern list = fun constrs ->
    constrs |&> function
      | Cstr_tuple { ct_name; ct_args; ct_codec=_; } ->
        ppat_construct ~loc
          (locmk ~loc (lident ct_name))
          (match ct_args with
           | [] -> None
           | _ -> Some (ppat_tuple ~loc (List.mapi (fun i _ -> pvari i) ct_args)))
      | Cstr_record { cr_name; cr_fields; cr_codec=_; } ->
        ppat_construct ~loc
          (locmk ~loc (lident cr_name))
          (Some (gen_record_encoder_params (cr_fields |&> fst))) in
  let gen_variant_encoder_body : variant_constructor_desc list -> expression list = fun constrs ->
    constrs |&> function
      | Cstr_tuple { ct_name; ct_args; ct_codec; } ->
        let name = estring ~loc ct_name in
        let args =
          List.mapi (fun i typ ->
              [%expr [%e evar ~loc (encoder_name typ ct_codec)] [%e evari i]])
            ct_args
          |> elist ~loc in
        [%expr `arr [[%e name]; `arr [%e args]]]
      | Cstr_record { cr_name; cr_fields; cr_codec=_; } ->
        let name = estring ~loc cr_name in
        let args = gen_record_encoder_body (cr_fields |&> fst) in
        [%expr `arr [[%e name]; [%e args]]] in
  match kind with
  | Record_kind record ->
    let fields = record |&> fst in
    let params = gen_record_encoder_params fields in
    let body = gen_record_encoder_body fields in
    value_binding ~loc
      ~pat:name
      ~expr:[%expr fun [%p params] -> [%e body]]
  | Variant_kind variant ->
    let constrs = variant |&> fst in
    let params = gen_variant_encoder_params constrs in
    let body = gen_variant_encoder_body constrs in
    let cases = List.map2 (fun p b ->
        case ~lhs:p ~rhs:b ~guard:None)
        params body in
    value_binding ~loc
      ~pat:name
      ~expr:(pexp_function ~loc cases)
