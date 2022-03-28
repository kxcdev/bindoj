open Ppxlib
open Ast_builder.Default
open Kxclib

let locmk = Located.mk
let lidloc ~loc x = locmk ~loc (lident x)
let typcons ~loc x = ptyp_constr ~loc (lidloc ~loc x) []

type 'x with_docstr = 'x*[ `docstr of string | `nodoc ]
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

type codec = [ `default_codec | `codec_val of string | `codec_in_module of string ]
let encoder_name type_name = function
  | `default_codec -> "encode_"^type_name^"_json"
  | `codec_val v -> v
  | `codec_in_module m -> m^".encode_"^type_name^"_json"
let decoder_name type_name = function
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
    with ptype_attributes = doc_attributes doc }

and type_kind_of_generic_kind : generic_kind -> type_kind = function
  | Record_kind record ->
    Ptype_record (record |&> fun (field, doc) ->
        { (label_declaration_of_record_field_desc field)
          with pld_attributes = doc_attributes doc; })
  | Variant_kind variant ->
    Ptype_variant (variant |&> fun (constructor, doc) ->
        { (constructor_declaration_of_variant_constructor_desc constructor)
          with pcd_attributes = doc_attributes doc; })

and label_declaration_of_record_field_desc : record_field_desc -> label_declaration =
  fun { rf_name; rf_type; rf_codec=_; } ->
  let loc = Location.none in
  label_declaration ~loc ~mutable_:Immutable
    ~name:(locmk ~loc rf_name)
    ~type_:(ptyp_constr ~loc (lidloc ~loc rf_type) [])

and constructor_declaration_of_variant_constructor_desc : variant_constructor_desc -> constructor_declaration =
  let loc = Location.none in
  function
  | Cstr_tuple { ct_name; ct_args; ct_codec=_; } ->
    constructor_declaration ~loc ~res:None
      ~name:(locmk ~loc ct_name)
      ~args:(Pcstr_tuple (ct_args |&> fun arg ->
          ptyp_constr ~loc (lidloc ~loc arg) []))
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

let gen_primitive_decoders : codec -> value_binding list = fun codec ->
  let loc = Location.none in
  let bind str expr =
    value_binding ~loc ~pat:(pvar ~loc str) ~expr:expr in
  [bind (decoder_name "null" codec)
     [%expr fun `null -> ()];
   bind (decoder_name "bool" codec)
     [%expr function
       | (`bool __bindoj_arg : Kxclib.Json.jv) -> Some __bindoj_arg
       | (_ : Kxclib.Json.jv) -> None];
   bind (decoder_name "int" codec)
     [%expr function
       | (`num __bindoj_arg : Kxclib.Json.jv) -> Some (int_of_float __bindoj_arg)
       | (_ : Kxclib.Json.jv) -> None];
   bind (decoder_name "float" codec)
     [%expr function
       | (`num __bindoj_arg : Kxclib.Json.jv) -> Some __bindoj_arg
       | _ -> None];
   bind (decoder_name "string" codec)
     [%expr function
       | (`str __bindoj_arg : Kxclib.Json.jv) -> Some __bindoj_arg
       | _ -> None]]

type flavor = [
  | `tuple
(*
  | `flat_kind_field
  | `nested_kind_field
*)
]

let gen_json_encoder :
  ?self_contained:bool -> ?flavor:flavor -> type_decl -> codec -> value_binding =
  fun ?(self_contained=false) ?(flavor=`tuple) { td_name; td_kind=(kind, _); } codec ->
  let loc = Location.none in
  let name = pvar ~loc (encoder_name td_name codec) in
  let vari i = "__bindoj_gen_json_encoder_var_"^string_of_int i in
  let evari i = evar ~loc (vari i) in
  let pvari i = pvar ~loc (vari i) in
  let wrap_self_contained e =
    if self_contained then
      pexp_let ~loc Nonrecursive
        (gen_primitive_encoders codec |&> fun binding ->
            { binding with pvb_attributes = warning_attributes "-26" })
        e
    else e in
  let record_params : record_field_desc list -> pattern = fun fields ->
    ppat_record ~loc
      (List.mapi (fun i { rf_name; rf_type=_; rf_codec=_; } ->
           (lidloc ~loc rf_name, pvari i))
          fields)
      Closed in
  let record_body : record_field_desc list -> expression = fun fields ->
    let members =
      List.mapi (fun i { rf_name; rf_type; rf_codec; } ->
          [%expr ([%e estring ~loc rf_name], [%e evar ~loc (encoder_name rf_type rf_codec)] [%e evari i])])
        fields in
    List.fold_right (fun e acc -> [%expr [%e e] :: [%e acc]])
      members [%expr []]
    |> fun e -> [%expr `obj [%e e]] in
  let variant_params : variant_constructor_desc list -> pattern list = fun constrs ->
    constrs |&> function
      | Cstr_tuple { ct_name; ct_args; ct_codec=_; } ->
        ppat_construct ~loc
          (lidloc ~loc ct_name)
          (match ct_args with
           | [] -> None
           | _ -> Some (ppat_tuple ~loc (List.mapi (fun i _ -> pvari i) ct_args)))
      | Cstr_record { cr_name; cr_fields; cr_codec=_; } ->
        ppat_construct ~loc
          (lidloc ~loc cr_name)
          (Some (record_params (cr_fields |&> fst))) in
  let variant_body : variant_constructor_desc list -> expression list = fun constrs ->
    match flavor with
    | `tuple ->
      constrs |&> begin function
        | Cstr_tuple { ct_name; ct_args; ct_codec; } ->
          let name = estring ~loc ct_name in
          let args =
            List.mapi (fun i typ ->
                [%expr [%e evar ~loc (encoder_name typ ct_codec)] [%e evari i]])
              ct_args in
          begin match args with
            | [] -> [%expr `arr [`str [%e name]]]
            | [arg] -> [%expr `arr [`str [%e name]; [%e arg]]]
            | _ -> [%expr `arr [`str [%e name]; `arr [%e elist ~loc args]]]
          end
        | Cstr_record { cr_name; cr_fields; cr_codec=_; } ->
          let name = estring ~loc cr_name in
          let args = record_body (cr_fields |&> fst) in
          [%expr `arr [`str [%e name]; [%e args]]]
      end in
  match kind with
  | Record_kind record ->
    let fields = record |&> fst in
    let params = record_params fields in
    let body = record_body fields in
    value_binding ~loc
      ~pat:name
      ~expr:(pexp_constraint ~loc
               (wrap_self_contained [%expr fun [%p params] -> [%e body]])
               [%type: [%t typcons ~loc td_name] -> Kxclib.Json.jv])
  | Variant_kind variant ->
    let constrs = variant |&> fst in
    let params = variant_params constrs in
    let body = variant_body constrs in
    let cases = List.map2 (fun p b ->
        case ~lhs:p ~rhs:b ~guard:None)
        params body in
    value_binding ~loc
      ~pat:name
      ~expr:(pexp_constraint ~loc
               (wrap_self_contained (pexp_function ~loc cases))
               [%type: [%t typcons ~loc td_name] -> Kxclib.Json.jv])

let gen_json_decoder :
  ?self_contained:bool -> ?flavor:flavor -> type_decl -> codec -> value_binding =
  fun ?(self_contained=false) ?(flavor=`tuple) { td_name; td_kind=(kind, _); } codec ->
  let loc = Location.none in
  let name = pvar ~loc (decoder_name td_name codec) in
  let vari i = "__bindoj_gen_json_decoder_var_"^string_of_int i in
  let evari i = evar ~loc (vari i) in
  let pvari i = pvar ~loc (vari i) in
  let param_e = evar ~loc "__bindoj_gen_json_decoder_var_param" in
  let param_p = pvar ~loc "__bindoj_gen_json_decoder_var_param" in
  let wrap_self_contained e =
    if self_contained then
      pexp_let ~loc Nonrecursive
        (gen_primitive_decoders codec |&> fun binding ->
            { binding with pvb_attributes = warning_attributes "-26" })
        e
    else e in
  let bind_options : (pattern * expression) list -> expression -> expression = fun bindings body ->
    [%expr
      let (>>=) = Option.bind in
      [%e List.fold_right (fun (p, e) body ->
          [%expr [%e e] >>= (fun [%p p] -> [%e body])])
          bindings body]] in
  let record_bindings : record_field_desc list -> (pattern * expression) list = fun fields ->
    List.mapi (fun i { rf_name; rf_type; rf_codec; } ->
        (pvari i,
         [%expr
           (List.assoc_opt [%e estring ~loc rf_name] [%e param_e]) >>=
           [%e evar ~loc (decoder_name rf_type rf_codec)]]))
      fields in
  let record_body : record_field_desc list -> expression = fun fields ->
    pexp_record ~loc
      (List.mapi (fun i { rf_name; rf_type=_; rf_codec=_; } ->
           (lidloc ~loc rf_name, [%expr [%e evari i]]))
          fields)
      None in
  let variant_params : variant_constructor_desc list -> pattern list = fun cstrs ->
    cstrs |&> function
      | Cstr_tuple { ct_name; ct_args; ct_codec=_; } ->
        begin match ct_args with
          | [] -> [%pat? `arr [`str [%p pstring ~loc ct_name]]]
          | _ -> [%pat? `arr (`str [%p pstring ~loc ct_name]
                              :: [%p plist ~loc (List.mapi (fun i _ -> pvari i) ct_args)])]
        end
      | Cstr_record { cr_name; cr_fields=_; cr_codec=_; } ->
        [%pat? `arr [`str [%p pstring ~loc cr_name]; `obj [%p param_p]]] in
  let variant_body : variant_constructor_desc list -> expression list = fun cstrs ->
    match flavor with
    | `tuple ->
      let construct name args =
        pexp_construct ~loc (lidloc ~loc name) args in
      cstrs |&> begin function
        | Cstr_tuple { ct_name; ct_args; ct_codec=_; } ->
          begin match ct_args with
            | [] -> [%expr Some [%e construct ct_name None]]
            | _ ->
              let bindings : (pattern * expression) list =
                List.mapi (fun i arg ->
                    (pvari i,
                     [%expr [%e evar ~loc (decoder_name arg codec)] [%e evari i]]))
                  ct_args in
              let body : expression =
                [%expr Some
                    [%e construct
                        ct_name
                        (Some (pexp_tuple ~loc (List.mapi (fun i _ -> evari i) ct_args)))]] in
              bind_options bindings body
          end
      | Cstr_record { cr_name; cr_fields; cr_codec=_; } ->
        begin match cr_fields with
          | [] -> construct cr_name None
          | _ ->
            let fields = cr_fields |&> fst in
            let bindings = record_bindings fields in
            let body = record_body fields in
            bind_options bindings [%expr Some [%e (construct cr_name (Some body))]]
        end
      end
  in
  begin match kind with
    | Record_kind record ->
      let fields = record |&> fst in
      let bindings = record_bindings fields in
      let body = record_body fields in
      value_binding ~loc
        ~pat:name
        ~expr:(pexp_constraint ~loc
                 (wrap_self_contained
                    [%expr function
                      | `obj [%p param_p] -> [%e bind_options bindings [%expr Some [%e body]]]
                      | _ -> None])
                 [%type: Kxclib.Json.jv -> [%t typcons ~loc td_name] option])
    | Variant_kind variant ->
      let cstrs = variant |&> fst in
      let params = variant_params cstrs in
      let body = variant_body cstrs in
      let cases =
        List.map2 (fun p b ->
            case ~lhs:p ~rhs:b ~guard:None)
          params body
        @ [(case ~lhs:(ppat_any ~loc) ~rhs:[%expr None] ~guard:None)] in
      value_binding ~loc
        ~pat:name
        ~expr:(pexp_constraint ~loc
                 (wrap_self_contained (pexp_function ~loc cases))
                 [%type: Kxclib.Json.jv -> [%t typcons ~loc td_name] option])
  end
