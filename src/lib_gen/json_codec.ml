(* Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

open Ppxlib
open Ast_builder.Default
open Ast_helper
open Utils

type variant_type_flavor = [
    `flat_kind
  (* | `tuple *)
  (* | `nested_kind *)
  ]
type ('pos, 'flavor) flavor_config +=
   | Flvconfig_flat_kind : {
       kind_fname : string option;
       arg_fname : string option;
     } -> ([ `branch ], [ `flat_kind ]) flavor_config

type flavor = variant_type_flavor

let get_variant_type_flavor : _ -> ([`branch], variant_type_flavor) flavor_config =
  let default =
    Flvconfig_flat_kind { kind_fname = None; arg_fname = None }
  in
  FlavorConfigs.get ~default (function Flvconfig_flat_kind _ -> true | _ -> false)

let encoder_name type_name = function
  | `default_codec -> "encode_"^type_name^"_json"
  | `codec_val v -> v
  | `codec_in_module m -> m^".encode_"^type_name^"_json"
let decoder_name type_name = function
  | `default_codec -> "decode_"^type_name^"_json"
  | `codec_val v -> v
  | `codec_in_module m -> m^".decode_"^type_name^"_json"

let default_kind_fname = "kind"
let default_arg_fname = "arg"
let kind_fname_value kind_fname =
  Option.value kind_fname ~default:default_kind_fname
let arg_fname_value arg_fname =
  Option.value arg_fname ~default:default_arg_fname

let gen_primitive_encoders : ?attrs:attributes -> codec -> value_binding list =
  fun ?attrs codec ->
  let loc = Location.none in
  let bind str expr = Vb.mk ~loc ?attrs (Pat.var (strloc str)) expr in
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

let gen_primitive_decoders : ?attrs:attributes -> codec -> value_binding list =
  fun ?attrs codec ->
  let loc = Location.none in
  let bind str expr = Vb.mk ~loc ?attrs (Pat.var (strloc str)) expr in
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

let gen_json_encoder :
      ?self_contained:bool -> ?flavor:flavor -> ?codec:codec -> type_decl -> value_binding =
  fun ?(self_contained=false) ?(flavor=`flat_kind) ?(codec=`default_codec) { td_name; td_kind=(kind, _); td_flvconfigs } ->
  let loc = Location.none in
  let name = pvar ~loc (encoder_name td_name codec) in
  let vari i = "__bindoj_gen_json_encoder_var_"^string_of_int i in
  let evari i = evar ~loc (vari i) in
  let pvari i = pvar ~loc (vari i) in
  let wrap_self_contained e =
    if self_contained then
      pexp_let ~loc Nonrecursive
        (gen_primitive_encoders ~attrs:(warning_attribute "-26") codec)
        e
    else e in
  let record_params : record_field_desc list -> pattern = fun fields ->
    ppat_record ~loc
      (List.mapi (fun i { rf_name; _; } ->
           (lidloc ~loc rf_name, pvari i))
         fields)
      Closed in
  let member_of_field : int -> record_field_desc -> expression =
    fun i { rf_name; rf_type; rf_codec; } ->
    [%expr ([%e estring ~loc rf_name],
            [%e evar ~loc (encoder_name rf_type rf_codec)] [%e evari i])] in
  let record_body : record_field_desc list -> expression = fun fields ->
    let members = List.mapi member_of_field fields in
    [%expr `obj [%e elist ~loc members]] in
  let variant_params : variant_constructor_desc list -> pattern list = fun constrs ->
    constrs |&> function
      | Cstr_tuple { ct_name; ct_args; _; } ->
        let inner =
          match ct_args with
          | [] -> None
          | _ -> Some (Pat.tuple (List.mapi (fun i _ -> pvari i) ct_args))
        in
        begin match Caml_datatype.get_variant_type td_flvconfigs with
        | `regular_variant -> Pat.construct (lidloc ct_name) inner
        | `polymorphic_variant -> Pat.variant ct_name inner
        end
      | Cstr_record { cr_name; cr_fields; _; } ->
        begin match Caml_datatype.get_variant_type td_flvconfigs with
        | `regular_variant ->
          ppat_construct ~loc
            (lidloc ~loc cr_name)
            (Some (record_params (cr_fields |&> fst)))
        | `polymorphic_variant ->
          failwith' "case '%s' with an inline record cannot be used in a polymorphic variant" cr_name
        end
  in
  let variant_body : variant_constructor_desc list -> expression list = fun cnstrs ->
    match flavor with
    | `flat_kind ->
      cnstrs |&> begin function
        | Cstr_tuple { ct_name; ct_args; ct_codec; ct_flvconfigs; } ->
          begin match get_variant_type_flavor ct_flvconfigs with
          | Flvconfig_flat_kind { kind_fname; arg_fname; } ->
            let kind_fname =
              estring ~loc (Option.value kind_fname ~default:default_kind_fname) in
            let arg_fname =
              estring ~loc (Option.value arg_fname ~default:default_arg_fname) in
            let cstr = [%expr ([%e kind_fname], `str [%e estring ~loc ct_name])] in
            let args =
              List.mapi (fun i typ ->
                  [%expr [%e evar ~loc (encoder_name typ ct_codec)] [%e evari i]])
                ct_args in
            begin match args with
            | [] -> [%expr `obj [[%e cstr]]]
            | [arg] -> [%expr `obj [[%e cstr]; ([%e arg_fname], [%e arg])]]
            | _ -> [%expr `obj [[%e cstr]; ([%e arg_fname], `arr [%e elist ~loc args])]]
            end
          | _ -> failwith' "unknown flavor configs on case '%s' of type '%s'" ct_name td_name
          end
        | Cstr_record { cr_name; cr_fields; cr_flvconfigs; _; } ->
          begin match get_variant_type_flavor cr_flvconfigs with
          | Flvconfig_flat_kind { kind_fname; _; } ->
            let kind_fname =
              estring ~loc (Option.value kind_fname ~default:default_kind_fname) in
            let cstr = [%expr ([%e kind_fname], `str [%e estring ~loc cr_name])] in
            let args = List.mapi (fun i (field, _) -> member_of_field i field) cr_fields in
            [%expr `obj [%e elist ~loc (cstr :: args)]]
          | _ -> failwith' "unknown flavor configs on case '%s' of type '%s'" cr_name td_name
          end
      end
  in
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
      ?self_contained:bool -> ?flavor:flavor -> ?codec:codec -> type_decl -> value_binding =
  fun ?(self_contained=false) ?(flavor=`flat_kind) ?(codec=`default_codec) { td_name; td_kind=(kind, _); td_flvconfigs } ->
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
        (gen_primitive_decoders ~attrs:(warning_attribute "-26") codec)
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
      (List.mapi (fun i { rf_name; _; } ->
           (lidloc ~loc rf_name, [%expr [%e evari i]]))
         fields)
      None in
  let variant_params : variant_constructor_desc list -> pattern list*(*kind_fname*)string = fun cstrs ->
    match flavor with
    | `flat_kind ->
      cstrs |&> begin function
        | Cstr_tuple { ct_name; ct_args; ct_flvconfigs; _; } ->
          begin match get_variant_type_flavor ct_flvconfigs with
          | Flvconfig_flat_kind { kind_fname; arg_fname; } ->
            let kind_fname = Option.value kind_fname ~default:default_kind_fname in
            let kind_fpatt = pstring ~loc kind_fname  in
            let arg_fname =
              pstring ~loc (Option.value arg_fname ~default:default_arg_fname) in
            let cstr = [%pat? ([%p kind_fpatt], `str [%p pstring ~loc ct_name])] in
            let args = List.mapi (fun i _ -> pvari i) ct_args in
            begin match args with
            | [] -> [%pat? `obj [[%p cstr]]]
            | [arg] -> [%pat? `obj [[%p cstr]; ([%p arg_fname], [%p arg])]]
            | _ -> [%pat? `obj [[%p cstr]; ([%p arg_fname], `arr [%p plist ~loc args])]]
            end, kind_fname
          | _ -> failwith' "unknown flavor configs on case '%s' of type '%s'" ct_name td_name
          end
        | Cstr_record { cr_name; cr_flvconfigs; _;  } ->
          begin match get_variant_type_flavor cr_flvconfigs with
          | Flvconfig_flat_kind { kind_fname; _; } ->
            let kind_fname = Option.value kind_fname ~default:default_kind_fname in
            let kind_fpatt = pstring ~loc kind_fname  in
            let cstr = [%pat? ([%p kind_fpatt], `str [%p pstring ~loc cr_name])] in
            [%pat? `obj ([%p cstr] :: [%p param_p])], kind_fname
          | _ -> failwith' "unknown flavor configs on case '%s' of type '%s'" cr_name td_name
          end
      end |> List.unzip |> (identity // List.dedup)
      |> (function
          | patts, [fname] -> patts, fname
          | _, fnames -> failwith' "inconsistant kind_fname: %a" List.(pp pp_string) fnames)
  in
  let variant_body : variant_constructor_desc list -> expression list = fun cstrs ->
    let construct name args =
      match Caml_datatype.get_variant_type td_flvconfigs with
      | `regular_variant -> Exp.construct (lidloc name) args
      | `polymorphic_variant -> Exp.variant name args
    in
    cstrs |&> begin function
      | Cstr_tuple { ct_name; ct_args; _; } ->
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
      | Cstr_record { cr_name; cr_fields; _; } ->
        begin match cr_fields with
        | [] -> construct cr_name None
        | _ ->
          let fields = cr_fields |&> fst in
          let bindings = record_bindings fields in
          let body =
            match Caml_datatype.get_variant_type td_flvconfigs with
            | `regular_variant -> record_body fields
            | `polymorphic_variant -> failwith' "case '%s' with an inline record cannot be used in a polymorphic variant" cr_name
          in
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
     let params, kind_fname = variant_params cstrs in
     let body = variant_body cstrs in
     let cases =
       List.map2 (fun p b ->
           case ~lhs:p ~rhs:b ~guard:None)
         params body
       @ [(case ~lhs:(ppat_any ~loc) ~rhs:[%expr None] ~guard:None)] in
     value_binding ~loc
       ~pat:name
       ~expr:(pexp_constraint ~loc
                [%expr fun __bindoj_orig ->
                    Kxclib.Jv.pump_field [%e estring ~loc kind_fname ] __bindoj_orig
                    |> [%e (wrap_self_contained (pexp_function ~loc cases))]]
                [%type: Kxclib.Json.jv -> [%t typcons ~loc td_name] option])
  end

open Bindoj_openapi.V3

let gen_openapi_schema : type_decl -> Json.jv =
  let schema = Schema_object.schema in

  let docopt = function
    | `docstr s -> Some s
    | `nodoc -> None
  in
  let convert_type ?description = function
    | "int" -> Schema_object.integer ?description ()
    | "float" -> Schema_object.number ?description ()
    | "string" -> Schema_object.string ?description ()
    | "bool" -> Schema_object.boolean ?description ()
    | x -> Schema_object.ref ("#" ^ x)
  in

  let record_to_t ?schema ?id ?(additional_fields = []) ~name ~doc desc =
    let field_to_t (field, doc) =
      field.rf_name,
      convert_type ?description:(docopt doc) field.rf_type
    in
    let fields = desc |> List.map field_to_t in
    Schema_object.record
      ?schema
      ~title:name
      ?description:(docopt doc)
      ?id
      (fields @ additional_fields)
  in

  fun { td_name; td_kind; _ } ->
    let id = "#" ^ td_name in
    let doc = snd td_kind in
    let so =
      match fst td_kind with
      | Record_kind desc ->
        record_to_t ~schema ~id ~name:td_name ~doc desc
      | Variant_kind desc ->
        let ctor_name (ctor, _) =
          match ctor with
          | Cstr_record r -> r.cr_name
          | Cstr_tuple  t -> t.ct_name
        in
        let ctor_to_t (ctor, doc) =
          let name = ctor_name (ctor, doc) in
          let pattern = sprintf "^%s$" name in
          match ctor with
          | Cstr_record r ->
            begin match get_variant_type_flavor r.cr_flvconfigs with
            | Flvconfig_flat_kind { kind_fname; _ } ->
              let kind_fname = kind_fname_value kind_fname in
              let additional_fields = [kind_fname, Schema_object.string ~pattern ()] in
              record_to_t
                ~additional_fields
                ~name
                ~doc
                r.cr_fields
            | _ -> failwith' "unknown flavor configs on case '%s' of type '%s'" name td_name
            end
          | Cstr_tuple t ->
            begin match get_variant_type_flavor t.ct_flvconfigs with
            | Flvconfig_flat_kind { kind_fname; arg_fname } ->
              let kind_fname = kind_fname_value kind_fname in
              let arg_fname = arg_fname_value arg_fname in
              let kind_field = [kind_fname, Schema_object.string ~pattern ()] in
              let arg_field =
                match t.ct_args with
                | []  -> []
                | [t] -> [arg_fname, convert_type t]
                | ts  -> [arg_fname,  Schema_object.tuple (ts |> List.map convert_type)]
              in
              let fields = kind_field @ arg_field in
              Schema_object.record
                ?description:(docopt doc)
                ~title:name
                fields
            | _ -> failwith' "unknown flavor configs on case '%s' of type '%s'" name td_name
            end
        in
        Schema_object.oneOf
          ~schema
          ~title:td_name
          ?description:(docopt doc)
          ~id
          (desc |> List.map ctor_to_t)
    in
    Schema_object.to_json so
