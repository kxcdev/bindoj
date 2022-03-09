module Datatype_desc = struct

  open Ppxlib
  open Ast_builder.Default

  type 'x with_docstr = 'x*[ `docstr of string | `nodoc ]
(*
    type_declaration = {
      ptype_name : label loc;
      ptype_params : (core_type * (variance * injectivity)) list;
      ptype_cstrs : (core_type * core_type * location) list;
      ptype_kind : type_kind;
      ptype_private : private_flag;
      ptype_manifest : core_type option;
      ptype_attributes : attributes;
      ptype_loc : location;
    }
*)

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
    td_kind : generic_kind;
  }
  and generic_kind =
    | Record_kind of record_type_desc with_docstr
    | Variant_kind of variant_type_desc with_docstr

  let kind_type_of_record_type_desc : record_type_desc with_docstr -> type_kind =
    fun (fields, _doc) ->
    let loc = Location.none in
    Ptype_record (List.map (fun ({rf_name; rf_type;}, _doc) ->
        label_declaration ~loc ~mutable_:Immutable
          ~name:(Located.mk ~loc rf_name)
          ~type_:(ptyp_constr ~loc (Located.mk ~loc (lident rf_type)) [])
      ) fields)

  let kind_type_of_variant_type_desc : variant_type_desc with_docstr -> type_kind =
    fun (constrs, _doc) ->
    let loc = Location.none in
    Ptype_variant (List.map (function
        | Cstr_tuple {ct_name; ct_args;}, _doc ->
           constructor_declaration ~loc ~res:None
             ~name:(Located.mk ~loc ct_name)
             ~args:(Pcstr_tuple (List.map (fun ct_arg ->
                 ptyp_constr ~loc (Located.mk ~loc (lident ct_arg)) [])
                 ct_args))
        | Cstr_record {cr_name; cr_fields;}, _doc ->
           constructor_declaration ~loc ~res:None
             ~name:(Located.mk ~loc cr_name)
             ~args:(match kind_type_of_record_type_desc (cr_fields, `nodoc) with
                 | Ptype_record fields -> Pcstr_record fields
                 | _ -> failwith "impossible type_kind"))
        constrs)

  let type_declaration_of_type_decl : type_decl with_docstr -> type_declaration =
    fun ({ td_name; td_kind; }, _doc) ->
    let loc = Location.none in
    type_declaration ~loc ~params:[] ~cstrs:[] ~private_:Public ~manifest:None
      ~name:(Located.mk ~loc td_name)
      ~kind:(match td_kind with
          | Record_kind record -> kind_type_of_record_type_desc record
          | Variant_kind variant -> kind_type_of_variant_type_desc variant)

end

let ex01 : Datatype_desc.type_decl Datatype_desc.with_docstr =
  let open Datatype_desc in
  { td_name = "student";
    td_kind = Record_kind
        ([{ rf_name = "admission_year"; rf_type = "int"; }, `nodoc;
          { rf_name = "name"; rf_type = "string"; }, `nodoc;
         ], `nodoc);
  }, `nodoc

let ex02 : Datatype_desc.type_decl Datatype_desc.with_docstr =
  let open Datatype_desc in
  { td_name = "person";
    td_kind = Variant_kind
        ([ Cstr_tuple { ct_name = "Anonymous"; ct_args = []; }, `nodoc;
           Cstr_tuple { ct_name = "With_id"; ct_args = ["int"]}, `nodoc;
           Cstr_record {
             cr_name = "Student";
             cr_fields = [
               { rf_name = "student_id"; rf_type = "int" }, `nodoc;
               { rf_name = "name"; rf_type = "string" }, `nodoc;
             ]}, `nodoc;
           Cstr_record {
             cr_name = "Teacher";
             cr_fields = [
               { rf_name = "faculty_id"; rf_type = "int" }, `nodoc;
               { rf_name = "name"; rf_type = "string" }, `nodoc;
               { rf_name = "department"; rf_type = "string" }, `nodoc;
             ]}, `nodoc;
         ], `nodoc);
  }, `nodoc

module ExpectedEx01_and_Ex02 = struct

  type ex01 = {
      admission_year : int;
      name : string;
    }

  type ex02 =
    | Anonymous
    | With_id of int
    | Student of {
        student_id : int;
        name : string;
      }
    | Teacher of {
        faculty_id : int;
        name : string;
        department : string;
      }

end

let () =
  Format.printf "Test_gen.greeting \"Alice\": %s@."
    (Codegen.Greeting.greeting "Alice");
  Format.printf "add 1 2 = %d@." (Codegen.Arith.add 1 2);
  let open Ppxlib in
  let open Ast_builder.Default in
  let loc = Location.none in
  Ppxlib.Pprintast.signature Format.std_formatter [
    (psig_type ~loc Recursive
       [Datatype_desc.type_declaration_of_type_decl ex01]);
    (psig_type ~loc Recursive
       [Datatype_desc.type_declaration_of_type_decl ex02]);
  ]
