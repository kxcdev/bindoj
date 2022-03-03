module Datatype_desc = struct

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

  let kind_type_of_record_type_desc : record_type_desc with_docstr -> type_kind =
    fun (fields, _doc) ->
    let loc = Location.none in
    Ptype_record (List.map (fun ({rf_name; rf_type}, _doc) ->
        label_declaration
          ~name:(Located.mk ~loc rf_name)
          ~type_:(ptyp_constr ~loc (Located.mk ~loc (lident rf_type)) [])
          ~loc
          ~mutable_:Immutable
      ) fields)

end

let ex01 : Datatype_desc.record_type_desc Datatype_desc.with_docstr =
  let open Datatype_desc in
  [{ rf_name = "admission_year"; rf_type = "int"; }, `nodoc;
   { rf_name = "name"; rf_type = "string"; }, `nodoc;
  ], `nodoc

let ex02 : Datatype_desc.variant_type_desc Datatype_desc.with_docstr =
  let open Datatype_desc in
  [ Cstr_tuple { ct_name = "Anonymous"; ct_args = []; }, `nodoc;
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
  ], `nodoc

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
  Ppxlib.Pprintast.signature_item
    Format.std_formatter
    (psig_type ~loc Recursive
       [type_declaration
          ~name:(Located.mk ~loc "student")
          ~kind:(Datatype_desc.kind_type_of_record_type_desc ex01)
          ~loc ~params:[] ~cstrs:[] ~private_:Public ~manifest:None])
