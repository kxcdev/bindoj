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

(*
module Datatype_declaration = struct

  type 'x with_docstr = 'x*[ `docstr of string | `nodoc ]

  type type_declaration = {
    type_name : string;
    type_kind : type_kind with_docstr;
  }
  and type_kind =
    | Type_record of label_declaration with_docstr list
    | Type_variant of constructor_declaration with_docstr list
  and label_declaration = {
    ld_name : string;
    ld_type : string;
  }
  and constructor_declaration = {
    cd_name : string;
    cd_args : constructor_arguments;
  }
  and constructor_arguments =
    | Cstr_tuple of string list
    | Cstr_record of label_declaration with_docstr list

  let to_ast : type_declaration with_docstr -> Ppxlib.type_declaration =
    fun ({type_name; type_kind;}, _doc) ->
    let module Abd = Ppxlib.Ast_builder.Default in
    Abd.type_declaration
      ~name:type_name
      ~kind:(convert_kind type_kind)
      ~loc:Ppxlib.Location.none
      ~params:[]
      ~cstrs:[]
      ~private_:Ppxlib.Asttypes.Public
      ~manifest:None
  and convert_kind : type_kind with_docstr -> Ppxlib.type_kind =
    let module Abd = Ppxlib.Ast_builder.Default in
    function
    | Type_record lds, _doc ->
       List.map (fun ({ld_name; ld_type;}, _doc) ->
          Ppxlib.Ptype_record
            (Abd.label_declaration
               ~name:
               ~type_:
               ~loc:Ppxlib.Location.none
               ~mutable_:Ppxlib.Immutable
            ))
         lds
    | Type_variant cds, _doc ->
       Ppxlib.Ptype_variant 

end

let ex01 : Datatype_declaration.type_declaration =
  let open Datatype_declaration in
  { type_name = "student";
    type_kind = Type_record [
        { ld_name = "admission_year"; ld_type = "int"; }, `nodoc;
        { ld_name = "name"; ld_type = "string"; }, `nodoc;
      ], `nodoc }

let ex02 : Datatype_declaration.type_declaration =
  let open Datatype_declaration in
  { type_name = "person";
    type_kind = Type_variant [
        { cd_name = "Anonymous"; cd_args = Cstr_tuple []; }, `nodoc;
        { cd_name = "With_id"; cd_args = Cstr_tuple ["int"]; }, `nodoc;
        { cd_name = "Student"; cd_args = Cstr_record [
              { ld_name = "student_id"; ld_type = "int"; }, `nodoc;
              { ld_name = "name"; ld_type = "string"; }, `nodoc]; }, `nodoc;
        { cd_name = "Teacher"; cd_args = Cstr_record [
              { ld_name = "faculty_id"; ld_type = "int"; }, `nodoc;
              { ld_name = "name"; ld_type = "string"; }, `nodoc;
              { ld_name = "department"; ld_type = "string"; }, `nodoc]; }, `nodoc], `nodoc }
*)

(*
type person =
  | Anonymous
  | With_id of int
  | Student { student_id : int; name : string; }
  | Teacher { faculty_id : int; name : string; department : string; }

let ex02 : Datatype_declaration.variant_declaration =
  let open Datatype_declaration in
  [{ pcd_name = "Anonymous"; pcd_args = Pcstr_tuple [] }, `nodoc;
   { pcd_name = "With_id"; pcd_args = Pcstr_tuple ["int"] }, `nodoc;
   { pcd_name = "Student"; pcd_args = Pcstr_record ([
         { pld_name = "student_id"; pld_type = "int"; }, `nodoc;
         { pld_name = "name"; pld_type = "string"; }, `nodoc
       ], `nodoc)}, `nodoc;
   { pcd_name = "Teacher"; pcd_args = Pcstr_record ([
         { pld_name = "faculty_id"; pld_type = "int" }, `nodoc;
         { pld_name = "name"; pld_type = "string" }, `nodoc;
         { pld_name = "department"; pld_type = "string" }, `nodoc;
       ], `nodoc)}, `nodoc],
  `nodoc
*)

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
  Format.printf "add 1 2 = %d@." (Codegen.Arith.add 1 2)
