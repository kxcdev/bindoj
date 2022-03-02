module Datatype_desc = struct

  type 'x with_docstr = 'x*[ `docstr of string | `nodoc ]

  type record_type_desc = record_field_desc list with_docstr
  and record_field_desc = record_field_desc' with_docstr
  and record_field_desc' = {
      field_name : string;
      field_type : string;
    }

  type variant_type_desc = variant_branch_desc list with_docstr
  and variant_branch_desc = variant_branch_desc' with_docstr
  and variant_branch_desc' =
    | Vbd_tag of {
        const_name : string;
      }
    | Vbd_simple of {
        const_name : string;
        param_type : string;
      }
    | Vbd_record of {
        const_name : string;
        fields: record_field_desc list;
      }

end

let ex01 : Datatype_desc.record_type_desc =
  let open Datatype_desc in
  [{ field_name = "admission_year"; field_type = "int" }, `nodoc;
   { field_name = "name"; field_type = "string" }, `nodoc;
  ], `nodoc

let ex02 : Datatype_desc.variant_type_desc =
  let open Datatype_desc in
  [ Vbd_tag { const_name = "Anonymous" }, `nodoc;
    Vbd_simple { const_name = "With_id"; param_type = "int"}, `nodoc;
    Vbd_record {
        const_name = "Student";
        fields = [
            { field_name = "student_id"; field_type = "int" }, `nodoc;
            { field_name = "name"; field_type = "string" }, `nodoc;
      ]}, `nodoc;
    Vbd_record {
        const_name = "Teacher";
        fields = [
            { field_name = "faculty_id"; field_type = "int" }, `nodoc;
            { field_name = "name"; field_type = "string" }, `nodoc;
            { field_name = "department"; field_type = "string" }, `nodoc;
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
  Format.printf "add 1 2 = %d@." (Codegen.Arith.add 1 2)
