type ex_record_student = { admission_year : int; name : string }

let rec (ex_record_student_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { admission_year; name } ->
             StringMap.of_list
               [
                 ("admission_year", Expr.of_int admission_year);
                 ("name", Expr.of_string name);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "admission_year" >>= Expr.to_int
             >>= fun admission_year ->
             xs |> StringMap.find_opt "name" >>= Expr.to_string >>= fun name ->
             Some { admission_year; name });
       })
[@@warning "-33-39"]

let ex_record_student_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExRecordStudent",
           `object_of
             [
               `mandatory_field ("admissionYear", `integral);
               `mandatory_field ("name", `string);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_record_student_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun { admission_year = x0; name = x1 } ->
     `obj [ ("admissionYear", int_to_json x0); ("name", string_to_json x1) ]
    : ex_record_student -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_record_student_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl =
        let string_of_json' path = function
          | (`str x : Kxclib.Json.jv) -> Ok x
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'string' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and int_of_json' path = function
          | (`num x : Kxclib.Json.jv) ->
              if Float.is_integer x then Ok (int_of_float x)
              else
                Error
                  ( Printf.sprintf "expecting an integer but the given is '%f'" x,
                    path )
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'int' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        in
        fun path __bindoj_orig ->
          match __bindoj_orig with
          | `obj param ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "admissionYear" param
              |> Option.to_result
                   ~none:("mandatory field 'admissionYear' does not exist", path)
              >>= int_of_json' (`f "admissionYear" :: path)
              >>= fun x0 ->
              List.assoc_opt "name" param
              |> Option.to_result
                   ~none:("mandatory field 'name' does not exist", path)
              >>= string_of_json' (`f "name" :: path)
              >>= fun x1 -> Ok { admission_year = x0; name = x1 }
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a record value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, ex_record_student_json_shape_explanation))
    : ex_record_student Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_record_student_of_json =
  (fun x -> ex_record_student_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_record_student option)
[@@warning "-39"]

let ex_record_student_decl =
  Bindoj_test_common_typedesc_examples.Ex_record.Student.decl

let ex_record_student_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_record.Student.decl
    ex_record_student_reflect

type ex_record_teacher = {
  faculty_id : int;
  name : string;
  department : string;
}

let rec (ex_record_teacher_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { faculty_id; name; department } ->
             StringMap.of_list
               [
                 ("faculty_id", Expr.of_int faculty_id);
                 ("name", Expr.of_string name);
                 ("department", Expr.of_string department);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "faculty_id" >>= Expr.to_int
             >>= fun faculty_id ->
             xs |> StringMap.find_opt "name" >>= Expr.to_string >>= fun name ->
             xs |> StringMap.find_opt "department" >>= Expr.to_string
             >>= fun department -> Some { faculty_id; name; department });
       })
[@@warning "-33-39"]

let ex_record_teacher_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExRecordTeacher",
           `object_of
             [
               `mandatory_field ("facultyId", `integral);
               `mandatory_field ("name", `string);
               `mandatory_field ("department", `string);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_record_teacher_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun { faculty_id = x0; name = x1; department = x2 } ->
     `obj
       [
         ("facultyId", int_to_json x0);
         ("name", string_to_json x1);
         ("department", string_to_json x2);
       ]
    : ex_record_teacher -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_record_teacher_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl =
        let string_of_json' path = function
          | (`str x : Kxclib.Json.jv) -> Ok x
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'string' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and int_of_json' path = function
          | (`num x : Kxclib.Json.jv) ->
              if Float.is_integer x then Ok (int_of_float x)
              else
                Error
                  ( Printf.sprintf "expecting an integer but the given is '%f'" x,
                    path )
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'int' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        in
        fun path __bindoj_orig ->
          match __bindoj_orig with
          | `obj param ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "facultyId" param
              |> Option.to_result
                   ~none:("mandatory field 'facultyId' does not exist", path)
              >>= int_of_json' (`f "facultyId" :: path)
              >>= fun x0 ->
              List.assoc_opt "name" param
              |> Option.to_result
                   ~none:("mandatory field 'name' does not exist", path)
              >>= string_of_json' (`f "name" :: path)
              >>= fun x1 ->
              List.assoc_opt "department" param
              |> Option.to_result
                   ~none:("mandatory field 'department' does not exist", path)
              >>= string_of_json' (`f "department" :: path)
              >>= fun x2 -> Ok { faculty_id = x0; name = x1; department = x2 }
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a record value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, ex_record_teacher_json_shape_explanation))
    : ex_record_teacher Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_record_teacher_of_json =
  (fun x -> ex_record_teacher_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_record_teacher option)
[@@warning "-39"]

let ex_record_teacher_decl =
  Bindoj_test_common_typedesc_examples.Ex_record.Teacher.decl

let ex_record_teacher_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_record.Teacher.decl
    ex_record_teacher_reflect
