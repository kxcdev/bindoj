type nested_record = {
  unit : Ex11_gen.unit;
  student : Ex01_gen.student;
  int53p : Ex09_gen.with_int53p;
  person1 : Ex02_no_mangling_gen.person;
  person2 : Ex02_no_mangling_gen.person;
}

let rec (nested_record_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { unit; student; int53p; person1; person2 } ->
             StringMap.of_list
               [
                 ("unit", Expr.of_unit unit);
                 ("student", (Expr.of_refl Ex01_gen.student_reflect) student);
                 ("int53p", (Expr.of_refl Ex09_gen.with_int53p_reflect) int53p);
                 ( "person1",
                   (Expr.of_refl Ex02_no_mangling_gen.person_reflect) person1 );
                 ( "person2",
                   (Expr.of_refl Ex02_no_mangling_gen.person_reflect) person2 );
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "unit" >>= Expr.to_unit >>= fun unit ->
             xs
             |> StringMap.find_opt "student"
             >>= Expr.to_refl Ex01_gen.student_reflect
             >>= fun student ->
             xs
             |> StringMap.find_opt "int53p"
             >>= Expr.to_refl Ex09_gen.with_int53p_reflect
             >>= fun int53p ->
             xs
             |> StringMap.find_opt "person1"
             >>= Expr.to_refl Ex02_no_mangling_gen.person_reflect
             >>= fun person1 ->
             xs
             |> StringMap.find_opt "person2"
             >>= Expr.to_refl Ex02_no_mangling_gen.person_reflect
             >>= fun person2 -> Some { unit; student; int53p; person1; person2 });
       })
[@@warning "-33-39"]

let nested_record_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "NestedRecord",
           `anyone_of
             [
               `object_of
                 [
                   `mandatory_field
                     ("unit", `named ("Unit", `special ("unit", `exactly `null)));
                   `mandatory_field
                     ( "student",
                       `named
                         ( "Student",
                           `object_of
                             [
                               `mandatory_field ("admissionYear", `integral);
                               `mandatory_field ("name", `string);
                             ] ) );
                   `mandatory_field ("value", `proper_int53p);
                   `mandatory_field
                     ( "person1",
                       `named
                         ( "person_no_mangling",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Student"));
                                   `mandatory_field ("student_id", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("faculty_id", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
                   `mandatory_field ("kind", `exactly (`str "Anonymous"));
                 ];
               `object_of
                 [
                   `mandatory_field
                     ("unit", `named ("Unit", `special ("unit", `exactly `null)));
                   `mandatory_field
                     ( "student",
                       `named
                         ( "Student",
                           `object_of
                             [
                               `mandatory_field ("admissionYear", `integral);
                               `mandatory_field ("name", `string);
                             ] ) );
                   `mandatory_field ("value", `proper_int53p);
                   `mandatory_field
                     ( "person1",
                       `named
                         ( "person_no_mangling",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Student"));
                                   `mandatory_field ("student_id", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("faculty_id", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
                   `mandatory_field ("kind", `exactly (`str "With_id"));
                   `mandatory_field ("arg", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field
                     ("unit", `named ("Unit", `special ("unit", `exactly `null)));
                   `mandatory_field
                     ( "student",
                       `named
                         ( "Student",
                           `object_of
                             [
                               `mandatory_field ("admissionYear", `integral);
                               `mandatory_field ("name", `string);
                             ] ) );
                   `mandatory_field ("value", `proper_int53p);
                   `mandatory_field
                     ( "person1",
                       `named
                         ( "person_no_mangling",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Student"));
                                   `mandatory_field ("student_id", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("faculty_id", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
                   `mandatory_field ("kind", `exactly (`str "Student"));
                   `mandatory_field ("student_id", `integral);
                   `mandatory_field ("name", `string);
                 ];
               `object_of
                 [
                   `mandatory_field
                     ("unit", `named ("Unit", `special ("unit", `exactly `null)));
                   `mandatory_field
                     ( "student",
                       `named
                         ( "Student",
                           `object_of
                             [
                               `mandatory_field ("admissionYear", `integral);
                               `mandatory_field ("name", `string);
                             ] ) );
                   `mandatory_field ("value", `proper_int53p);
                   `mandatory_field
                     ( "person1",
                       `named
                         ( "person_no_mangling",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Student"));
                                   `mandatory_field ("student_id", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("faculty_id", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
                   `mandatory_field ("kind", `exactly (`str "Teacher"));
                   `mandatory_field ("faculty_id", `integral);
                   `mandatory_field ("name", `string);
                   `mandatory_field ("department", `string);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec nested_record_to_json =
  (let unit_to_json () : Kxclib.Json.jv = `num 1.
   and string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int53p_to_json (x : Kxclib.int53p) : Kxclib.Json.jv =
     `num (Kxclib.Int53p.to_float x)
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   let rec ex01_gen__student_to_json_nested =
     (fun { admission_year = x0; name = x1 } ->
        [ ("admissionYear", int_to_json x0); ("name", string_to_json x1) ]
       : Ex01_gen.student -> (string * Kxclib.Json.jv) list)
   and ex02_no_mangling_gen__person_to_json_nested =
     (function
      | Anonymous -> [ ("kind", `str "Anonymous") ]
      | With_id x0 -> [ ("kind", `str "With_id"); ("arg", int_to_json x0) ]
      | Student { student_id = x0; name = x1 } ->
          [
            ("kind", `str "Student");
            ("student_id", int_to_json x0);
            ("name", string_to_json x1);
          ]
      | Teacher { faculty_id = x0; name = x1; department = x2 } ->
          [
            ("kind", `str "Teacher");
            ("faculty_id", int_to_json x0);
            ("name", string_to_json x1);
            ("department", string_to_json x2);
          ]
       : Ex02_no_mangling_gen.person -> (string * Kxclib.Json.jv) list)
   and ex09_gen__with_int53p_to_json_nested =
     (fun { value = x0 } -> [ ("value", int53p_to_json x0) ]
       : Ex09_gen.with_int53p -> (string * Kxclib.Json.jv) list)
   in
   fun { unit = x0; student = x1; int53p = x2; person1 = x3; person2 = x4 } ->
     `obj
       ([
          ("unit", unit_to_json x0);
          ("student", `obj (ex01_gen__student_to_json_nested x1));
        ]
       @ ex09_gen__with_int53p_to_json_nested x2
       @ [ ("person1", `obj (ex02_no_mangling_gen__person_to_json_nested x3)) ]
       @ ex02_no_mangling_gen__person_to_json_nested x4)
    : nested_record -> Kxclib.Json.jv)
[@@warning "-39"]

and nested_record_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl =
        let unit_of_json' path = function
          | `bool _ | `num _ | `str _ | `arr [] | `obj [] -> Ok ()
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'unit' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and string_of_json' path = function
          | (`str x : Kxclib.Json.jv) -> Ok x
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'string' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and int53p_of_json' path = function
          | (`num x : Kxclib.Json.jv) -> Ok (Kxclib.Int53p.of_float x)
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'int53p' but the given is of type '%s'"
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
        let rec ex01_gen__student_of_json_nested path __bindoj_orig =
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
              >>= fun x1 ->
              Ok ({ admission_year = x0; name = x1 } : Ex01_gen.student)
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a record value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and ex02_no_mangling_gen__person_of_json_nested path __bindoj_orig =
          match Kxclib.Jv.pump_field "kind" __bindoj_orig with
          | `obj (("kind", `str "Anonymous") :: _) ->
              Ok (Anonymous : Ex02_no_mangling_gen.person)
          | `obj (("kind", `str "With_id") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`f "arg" :: path) arg >>= fun x0 ->
                  Ok (With_id x0 : Ex02_no_mangling_gen.person)
              | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("kind", `str "Student") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "student_id" param
              |> Option.to_result
                   ~none:("mandatory field 'student_id' does not exist", path)
              >>= int_of_json' (`f "student_id" :: path)
              >>= fun x0 ->
              List.assoc_opt "name" param
              |> Option.to_result
                   ~none:("mandatory field 'name' does not exist", path)
              >>= string_of_json' (`f "name" :: path)
              >>= fun x1 ->
              Ok
                (Student { student_id = x0; name = x1 }
                  : Ex02_no_mangling_gen.person)
          | `obj (("kind", `str "Teacher") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "faculty_id" param
              |> Option.to_result
                   ~none:("mandatory field 'faculty_id' does not exist", path)
              >>= int_of_json' (`f "faculty_id" :: path)
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
              >>= fun x2 ->
              Ok
                (Teacher { faculty_id = x0; name = x1; department = x2 }
                  : Ex02_no_mangling_gen.person)
          | `obj (("kind", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'Anonymous', 'With_id', 'Student', 'Teacher' ]"
                    discriminator_value,
                  `f "kind" :: path )
          | `obj (("kind", jv) :: _) ->
              Error
                ( Printf.sprintf
                    "a string is expected for a variant discriminator, but the \
                     given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  `f "kind" :: path )
          | `obj _ -> Error ("discriminator field 'kind' does not exist", path)
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a variant value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and ex09_gen__with_int53p_of_json_nested path __bindoj_orig =
          match __bindoj_orig with
          | `obj param ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "value" param
              |> Option.to_result
                   ~none:("mandatory field 'value' does not exist", path)
              >>= int53p_of_json' (`f "value" :: path)
              >>= fun x0 -> Ok ({ value = x0 } : Ex09_gen.with_int53p)
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a record value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        in
        fun path __bindoj_orig ->
          match __bindoj_orig with
          | `obj param ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "unit" param
              |> Option.to_result
                   ~none:("mandatory field 'unit' does not exist", path)
              >>= unit_of_json' (`f "unit" :: path)
              >>= fun x0 ->
              List.assoc_opt "student" param
              |> Option.to_result
                   ~none:("mandatory field 'student' does not exist", path)
              >>= ex01_gen__student_of_json_nested (`f "student" :: path)
              >>= fun x1 ->
              ex09_gen__with_int53p_of_json_nested path __bindoj_orig
              >>= fun x2 ->
              List.assoc_opt "person1" param
              |> Option.to_result
                   ~none:("mandatory field 'person1' does not exist", path)
              >>= ex02_no_mangling_gen__person_of_json_nested
                    (`f "person1" :: path)
              >>= fun x3 ->
              ex02_no_mangling_gen__person_of_json_nested path __bindoj_orig
              >>= fun x4 ->
              Ok
                {
                  unit = x0;
                  student = x1;
                  int53p = x2;
                  person1 = x3;
                  person2 = x4;
                }
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
            (msg, path, nested_record_json_shape_explanation))
    : nested_record Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and nested_record_of_json =
  (fun x -> nested_record_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> nested_record option)
[@@warning "-39"]

let nested_record_decl = Bindoj_test_common_typedesc_examples.Ex16.decl

let nested_record_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex16.decl nested_record_reflect
