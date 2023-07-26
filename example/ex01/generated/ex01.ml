type nonrec my_int = int [@@ocaml.doc "definition of my_int type"]

let (my_int_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias { get = Expr.of_int; mk = Expr.to_int })
[@@warning "-33-39"]

let my_int_json_shape_explanation =
  (`with_warning
     ("not considering any config if exists", `named ("MyInt", `integral))
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec my_int_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   int_to_json
    : my_int -> Kxclib.Json.jv)
[@@warning "-39"]

and my_int_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl =
        let int_of_json' path = function
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
        int_of_json'
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, my_int_json_shape_explanation))
    : my_int Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and my_int_of_json =
  (fun x -> my_int_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> my_int option)
[@@warning "-39"]

type nonrec my_tuple = float * string
[@@ocaml.doc "definition of my_tuple type"]

let (my_tuple_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias
       {
         get =
           (fun (x0, x1) -> Expr.Tuple [ Expr.of_float x0; Expr.of_string x1 ]);
         mk =
           (function
           | Expr.Tuple [ x0; x1 ] ->
               Expr.to_float x0 >>= fun x0 ->
               Expr.to_string x1 >>= fun x1 -> Some (x0, x1)
           | _ -> None);
       })
[@@warning "-33-39"]

let my_tuple_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named ("MyTuple", `tuple_of [ `proper_float; `string ]) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec my_tuple_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and float_to_json (x : float) : Kxclib.Json.jv = `num x in
   fun (x0, x1) : Kxclib.Json.jv ->
     `obj [ ("_0", float_to_json x0); ("_1", string_to_json x1) ]
    : my_tuple -> Kxclib.Json.jv)
[@@warning "-39"]

and my_tuple_of_json' =
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
        and float_of_json' path = function
          | (`num x : Kxclib.Json.jv) -> Ok x
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'float' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        in
        fun path -> function
          | (`obj fields : Kxclib.Json.jv) ->
              let fields = Bindoj_runtime.StringMap.of_list fields in
              let ( >>= ) = Result.bind in
              (Bindoj_runtime.StringMap.find_opt "_0" fields |> function
               | Some a -> Ok a
               | None -> Error ("mandatory field '_0' does not exist", path))
              >>= fun x0 ->
              (Bindoj_runtime.StringMap.find_opt "_1" fields |> function
               | Some a -> Ok a
               | None -> Error ("mandatory field '_1' does not exist", path))
              >>= fun x1 ->
              let ( >>= ) = Result.bind in
              float_of_json' (`f "_0" :: path) x0 >>= fun x0 ->
              string_of_json' (`f "_1" :: path) x1 >>= fun x1 -> Ok (x0, x1)
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a tuple value, but the given is \
                     of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, my_tuple_json_shape_explanation))
    : my_tuple Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and my_tuple_of_json =
  (fun x -> my_tuple_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> my_tuple option)
[@@warning "-39"]

type student = {
  admission_year : int; [@ocaml.doc "addmission_year field"]
  name : string; [@ocaml.doc "name field"]
}
[@@ocaml.doc "definition of student type"]

let rec (student_reflect : _ Bindoj_runtime.Refl.t) =
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

let student_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "Student",
           `object_of
             [
               `mandatory_field ("admissionYear", `integral);
               `mandatory_field ("name", `string);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec student_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun { admission_year = x0; name = x1 } ->
     `obj [ ("admissionYear", int_to_json x0); ("name", string_to_json x1) ]
    : student -> Kxclib.Json.jv)
[@@warning "-39"]

and student_of_json' =
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
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error
                         ("mandatory field 'admissionYear' does not exist", path))
              >>= int_of_json' (`f "admissionYear" :: path)
              >>= fun x0 ->
              List.assoc_opt "name" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'name' does not exist", path))
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
            (msg, path, student_json_shape_explanation))
    : student Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and student_of_json =
  (fun x -> student_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> student option)
[@@warning "-39"]

type person =
  | Anonymous [@ocaml.doc "Anonymous constructor"]
  | With_id of int [@ocaml.doc "With_id constructor"]
  | Student of {
      student_id : int; [@ocaml.doc "student_id field in Student constructor"]
      name : string; [@ocaml.doc "name field in Student constructor"]
    } [@ocaml.doc "Student constructor"]
  | Teacher of {
      faculty_id : int; [@ocaml.doc "faculty_id field in Teacher constructor"]
      name : string; [@ocaml.doc "name field in Teacher constructor"]
      department : string; [@ocaml.doc "dapartment field in Teacher constructor"]
    } [@ocaml.doc "Teacher constructor"]
[@@ocaml.doc "definition of person type"]

let rec (person_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_Anonymous = Refl.NoParam { value = Anonymous } in
     let ctor_With_id =
       Refl.TupleLike
         {
           get =
             (function
             | With_id x -> [ Expr.of_int x ]
             | _ -> invalid_arg "With_id is expected");
           mk =
             (function
             | x :: [] -> Expr.to_int x |> Option.map (fun x -> With_id x)
             | _ -> None);
         }
     in
     let ctor_Student =
       Refl.InlineRecord
         {
           get =
             (function
             | Student { student_id; name } ->
                 StringMap.of_list
                   [
                     ("student_id", Expr.of_int student_id);
                     ("name", Expr.of_string name);
                   ]
             | _ -> invalid_arg "Student is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "student_id" >>= Expr.to_int
               >>= fun student_id ->
               xs |> StringMap.find_opt "name" >>= Expr.to_string
               >>= fun name -> Some (Student { student_id; name }));
         }
     in
     let ctor_Teacher =
       Refl.InlineRecord
         {
           get =
             (function
             | Teacher { faculty_id; name; department } ->
                 StringMap.of_list
                   [
                     ("faculty_id", Expr.of_int faculty_id);
                     ("name", Expr.of_string name);
                     ("department", Expr.of_string department);
                   ]
             | _ -> invalid_arg "Teacher is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "faculty_id" >>= Expr.to_int
               >>= fun faculty_id ->
               xs |> StringMap.find_opt "name" >>= Expr.to_string
               >>= fun name ->
               xs |> StringMap.find_opt "department" >>= Expr.to_string
               >>= fun department ->
               Some (Teacher { faculty_id; name; department }));
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list
             [
               ("Anonymous", ctor_Anonymous);
               ("With_id", ctor_With_id);
               ("Student", ctor_Student);
               ("Teacher", ctor_Teacher);
             ];
         classify =
           (function
           | Anonymous -> ("Anonymous", ctor_Anonymous)
           | With_id _ -> ("With_id", ctor_With_id)
           | Student _ -> ("Student", ctor_Student)
           | Teacher _ -> ("Teacher", ctor_Teacher));
       })
[@@warning "-33-39"]

let person_json_discriminator_value =
  (function
   | Anonymous -> "anonymous"
   | With_id _ -> "with-id"
   | Student _ -> "student"
   | Teacher _ -> "teacher"
    : person -> string)
[@@warning "-39"]

let person_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "Person",
           `anyone_of
             [
               `object_of
                 [ `mandatory_field ("kind", `exactly (`str "anonymous")) ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "with-id"));
                   `mandatory_field ("arg", `tuple_of [ `integral ]);
                 ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "student"));
                   `mandatory_field ("studentId", `integral);
                   `mandatory_field ("name", `string);
                 ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "teacher"));
                   `mandatory_field ("facultyId", `integral);
                   `mandatory_field ("name", `string);
                   `mandatory_field ("department", `string);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec person_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   function
   | Anonymous -> `obj [ ("kind", `str "anonymous") ]
   | With_id x0 -> `obj [ ("kind", `str "with-id"); ("arg", int_to_json x0) ]
   | Student { student_id = x0; name = x1 } ->
       `obj
         [
           ("kind", `str "student");
           ("studentId", int_to_json x0);
           ("name", string_to_json x1);
         ]
   | Teacher { faculty_id = x0; name = x1; department = x2 } ->
       `obj
         [
           ("kind", `str "teacher");
           ("facultyId", int_to_json x0);
           ("name", string_to_json x1);
           ("department", string_to_json x2);
         ]
    : person -> Kxclib.Json.jv)
[@@warning "-39"]

and person_of_json' =
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
          match Kxclib.Jv.pump_field "kind" __bindoj_orig with
          | `obj (("kind", `str "anonymous") :: _) -> Ok Anonymous
          | `obj (("kind", `str "with-id") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`f "arg" :: path) arg >>= fun x0 ->
                  Ok (With_id x0)
              | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("kind", `str "student") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "studentId" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'studentId' does not exist", path))
              >>= int_of_json' (`f "studentId" :: path)
              >>= fun x0 ->
              List.assoc_opt "name" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'name' does not exist", path))
              >>= string_of_json' (`f "name" :: path)
              >>= fun x1 -> Ok (Student { student_id = x0; name = x1 })
          | `obj (("kind", `str "teacher") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "facultyId" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'facultyId' does not exist", path))
              >>= int_of_json' (`f "facultyId" :: path)
              >>= fun x0 ->
              List.assoc_opt "name" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'name' does not exist", path))
              >>= string_of_json' (`f "name" :: path)
              >>= fun x1 ->
              List.assoc_opt "department" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error
                         ("mandatory field 'department' does not exist", path))
              >>= string_of_json' (`f "department" :: path)
              >>= fun x2 ->
              Ok (Teacher { faculty_id = x0; name = x1; department = x2 })
          | `obj (("kind", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'anonymous', 'with-id', 'student', 'teacher' ]"
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
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, person_json_shape_explanation))
    : person Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and person_of_json =
  (fun x -> person_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> person option)
[@@warning "-39"]
