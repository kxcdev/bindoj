type ex_variant_person =
  | Anonymous
  | With_id of int
  | Student of { student_id : int; name : string }
  | Teacher of { faculty_id : int; name : string; department : string }

let rec (ex_variant_person_reflect : _ Bindoj_runtime.Refl.t) =
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

let ex_variant_person_json_discriminator_value =
  (function
   | Anonymous -> "anonymous"
   | With_id _ -> "with-id"
   | Student _ -> "student"
   | Teacher _ -> "teacher"
    : ex_variant_person -> string)
[@@warning "-39"]

let ex_variant_person_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExVariantPerson",
           `anyone_of
             [
               `object_of
                 [ `mandatory_field ("kind", `exactly (`str "anonymous")) ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "with-id"));
                   `mandatory_field ("arg", `integral);
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

let rec ex_variant_person_to_json =
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
    : ex_variant_person -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_variant_person_of_json' =
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
              |> Option.to_result
                   ~none:("mandatory field 'studentId' does not exist", path)
              >>= int_of_json' (`f "studentId" :: path)
              >>= fun x0 ->
              List.assoc_opt "name" param
              |> Option.to_result
                   ~none:("mandatory field 'name' does not exist", path)
              >>= string_of_json' (`f "name" :: path)
              >>= fun x1 -> Ok (Student { student_id = x0; name = x1 })
          | `obj (("kind", `str "teacher") :: param) ->
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
            (msg, path, ex_variant_person_json_shape_explanation))
    : ex_variant_person Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_variant_person_of_json =
  (fun x -> ex_variant_person_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_variant_person option)
[@@warning "-39"]

let ex_variant_person_decl =
  Bindoj_test_common_typedesc_examples.Ex_variant.Person.decl

let ex_variant_person_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_variant.Person.decl
    ex_variant_person_reflect

type ex_variant_person_reused =
  | Anonymous
  | With_id of int
  | Student of { student_id : int; name : string }
  | Teacher of { faculty_id : int; name : string; department : string }

let rec (ex_variant_person_reused_reflect : _ Bindoj_runtime.Refl.t) =
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
       Refl.ReusedInlineRecord
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

let ex_variant_person_reused_json_discriminator_value =
  (function
   | Anonymous -> "anonymous"
   | With_id _ -> "with-id"
   | Student _ -> "student"
   | Teacher _ -> "teacher"
    : ex_variant_person_reused -> string)
[@@warning "-39"]

let ex_variant_person_reused_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExVariantPersonReused",
           `anyone_of
             [
               `object_of
                 [ `mandatory_field ("kind", `exactly (`str "anonymous")) ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "with-id"));
                   `mandatory_field ("arg", `integral);
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

let rec ex_variant_person_reused_to_json =
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
    : ex_variant_person_reused -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_variant_person_reused_of_json' =
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
              |> Option.to_result
                   ~none:("mandatory field 'studentId' does not exist", path)
              >>= int_of_json' (`f "studentId" :: path)
              >>= fun x0 ->
              List.assoc_opt "name" param
              |> Option.to_result
                   ~none:("mandatory field 'name' does not exist", path)
              >>= string_of_json' (`f "name" :: path)
              >>= fun x1 -> Ok (Student { student_id = x0; name = x1 })
          | `obj (("kind", `str "teacher") :: param) ->
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
            (msg, path, ex_variant_person_reused_json_shape_explanation))
    : ex_variant_person_reused Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_variant_person_reused_of_json =
  (fun x -> ex_variant_person_reused_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_variant_person_reused option)
[@@warning "-39"]

let ex_variant_person_reused_decl =
  Bindoj_test_common_typedesc_examples.Ex_variant.Person_reused.decl

let ex_variant_person_reused_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_variant.Person_reused.decl
    ex_variant_person_reused_reflect

type ex_variant_int_list = IntNil | IntCons of int * ex_variant_int_list

let rec (ex_variant_int_list_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_IntNil = Refl.NoParam { value = IntNil } in
     let ctor_IntCons =
       Refl.TupleLike
         {
           get =
             (function
             | IntCons (x0, x1) ->
                 [
                   Expr.of_int x0; (Expr.of_refl ex_variant_int_list_reflect) x1;
                 ]
             | _ -> invalid_arg "IntCons is expected");
           mk =
             (function
             | [ x0; x1 ] ->
                 Expr.to_int x0 >>= fun x0 ->
                 (Expr.to_refl ex_variant_int_list_reflect) x1 >>= fun x1 ->
                 Some (IntCons (x0, x1))
             | _ -> None);
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list
             [ ("IntNil", ctor_IntNil); ("IntCons", ctor_IntCons) ];
         classify =
           (function
           | IntNil -> ("IntNil", ctor_IntNil)
           | IntCons _ -> ("IntCons", ctor_IntCons));
       })
[@@warning "-33-39"]

let ex_variant_int_list_json_discriminator_value =
  (function IntNil -> "intnil" | IntCons _ -> "intcons"
    : ex_variant_int_list -> string)
[@@warning "-39"]

let ex_variant_int_list_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExVariantIntList",
           `anyone_of
             [
               `object_of
                 [ `mandatory_field ("kind", `exactly (`str "intnil")) ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "intcons"));
                   `mandatory_field ("arg", `tuple_of [ `integral; `self ]);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_variant_int_list_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   function
   | IntNil -> `obj [ ("kind", `str "intnil") ]
   | IntCons (x0, x1) ->
       `obj
         [
           ("kind", `str "intcons");
           ("arg", `arr [ int_to_json x0; ex_variant_int_list_to_json x1 ]);
         ]
    : ex_variant_int_list -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_variant_int_list_of_json' =
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
        fun path __bindoj_orig ->
          match Kxclib.Jv.pump_field "kind" __bindoj_orig with
          | `obj (("kind", `str "intnil") :: _) -> Ok IntNil
          | `obj (("kind", `str "intcons") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some (`arr [ x0; x1 ]) ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`i 0 :: `f "arg" :: path) x0 >>= fun x0 ->
                  of_json_impl (`i 1 :: `f "arg" :: path) x1 >>= fun x1 ->
                  Ok (IntCons (x0, x1))
              | Some (`arr xs) ->
                  Error
                    ( Printf.sprintf
                        "expecting an array of length 2, but the given has a \
                         length of %d"
                        (List.length xs),
                      `f "arg" :: path )
              | Some jv ->
                  Error
                    ( Printf.sprintf
                        "an array is expected for a tuple value, but the given \
                         is of type '%s'"
                        (let open Kxclib.Json in
                         string_of_jv_kind (classify_jv jv)),
                      `f "arg" :: path )
              | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("kind", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'intnil', 'intcons' ]"
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
            (msg, path, ex_variant_int_list_json_shape_explanation))
    : ex_variant_int_list Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_variant_int_list_of_json =
  (fun x -> ex_variant_int_list_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_variant_int_list option)
[@@warning "-39"]

let ex_variant_int_list_decl =
  Bindoj_test_common_typedesc_examples.Ex_variant.Int_list.decl

let ex_variant_int_list_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_variant.Int_list.decl
    ex_variant_int_list_reflect

type ex_variant_int_list_objtuple =
  | IntNil
  | IntCons of int * ex_variant_int_list_objtuple

let rec (ex_variant_int_list_objtuple_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_IntNil = Refl.NoParam { value = IntNil } in
     let ctor_IntCons =
       Refl.TupleLike
         {
           get =
             (function
             | IntCons (x0, x1) ->
                 [
                   Expr.of_int x0;
                   (Expr.of_refl ex_variant_int_list_objtuple_reflect) x1;
                 ]
             | _ -> invalid_arg "IntCons is expected");
           mk =
             (function
             | [ x0; x1 ] ->
                 Expr.to_int x0 >>= fun x0 ->
                 (Expr.to_refl ex_variant_int_list_objtuple_reflect) x1
                 >>= fun x1 -> Some (IntCons (x0, x1))
             | _ -> None);
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list
             [ ("IntNil", ctor_IntNil); ("IntCons", ctor_IntCons) ];
         classify =
           (function
           | IntNil -> ("IntNil", ctor_IntNil)
           | IntCons _ -> ("IntCons", ctor_IntCons));
       })
[@@warning "-33-39"]

let ex_variant_int_list_objtuple_json_discriminator_value =
  (function IntNil -> "intnil" | IntCons _ -> "intcons"
    : ex_variant_int_list_objtuple -> string)
[@@warning "-39"]

let ex_variant_int_list_objtuple_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExVariantIntListObjtuple",
           `anyone_of
             [
               `object_of
                 [ `mandatory_field ("kind", `exactly (`str "intnil")) ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "intcons"));
                   `mandatory_field ("_0", `integral);
                   `mandatory_field ("_1", `self);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_variant_int_list_objtuple_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   function
   | IntNil -> `obj [ ("kind", `str "intnil") ]
   | IntCons (x0, x1) ->
       `obj
         [
           ("kind", `str "intcons");
           ("_0", int_to_json x0);
           ("_1", ex_variant_int_list_objtuple_to_json x1);
         ]
    : ex_variant_int_list_objtuple -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_variant_int_list_objtuple_of_json' =
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
        fun path __bindoj_orig ->
          match Kxclib.Jv.pump_field "kind" __bindoj_orig with
          | `obj (("kind", `str "intnil") :: _) -> Ok IntNil
          | `obj (("kind", `str "intcons") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "_0" param
              |> Option.to_result
                   ~none:("mandatory field '_0' does not exist", path)
              >>= int_of_json' (`f "_0" :: path)
              >>= fun x0 ->
              List.assoc_opt "_1" param
              |> Option.to_result
                   ~none:("mandatory field '_1' does not exist", path)
              >>= of_json_impl (`f "_1" :: path)
              >>= fun x1 -> Ok (IntCons (x0, x1))
          | `obj (("kind", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'intnil', 'intcons' ]"
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
            (msg, path, ex_variant_int_list_objtuple_json_shape_explanation))
    : ex_variant_int_list_objtuple Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_variant_int_list_objtuple_of_json =
  (fun x -> ex_variant_int_list_objtuple_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_variant_int_list_objtuple option)
[@@warning "-39"]

let ex_variant_int_list_objtuple_decl =
  Bindoj_test_common_typedesc_examples.Ex_variant.Int_list_objtuple.decl

let ex_variant_int_list_objtuple_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_variant.Int_list_objtuple.decl
    ex_variant_int_list_objtuple_reflect

type ex_variant_foo = [ `Foo0 | `Foo1 of int | `Foo2 of int * int ]

let rec (ex_variant_foo_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_Foo0 = Refl.NoParam { value = `Foo0 } in
     let ctor_Foo1 =
       Refl.TupleLike
         {
           get =
             (function
             | `Foo1 x -> [ Expr.of_int x ]
             | _ -> invalid_arg "Foo1 is expected");
           mk =
             (function
             | x :: [] -> Expr.to_int x |> Option.map (fun x -> `Foo1 x)
             | _ -> None);
         }
     in
     let ctor_Foo2 =
       Refl.TupleLike
         {
           get =
             (function
             | `Foo2 (x0, x1) -> [ Expr.of_int x0; Expr.of_int x1 ]
             | _ -> invalid_arg "Foo2 is expected");
           mk =
             (function
             | [ x0; x1 ] ->
                 Expr.to_int x0 >>= fun x0 ->
                 Expr.to_int x1 >>= fun x1 -> Some (`Foo2 (x0, x1))
             | _ -> None);
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list
             [ ("Foo0", ctor_Foo0); ("Foo1", ctor_Foo1); ("Foo2", ctor_Foo2) ];
         classify =
           (function
           | `Foo0 -> ("Foo0", ctor_Foo0)
           | `Foo1 _ -> ("Foo1", ctor_Foo1)
           | `Foo2 _ -> ("Foo2", ctor_Foo2));
       })
[@@warning "-33-39"]

let ex_variant_foo_json_discriminator_value =
  (function `Foo0 -> "foo0" | `Foo1 _ -> "foo1" | `Foo2 _ -> "foo2"
    : ex_variant_foo -> string)
[@@warning "-39"]

let ex_variant_foo_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExVariantFoo",
           `anyone_of
             [
               `object_of [ `mandatory_field ("kind", `exactly (`str "foo0")) ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "foo1"));
                   `mandatory_field ("arg", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "foo2"));
                   `mandatory_field ("arg", `tuple_of [ `integral; `integral ]);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_variant_foo_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   function
   | `Foo0 -> `obj [ ("kind", `str "foo0") ]
   | `Foo1 x0 -> `obj [ ("kind", `str "foo1"); ("arg", int_to_json x0) ]
   | `Foo2 (x0, x1) ->
       `obj
         [
           ("kind", `str "foo2");
           ("arg", `arr [ int_to_json x0; int_to_json x1 ]);
         ]
    : ex_variant_foo -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_variant_foo_of_json' =
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
        fun path __bindoj_orig ->
          match Kxclib.Jv.pump_field "kind" __bindoj_orig with
          | `obj (("kind", `str "foo0") :: _) -> Ok `Foo0
          | `obj (("kind", `str "foo1") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`f "arg" :: path) arg >>= fun x0 ->
                  Ok (`Foo1 x0)
              | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("kind", `str "foo2") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some (`arr [ x0; x1 ]) ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`i 0 :: `f "arg" :: path) x0 >>= fun x0 ->
                  int_of_json' (`i 1 :: `f "arg" :: path) x1 >>= fun x1 ->
                  Ok (`Foo2 (x0, x1))
              | Some (`arr xs) ->
                  Error
                    ( Printf.sprintf
                        "expecting an array of length 2, but the given has a \
                         length of %d"
                        (List.length xs),
                      `f "arg" :: path )
              | Some jv ->
                  Error
                    ( Printf.sprintf
                        "an array is expected for a tuple value, but the given \
                         is of type '%s'"
                        (let open Kxclib.Json in
                         string_of_jv_kind (classify_jv jv)),
                      `f "arg" :: path )
              | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("kind", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'foo0', 'foo1', 'foo2' ]"
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
            (msg, path, ex_variant_foo_json_shape_explanation))
    : ex_variant_foo Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_variant_foo_of_json =
  (fun x -> ex_variant_foo_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_variant_foo option)
[@@warning "-39"]

let ex_variant_foo_decl =
  Bindoj_test_common_typedesc_examples.Ex_variant.Polymorphic.decl

let ex_variant_foo_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_variant.Polymorphic.decl
    ex_variant_foo_reflect

type ex_variant_customized_union =
  | Case1 of int
  | Case2 of { x : int; y : int }

let rec (ex_variant_customized_union_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_Case1 =
       Refl.TupleLike
         {
           get =
             (function
             | Case1 x -> [ Expr.of_int x ]
             | _ -> invalid_arg "Case1 is expected");
           mk =
             (function
             | x :: [] -> Expr.to_int x |> Option.map (fun x -> Case1 x)
             | _ -> None);
         }
     in
     let ctor_Case2 =
       Refl.InlineRecord
         {
           get =
             (function
             | Case2 { x; y } ->
                 StringMap.of_list
                   [ ("x", Expr.of_int x); ("y", Expr.of_int y) ]
             | _ -> invalid_arg "Case2 is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "x" >>= Expr.to_int >>= fun x ->
               xs |> StringMap.find_opt "y" >>= Expr.to_int >>= fun y ->
               Some (Case2 { x; y }));
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list [ ("Case1", ctor_Case1); ("Case2", ctor_Case2) ];
         classify =
           (function
           | Case1 _ -> ("Case1", ctor_Case1) | Case2 _ -> ("Case2", ctor_Case2));
       })
[@@warning "-33-39"]

let ex_variant_customized_union_json_discriminator_value =
  (function Case1 _ -> "case1'" | Case2 _ -> "case2'"
    : ex_variant_customized_union -> string)
[@@warning "-39"]

let ex_variant_customized_union_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExVariantCustomizedUnion",
           `anyone_of
             [
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "case1'"));
                   `mandatory_field ("value", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "case2'"));
                   `mandatory_field ("x'", `integral);
                   `mandatory_field ("y'", `integral);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_variant_customized_union_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   function
   | Case1 x0 -> `obj [ ("tag", `str "case1'"); ("value", int_to_json x0) ]
   | Case2 { x = x0; y = x1 } ->
       `obj
         [
           ("tag", `str "case2'"); ("x'", int_to_json x0); ("y'", int_to_json x1);
         ]
    : ex_variant_customized_union -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_variant_customized_union_of_json' =
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
        fun path __bindoj_orig ->
          match Kxclib.Jv.pump_field "tag" __bindoj_orig with
          | `obj (("tag", `str "case1'") :: param) -> (
              match List.assoc_opt "value" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`f "value" :: path) arg >>= fun x0 ->
                  Ok (Case1 x0)
              | None -> Error ("mandatory field 'value' does not exist", path))
          | `obj (("tag", `str "case2'") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "x'" param
              |> Option.to_result
                   ~none:("mandatory field 'x'' does not exist", path)
              >>= int_of_json' (`f "x'" :: path)
              >>= fun x0 ->
              List.assoc_opt "y'" param
              |> Option.to_result
                   ~none:("mandatory field 'y'' does not exist", path)
              >>= int_of_json' (`f "y'" :: path)
              >>= fun x1 -> Ok (Case2 { x = x0; y = x1 })
          | `obj (("tag", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'case1'', 'case2'' ]"
                    discriminator_value,
                  `f "tag" :: path )
          | `obj (("tag", jv) :: _) ->
              Error
                ( Printf.sprintf
                    "a string is expected for a variant discriminator, but the \
                     given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  `f "tag" :: path )
          | `obj _ -> Error ("discriminator field 'tag' does not exist", path)
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
            (msg, path, ex_variant_customized_union_json_shape_explanation))
    : ex_variant_customized_union Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_variant_customized_union_of_json =
  (fun x -> ex_variant_customized_union_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_variant_customized_union option)
[@@warning "-39"]

let ex_variant_customized_union_decl =
  Bindoj_test_common_typedesc_examples.Ex_variant.Customized_union.decl

let ex_variant_customized_union_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_variant.Customized_union.decl
    ex_variant_customized_union_reflect
