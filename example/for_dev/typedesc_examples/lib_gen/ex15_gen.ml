type nested_variant =
  | Student1 of { student : Ex01_gen.student }
  | Student2 of { student : Ex01_gen.student }
  | Student3 of Ex01_gen.student
  | Student4 of Ex01_gen.student
  | Int_list1 of Ex03_gen.int_list
  | Int_list2 of Ex03_gen.int_list

let rec (nested_variant_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_Student1 =
       Refl.InlineRecord
         {
           get =
             (function
             | Student1 { student } ->
                 StringMap.of_list
                   [
                     ("student", (Expr.of_refl Ex01_gen.student_reflect) student);
                   ]
             | _ -> invalid_arg "Student1 is expected");
           mk =
             (fun xs ->
               xs
               |> StringMap.find_opt "student"
               >>= Expr.to_refl Ex01_gen.student_reflect
               >>= fun student -> Some (Student1 { student }));
         }
     in
     let ctor_Student2 =
       Refl.InlineRecord
         {
           get =
             (function
             | Student2 { student } ->
                 StringMap.of_list
                   [
                     ("student", (Expr.of_refl Ex01_gen.student_reflect) student);
                   ]
             | _ -> invalid_arg "Student2 is expected");
           mk =
             (fun xs ->
               xs
               |> StringMap.find_opt "student"
               >>= Expr.to_refl Ex01_gen.student_reflect
               >>= fun student -> Some (Student2 { student }));
         }
     in
     let ctor_Student3 =
       Refl.TupleLike
         {
           get =
             (function
             | Student3 x -> [ (Expr.of_refl Ex01_gen.student_reflect) x ]
             | _ -> invalid_arg "Student3 is expected");
           mk =
             (function
             | x :: [] ->
                 (Expr.to_refl Ex01_gen.student_reflect) x
                 |> Option.map (fun x -> Student3 x)
             | _ -> None);
         }
     in
     let ctor_Student4 =
       Refl.TupleLike
         {
           get =
             (function
             | Student4 x -> [ (Expr.of_refl Ex01_gen.student_reflect) x ]
             | _ -> invalid_arg "Student4 is expected");
           mk =
             (function
             | x :: [] ->
                 (Expr.to_refl Ex01_gen.student_reflect) x
                 |> Option.map (fun x -> Student4 x)
             | _ -> None);
         }
     in
     let ctor_Int_list1 =
       Refl.TupleLike
         {
           get =
             (function
             | Int_list1 x -> [ (Expr.of_refl Ex03_gen.int_list_reflect) x ]
             | _ -> invalid_arg "Int_list1 is expected");
           mk =
             (function
             | x :: [] ->
                 (Expr.to_refl Ex03_gen.int_list_reflect) x
                 |> Option.map (fun x -> Int_list1 x)
             | _ -> None);
         }
     in
     let ctor_Int_list2 =
       Refl.TupleLike
         {
           get =
             (function
             | Int_list2 x -> [ (Expr.of_refl Ex03_gen.int_list_reflect) x ]
             | _ -> invalid_arg "Int_list2 is expected");
           mk =
             (function
             | x :: [] ->
                 (Expr.to_refl Ex03_gen.int_list_reflect) x
                 |> Option.map (fun x -> Int_list2 x)
             | _ -> None);
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list
             [
               ("Student1", ctor_Student1);
               ("Student2", ctor_Student2);
               ("Student3", ctor_Student3);
               ("Student4", ctor_Student4);
               ("Int_list1", ctor_Int_list1);
               ("Int_list2", ctor_Int_list2);
             ];
         classify =
           (function
           | Student1 _ -> ("Student1", ctor_Student1)
           | Student2 _ -> ("Student2", ctor_Student2)
           | Student3 _ -> ("Student3", ctor_Student3)
           | Student4 _ -> ("Student4", ctor_Student4)
           | Int_list1 _ -> ("Int_list1", ctor_Int_list1)
           | Int_list2 _ -> ("Int_list2", ctor_Int_list2));
       })
[@@warning "-33-39"]

let nested_variant_json_discriminator_value =
  (function
   | Student1 _ -> "student1"
   | Student2 _ -> "student2"
   | Student3 _ -> "student3"
   | Student4 _ -> "student4"
   | Int_list1 _ -> "int-list1"
   | Int_list2 _ -> "int-list2"
    : nested_variant -> string)
[@@warning "-39"]

let nested_variant_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "NestedVariant",
           `anyone_of
             [
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "student1"));
                   `mandatory_field
                     ( "student",
                       `named
                         ( "Student",
                           `object_of
                             [
                               `mandatory_field ("admissionYear", `integral);
                               `mandatory_field ("name", `string);
                             ] ) );
                 ];
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "student2"));
                   `mandatory_field ("admissionYear", `integral);
                   `mandatory_field ("name", `string);
                 ];
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "student3"));
                   `mandatory_field
                     ( "value",
                       `named
                         ( "Student",
                           `object_of
                             [
                               `mandatory_field ("admissionYear", `integral);
                               `mandatory_field ("name", `string);
                             ] ) );
                 ];
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "student4"));
                   `mandatory_field ("admissionYear", `integral);
                   `mandatory_field ("name", `string);
                 ];
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "int-list1"));
                   `mandatory_field
                     ( "value",
                       `named
                         ( "IntList",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "intnil"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "intcons"));
                                   `mandatory_field
                                     ("arg", `tuple_of [ `integral; `self ]);
                                 ];
                             ] ) );
                 ];
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "int-list2"));
                   `mandatory_field ("kind", `exactly (`str "intnil"));
                 ];
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "int-list2"));
                   `mandatory_field ("kind", `exactly (`str "intcons"));
                   `mandatory_field ("arg", `tuple_of [ `integral; `self ]);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec nested_variant_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   let rec ex01_gen__student_to_json_nested =
     (fun { admission_year = x0; name = x1 } ->
        [ ("admissionYear", int_to_json x0); ("name", string_to_json x1) ]
       : Ex01_gen.student -> (string * Kxclib.Json.jv) list)
   and ex03_gen__int_list_to_json_nested =
     (function
      | IntNil -> [ ("kind", `str "intnil") ]
      | IntCons (x0, x1) ->
          [
            ("kind", `str "intcons");
            ( "arg",
              `arr
                [ int_to_json x0; `obj (ex03_gen__int_list_to_json_nested x1) ]
            );
          ]
       : Ex03_gen.int_list -> (string * Kxclib.Json.jv) list)
   in
   function
   | Student1 { student = x0 } ->
       `obj
         [
           ("tag", `str "student1");
           ("student", `obj (ex01_gen__student_to_json_nested x0));
         ]
   | Student2 { student = x0 } ->
       `obj (("tag", `str "student2") :: ex01_gen__student_to_json_nested x0)
   | Student3 x0 ->
       `obj
         [
           ("tag", `str "student3");
           ("value", `obj (ex01_gen__student_to_json_nested x0));
         ]
   | Student4 x0 ->
       `obj (("tag", `str "student4") :: ex01_gen__student_to_json_nested x0)
   | Int_list1 x0 ->
       `obj
         [
           ("tag", `str "int-list1");
           ("value", `obj (ex03_gen__int_list_to_json_nested x0));
         ]
   | Int_list2 x0 ->
       `obj (("tag", `str "int-list2") :: ex03_gen__int_list_to_json_nested x0)
    : nested_variant -> Kxclib.Json.jv)
[@@warning "-39"]

and nested_variant_of_json' =
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
        and ex03_gen__int_list_of_json_nested path __bindoj_orig =
          match Kxclib.Jv.pump_field "kind" __bindoj_orig with
          | `obj (("kind", `str "intnil") :: _) ->
              Ok (IntNil : Ex03_gen.int_list)
          | `obj (("kind", `str "intcons") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some (`arr [ x0; x1 ]) ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`i 0 :: `f "arg" :: path) x0 >>= fun x0 ->
                  ex03_gen__int_list_of_json_nested (`i 1 :: `f "arg" :: path)
                    x1
                  >>= fun x1 -> Ok (IntCons (x0, x1) : Ex03_gen.int_list)
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
        fun path __bindoj_orig ->
          match Kxclib.Jv.pump_field "tag" __bindoj_orig with
          | `obj (("tag", `str "student1") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "student" param
              |> Option.to_result
                   ~none:("mandatory field 'student' does not exist", path)
              >>= ex01_gen__student_of_json_nested (`f "student" :: path)
              >>= fun x0 -> Ok (Student1 { student = x0 })
          | `obj (("tag", `str "student2") :: _) ->
              let ( >>= ) = Result.bind in
              ex01_gen__student_of_json_nested path __bindoj_orig >>= fun x0 ->
              Ok (Student2 { student = x0 })
          | `obj (("tag", `str "student3") :: param) -> (
              match List.assoc_opt "value" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  ex01_gen__student_of_json_nested (`f "value" :: path) arg
                  >>= fun x0 -> Ok (Student3 x0)
              | None -> Error ("mandatory field 'value' does not exist", path))
          | `obj (("tag", `str "student4") :: _) ->
              let ( >>= ) = Result.bind in
              ex01_gen__student_of_json_nested path __bindoj_orig >>= fun x0 ->
              Ok (Student4 x0)
          | `obj (("tag", `str "int-list1") :: param) -> (
              match List.assoc_opt "value" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  ex03_gen__int_list_of_json_nested (`f "value" :: path) arg
                  >>= fun x0 -> Ok (Int_list1 x0)
              | None -> Error ("mandatory field 'value' does not exist", path))
          | `obj (("tag", `str "int-list2") :: _) ->
              let ( >>= ) = Result.bind in
              ex03_gen__int_list_of_json_nested path __bindoj_orig >>= fun x0 ->
              Ok (Int_list2 x0)
          | `obj (("tag", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'student1', 'student2', 'student3', 'student4', \
                     'int-list1', 'int-list2' ]"
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
            (msg, path, nested_variant_json_shape_explanation))
    : nested_variant Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and nested_variant_of_json =
  (fun x -> nested_variant_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> nested_variant option)
[@@warning "-39"]

let nested_variant_decl = Bindoj_test_common_typedesc_examples.Ex15.decl

let nested_variant_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex15.decl nested_variant_reflect
