type ex_ident_student_pair = {
  student1 : Ex_record_gen.ex_record_student;
  student2 : Ex_record_gen.ex_record_student;
}

let rec (ex_ident_student_pair_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { student1; student2 } ->
             StringMap.of_list
               [
                 ( "student1",
                   (Expr.of_refl Ex_record_gen.ex_record_student_reflect)
                     student1 );
                 ( "student2",
                   (Expr.of_refl Ex_record_gen.ex_record_student_reflect)
                     student2 );
               ]);
         mk =
           (fun xs ->
             xs
             |> StringMap.find_opt "student1"
             >>= Expr.to_refl Ex_record_gen.ex_record_student_reflect
             >>= fun student1 ->
             xs
             |> StringMap.find_opt "student2"
             >>= Expr.to_refl Ex_record_gen.ex_record_student_reflect
             >>= fun student2 -> Some { student1; student2 });
       })
[@@warning "-33-39"]

let ex_ident_student_pair_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExIdentStudentPair",
           `object_of
             [
               `mandatory_field
                 ( "student1",
                   match
                     let open Ex_record_gen in
                     ex_record_student_json_shape_explanation
                   with
                   | `with_warning (_, (`named _ as s)) -> s
                   | `with_warning (_, s) | s -> `named ("ExRecordStudent", s)
                 );
               `mandatory_field
                 ( "student2",
                   match
                     let open Ex_record_gen in
                     ex_record_student_json_shape_explanation
                   with
                   | `with_warning (_, (`named _ as s)) -> s
                   | `with_warning (_, s) | s -> `named ("ExRecordStudent", s)
                 );
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_ident_student_pair_to_json =
  (fun { student1 = x0; student2 = x1 } ->
     `obj
       [
         ( "student1",
           (let open Ex_record_gen in
            ex_record_student_to_json)
             x0 );
         ( "student2",
           (let open Ex_record_gen in
            ex_record_student_to_json)
             x1 );
       ]
    : ex_ident_student_pair -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_ident_student_pair_of_json' =
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl path __bindoj_orig =
         match __bindoj_orig with
         | `obj param ->
             let ( >>= ) = Result.bind in
             List.assoc_opt "student1" param
             |> Option.to_result
                  ~none:("mandatory field 'student1' does not exist", path)
             >>= (fun path ->
                   fun x ->
                    (let open Ex_record_gen in
                     ex_record_student_of_json')
                      ~path x
                    |> Result.map_error (fun (msg, path, _) -> (msg, path)))
                   (`f "student1" :: path)
             >>= fun x0 ->
             List.assoc_opt "student2" param
             |> Option.to_result
                  ~none:("mandatory field 'student2' does not exist", path)
             >>= (fun path ->
                   fun x ->
                    (let open Ex_record_gen in
                     ex_record_student_of_json')
                      ~path x
                    |> Result.map_error (fun (msg, path, _) -> (msg, path)))
                   (`f "student2" :: path)
             >>= fun x1 -> Ok { student1 = x0; student2 = x1 }
         | jv ->
             Error
               ( Printf.sprintf
                   "an object is expected for a record value, but the given is \
                    of type '%s'"
                   (let open Kxclib.Json in
                    string_of_jv_kind (classify_jv jv)),
                 path )
       in
       of_json_impl)
        path x
      |> Result.map_error (fun (msg, path) ->
             (msg, path, ex_ident_student_pair_json_shape_explanation))
    : ex_ident_student_pair Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_ident_student_pair_of_json =
  (fun x -> ex_ident_student_pair_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_ident_student_pair option)
[@@warning "-39"]

let ex_ident_student_pair_decl =
  Bindoj_test_common_typedesc_examples.Ex_ident.Student_pair.decl

let ex_ident_student_pair_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_ident.Student_pair.decl
    ex_ident_student_pair_reflect
