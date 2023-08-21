type student = {
  admission_year : int;
  name : string;
  case_value : [ `Case_at0 | `case_at1 ];
}

let rec (student_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { admission_year; name; case_value } ->
             StringMap.of_list
               [
                 ("admission_year", Expr.of_int admission_year);
                 ("name", Expr.of_string name);
                 ( "case_value",
                   (function
                     | `Case_at0 -> Expr.StringEnum "Case_at0"
                     | `case_at1 -> Expr.StringEnum "case_at1")
                     case_value );
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "admission_year" >>= Expr.to_int
             >>= fun admission_year ->
             xs |> StringMap.find_opt "name" >>= Expr.to_string >>= fun name ->
             (xs |> StringMap.find_opt "case_value" >>= function
              | Expr.StringEnum "Case_at0" -> Some `Case_at0
              | Expr.StringEnum "case_at1" -> Some `case_at1
              | _ -> None)
             >>= fun case_value -> Some { admission_year; name; case_value });
       })
[@@warning "-33-39"]

let student_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "student_inherited_mangling",
           `object_of
             [
               `mandatory_field ("admission_year", `integral);
               `mandatory_field ("name", `string);
               `mandatory_field
                 ("caseValue", `string_enum [ "Case-at0"; "case_at1" ]);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec student_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun { admission_year = x0; name = x1; case_value = x2 } ->
     `obj
       [
         ("admission_year", int_to_json x0);
         ("name", string_to_json x1);
         ( "caseValue",
           (function
             | `Case_at0 -> `str "Case-at0" | `case_at1 -> `str "case_at1")
             x2 );
       ]
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
              List.assoc_opt "admission_year" param
              |> Option.to_result
                   ~none:
                     ("mandatory field 'admission_year' does not exist", path)
              >>= int_of_json' (`f "admission_year" :: path)
              >>= fun x0 ->
              List.assoc_opt "name" param
              |> Option.to_result
                   ~none:("mandatory field 'name' does not exist", path)
              >>= string_of_json' (`f "name" :: path)
              >>= fun x1 ->
              List.assoc_opt "caseValue" param
              |> Option.to_result
                   ~none:("mandatory field 'caseValue' does not exist", path)
              >>= (fun path -> function
                    | `str s ->
                        (function
                          | "Case-at0" -> Ok `Case_at0
                          | "case_at1" -> Ok `case_at1
                          | s ->
                              Error
                                ( Printf.sprintf
                                    "given string '%s' is not one of [ \
                                     'Case-at0', 'case_at1' ]"
                                    s,
                                  path ))
                          s
                    | jv ->
                        Error
                          ( Printf.sprintf
                              "expecting type 'string' but the given is of \
                               type '%s'"
                              (let open Kxclib.Json in
                               string_of_jv_kind (classify_jv jv)),
                            path ))
                    (`f "caseValue" :: path)
              >>= fun x2 ->
              Ok { admission_year = x0; name = x1; case_value = x2 }
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

let student_decl =
  Bindoj_test_common_typedesc_examples.Ex01_inherited_mangling.decl

let student_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex01_inherited_mangling.decl
    student_reflect
