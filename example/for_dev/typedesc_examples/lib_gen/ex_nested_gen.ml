type ex_nested_point2 = { x : float; y : float }

let rec (ex_nested_point2_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { x; y } ->
             StringMap.of_list
               [ ("x", Expr.of_float x); ("y", Expr.of_float y) ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "x" >>= Expr.to_float >>= fun x ->
             xs |> StringMap.find_opt "y" >>= Expr.to_float >>= fun y ->
             Some { x; y });
       })
[@@warning "-33-39"]

let ex_nested_point2_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExNestedPoint2",
           `object_of
             [
               `mandatory_field ("x", `proper_float);
               `mandatory_field ("y", `proper_float);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_nested_point2_to_json =
  (let float_to_json (x : float) : Kxclib.Json.jv = `num x in
   fun { x = x0; y = x1 } ->
     `obj [ ("x", float_to_json x0); ("y", float_to_json x1) ]
    : ex_nested_point2 -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_nested_point2_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl =
        let float_of_json' path = function
          | (`num x : Kxclib.Json.jv) -> Ok x
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'float' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        in
        fun path __bindoj_orig ->
          match __bindoj_orig with
          | `obj param ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "x" param
              |> Option.to_result
                   ~none:("mandatory field 'x' does not exist", path)
              >>= float_of_json' (`f "x" :: path)
              >>= fun x0 ->
              List.assoc_opt "y" param
              |> Option.to_result
                   ~none:("mandatory field 'y' does not exist", path)
              >>= float_of_json' (`f "y" :: path)
              >>= fun x1 -> Ok { x = x0; y = x1 }
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
            (msg, path, ex_nested_point2_json_shape_explanation))
    : ex_nested_point2 Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_nested_point2_of_json =
  (fun x -> ex_nested_point2_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_nested_point2 option)
[@@warning "-39"]

let ex_nested_point2_decl =
  Bindoj_test_common_typedesc_examples.Ex_nested.Point2.decl

let ex_nested_point2_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_nested.Point2.decl
    ex_nested_point2_reflect

type ex_nested_record = {
  unit : Ex_alias_gen.ex_alias_unit;
  point2 : ex_nested_point2;
  point2_spread : ex_nested_point2;
  person : Ex_mangling_gen.ex_mangling_person_inherited;
  optional_variant : Ex_optional_gen.ex_optional_variant;
  person_spread : Ex_mangling_gen.ex_mangling_person_inherited;
}

let rec (ex_nested_record_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun {
                  unit;
                  point2;
                  point2_spread;
                  person;
                  optional_variant;
                  person_spread;
                } ->
             StringMap.of_list
               [
                 ("unit", Expr.of_unit unit);
                 ("point2", (Expr.of_refl ex_nested_point2_reflect) point2);
                 ( "point2_spread",
                   (Expr.of_refl ex_nested_point2_reflect) point2_spread );
                 ( "person",
                   (Expr.of_refl
                      Ex_mangling_gen.ex_mangling_person_inherited_reflect)
                     person );
                 ( "optional_variant",
                   (Expr.of_refl Ex_optional_gen.ex_optional_variant_reflect)
                     optional_variant );
                 ( "person_spread",
                   (Expr.of_refl
                      Ex_mangling_gen.ex_mangling_person_inherited_reflect)
                     person_spread );
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "unit" >>= Expr.to_unit >>= fun unit ->
             xs
             |> StringMap.find_opt "point2"
             >>= Expr.to_refl ex_nested_point2_reflect
             >>= fun point2 ->
             xs
             |> StringMap.find_opt "point2_spread"
             >>= Expr.to_refl ex_nested_point2_reflect
             >>= fun point2_spread ->
             xs
             |> StringMap.find_opt "person"
             >>= Expr.to_refl
                   Ex_mangling_gen.ex_mangling_person_inherited_reflect
             >>= fun person ->
             xs
             |> StringMap.find_opt "optional_variant"
             >>= Expr.to_refl Ex_optional_gen.ex_optional_variant_reflect
             >>= fun optional_variant ->
             xs
             |> StringMap.find_opt "person_spread"
             >>= Expr.to_refl
                   Ex_mangling_gen.ex_mangling_person_inherited_reflect
             >>= fun person_spread ->
             Some
               {
                 unit;
                 point2;
                 point2_spread;
                 person;
                 optional_variant;
                 person_spread;
               });
       })
[@@warning "-33-39"]

let ex_nested_record_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExNestedRecord",
           `anyone_of
             [
               `object_of
                 [
                   `mandatory_field
                     ( "unit",
                       `named ("ExAliasUnit", `special ("unit", `exactly `null))
                     );
                   `mandatory_field
                     ( "point2",
                       `named
                         ( "ExNestedPoint2",
                           `object_of
                             [
                               `mandatory_field ("x", `proper_float);
                               `mandatory_field ("y", `proper_float);
                             ] ) );
                   `mandatory_field ("x", `proper_float);
                   `mandatory_field ("y", `proper_float);
                   `mandatory_field
                     ( "person",
                       `named
                         ( "ex_mangling_person_inherited",
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
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "student"));
                                   `mandatory_field ("student_id", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field
                                     ( "caseValue",
                                       `string_enum [ "Case_at0"; "case-at1" ]
                                     );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
                   `mandatory_field
                     ( "optionalVariant",
                       `named
                         ( "ExOptionalVariant",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like"));
                                   `optional_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like-alias"));
                                   `optional_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like-obj"));
                                   `optional_field ("_0", `integral);
                                   `optional_field ("_1", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "tuple-like-spreading") );
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "inline-record"));
                                   `optional_field ("intOpt", `integral);
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                   `mandatory_field
                                     ( "objtuple",
                                       `object_of
                                         [
                                           `optional_field ("_0", `integral);
                                           `optional_field ("_1", `integral);
                                         ] );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "inline-record-spreading")
                                     );
                                   `optional_field ("intOpt", `integral);
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "reused-inline-record") );
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                             ] ) );
                   `mandatory_field ("kind", `exactly (`str "Anonymous"));
                 ];
               `object_of
                 [
                   `mandatory_field
                     ( "unit",
                       `named ("ExAliasUnit", `special ("unit", `exactly `null))
                     );
                   `mandatory_field
                     ( "point2",
                       `named
                         ( "ExNestedPoint2",
                           `object_of
                             [
                               `mandatory_field ("x", `proper_float);
                               `mandatory_field ("y", `proper_float);
                             ] ) );
                   `mandatory_field ("x", `proper_float);
                   `mandatory_field ("y", `proper_float);
                   `mandatory_field
                     ( "person",
                       `named
                         ( "ex_mangling_person_inherited",
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
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "student"));
                                   `mandatory_field ("student_id", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field
                                     ( "caseValue",
                                       `string_enum [ "Case_at0"; "case-at1" ]
                                     );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
                   `mandatory_field
                     ( "optionalVariant",
                       `named
                         ( "ExOptionalVariant",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like"));
                                   `optional_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like-alias"));
                                   `optional_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like-obj"));
                                   `optional_field ("_0", `integral);
                                   `optional_field ("_1", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "tuple-like-spreading") );
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "inline-record"));
                                   `optional_field ("intOpt", `integral);
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                   `mandatory_field
                                     ( "objtuple",
                                       `object_of
                                         [
                                           `optional_field ("_0", `integral);
                                           `optional_field ("_1", `integral);
                                         ] );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "inline-record-spreading")
                                     );
                                   `optional_field ("intOpt", `integral);
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "reused-inline-record") );
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                             ] ) );
                   `mandatory_field ("kind", `exactly (`str "With_id"));
                   `mandatory_field ("value", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field
                     ( "unit",
                       `named ("ExAliasUnit", `special ("unit", `exactly `null))
                     );
                   `mandatory_field
                     ( "point2",
                       `named
                         ( "ExNestedPoint2",
                           `object_of
                             [
                               `mandatory_field ("x", `proper_float);
                               `mandatory_field ("y", `proper_float);
                             ] ) );
                   `mandatory_field ("x", `proper_float);
                   `mandatory_field ("y", `proper_float);
                   `mandatory_field
                     ( "person",
                       `named
                         ( "ex_mangling_person_inherited",
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
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "student"));
                                   `mandatory_field ("student_id", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field
                                     ( "caseValue",
                                       `string_enum [ "Case_at0"; "case-at1" ]
                                     );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
                   `mandatory_field
                     ( "optionalVariant",
                       `named
                         ( "ExOptionalVariant",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like"));
                                   `optional_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like-alias"));
                                   `optional_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like-obj"));
                                   `optional_field ("_0", `integral);
                                   `optional_field ("_1", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "tuple-like-spreading") );
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "inline-record"));
                                   `optional_field ("intOpt", `integral);
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                   `mandatory_field
                                     ( "objtuple",
                                       `object_of
                                         [
                                           `optional_field ("_0", `integral);
                                           `optional_field ("_1", `integral);
                                         ] );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "inline-record-spreading")
                                     );
                                   `optional_field ("intOpt", `integral);
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "reused-inline-record") );
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                             ] ) );
                   `mandatory_field ("kind", `exactly (`str "student"));
                   `mandatory_field ("student_id", `integral);
                   `mandatory_field ("name", `string);
                   `mandatory_field
                     ("caseValue", `string_enum [ "Case_at0"; "case-at1" ]);
                 ];
               `object_of
                 [
                   `mandatory_field
                     ( "unit",
                       `named ("ExAliasUnit", `special ("unit", `exactly `null))
                     );
                   `mandatory_field
                     ( "point2",
                       `named
                         ( "ExNestedPoint2",
                           `object_of
                             [
                               `mandatory_field ("x", `proper_float);
                               `mandatory_field ("y", `proper_float);
                             ] ) );
                   `mandatory_field ("x", `proper_float);
                   `mandatory_field ("y", `proper_float);
                   `mandatory_field
                     ( "person",
                       `named
                         ( "ex_mangling_person_inherited",
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
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "student"));
                                   `mandatory_field ("student_id", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field
                                     ( "caseValue",
                                       `string_enum [ "Case_at0"; "case-at1" ]
                                     );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
                   `mandatory_field
                     ( "optionalVariant",
                       `named
                         ( "ExOptionalVariant",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like"));
                                   `optional_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like-alias"));
                                   `optional_field ("arg", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "tuple-like-obj"));
                                   `optional_field ("_0", `integral);
                                   `optional_field ("_1", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "tuple-like-spreading") );
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "inline-record"));
                                   `optional_field ("intOpt", `integral);
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                   `mandatory_field
                                     ( "objtuple",
                                       `object_of
                                         [
                                           `optional_field ("_0", `integral);
                                           `optional_field ("_1", `integral);
                                         ] );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "inline-record-spreading")
                                     );
                                   `optional_field ("intOpt", `integral);
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "tag",
                                       `exactly (`str "reused-inline-record") );
                                   `optional_field ("xOpt", `integral);
                                   `optional_field ("yOpt", `integral);
                                 ];
                             ] ) );
                   `mandatory_field ("kind", `exactly (`str "Teacher"));
                   `mandatory_field ("facultyId", `integral);
                   `mandatory_field ("name", `string);
                   `mandatory_field ("department", `string);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_nested_record_to_json =
  (let unit_to_json () : Kxclib.Json.jv = `num 1.
   and string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x)
   and float_to_json (x : float) : Kxclib.Json.jv = `num x in
   let rec ex_mangling_gen__ex_mangling_person_inherited_to_json_nested =
     (function
      | Anonymous -> [ ("kind", `str "Anonymous") ]
      | With_id x0 -> [ ("kind", `str "With_id"); ("value", int_to_json x0) ]
      | Student { student_id = x0; name = x1; case_value = x2 } ->
          [
            ("kind", `str "student");
            ("student_id", int_to_json x0);
            ("name", string_to_json x1);
            ( "caseValue",
              (function
                | `Case_at0 -> `str "Case_at0" | `case_at1 -> `str "case-at1")
                x2 );
          ]
      | Teacher { faculty_id = x0; name = x1; department = x2 } ->
          [
            ("kind", `str "Teacher");
            ("facultyId", int_to_json x0);
            ("name", string_to_json x1);
            ("department", string_to_json x2);
          ]
       : Ex_mangling_gen.ex_mangling_person_inherited ->
         (string * Kxclib.Json.jv) list)
   and ex_optional_gen__ex_optional_variant_to_json_nested =
     (function
      | Tuple_like None -> [ ("tag", `str "tuple-like") ]
      | Tuple_like (Some x0) ->
          [ ("tag", `str "tuple-like"); ("arg", int_to_json x0) ]
      | Tuple_like_alias None -> [ ("tag", `str "tuple-like-alias") ]
      | Tuple_like_alias (Some x0) ->
          [ ("tag", `str "tuple-like-alias"); ("arg", int_to_json x0) ]
      | Tuple_like_obj (x0, x1) ->
          ("tag", `str "tuple-like-obj")
          :: List.filter_map Kxclib.identity
               [
                 Option.map (fun x0 -> ("_0", int_to_json x0)) x0;
                 Option.map (fun x1 -> ("_1", int_to_json x1)) x1;
               ]
      | Tuple_like_spreading x0 ->
          ("tag", `str "tuple-like-spreading")
          :: ex_optional_gen__ex_optional_xy_opt_to_json_nested x0
      | Inline_record { int_opt = x0; x_opt = x1; y_opt = x2; objtuple = x3 } ->
          ("tag", `str "inline-record")
          :: (List.filter_map
                (fun x -> x)
                [
                  Option.map (fun x0 -> ("intOpt", int_to_json x0)) x0;
                  Option.map (fun x1 -> ("xOpt", int_to_json x1)) x1;
                  Option.map (fun x2 -> ("yOpt", int_to_json x2)) x2;
                ]
             @ [
                 ( "objtuple",
                   (fun (x0, x1) : Kxclib.Json.jv ->
                     `obj
                       (List.filter_map Kxclib.identity
                          [
                            Option.map (fun x0 -> ("_0", int_to_json x0)) x0;
                            Option.map (fun x1 -> ("_1", int_to_json x1)) x1;
                          ]))
                     x3 );
               ])
      | Inline_record_spreading { int_opt = x0; xy_opt = x1 } ->
          ("tag", `str "inline-record-spreading")
          :: (List.filter_map
                (fun x -> x)
                [ Option.map (fun x0 -> ("intOpt", int_to_json x0)) x0 ]
             @ ex_optional_gen__ex_optional_xy_opt_to_json_nested x1)
      | Reused_inline_record { x_opt = x0; y_opt = x1 } ->
          ("tag", `str "reused-inline-record")
          :: List.filter_map
               (fun x -> x)
               [
                 Option.map (fun x0 -> ("xOpt", int_to_json x0)) x0;
                 Option.map (fun x1 -> ("yOpt", int_to_json x1)) x1;
               ]
       : Ex_optional_gen.ex_optional_variant -> (string * Kxclib.Json.jv) list)
   and ex_optional_gen__ex_optional_xy_opt_to_json_nested =
     (fun { x_opt = x0; y_opt = x1 } ->
        List.filter_map
          (fun x -> x)
          [
            Option.map (fun x0 -> ("xOpt", int_to_json x0)) x0;
            Option.map (fun x1 -> ("yOpt", int_to_json x1)) x1;
          ]
       : Ex_optional_gen.ex_optional_xy_opt -> (string * Kxclib.Json.jv) list)
   and ex_nested_point2_to_json_nested =
     (fun { x = x0; y = x1 } ->
        [ ("x", float_to_json x0); ("y", float_to_json x1) ]
       : ex_nested_point2 -> (string * Kxclib.Json.jv) list)
   in
   fun {
         unit = x0;
         point2 = x1;
         point2_spread = x2;
         person = x3;
         optional_variant = x4;
         person_spread = x5;
       } ->
     `obj
       ([
          ("unit", unit_to_json x0);
          ("point2", `obj (ex_nested_point2_to_json_nested x1));
        ]
       @ ex_nested_point2_to_json_nested x2
       @ [
           ( "person",
             `obj
               (ex_mangling_gen__ex_mangling_person_inherited_to_json_nested x3)
           );
           ( "optionalVariant",
             `obj (ex_optional_gen__ex_optional_variant_to_json_nested x4) );
         ]
       @ ex_mangling_gen__ex_mangling_person_inherited_to_json_nested x5)
    : ex_nested_record -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_nested_record_of_json' =
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
        and option_of_json' t_of_json path = function
          | `null -> Ok None
          | x -> (
              match t_of_json path x with
              | Ok x -> Ok (Some x)
              | Error msg -> Error msg)
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
        let rec ex_mangling_gen__ex_mangling_person_inherited_of_json_nested
            path __bindoj_orig =
          match Kxclib.Jv.pump_field "kind" __bindoj_orig with
          | `obj (("kind", `str "Anonymous") :: _) ->
              Ok (Anonymous : Ex_mangling_gen.ex_mangling_person_inherited)
          | `obj (("kind", `str "With_id") :: param) -> (
              match List.assoc_opt "value" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`f "value" :: path) arg >>= fun x0 ->
                  Ok (With_id x0 : Ex_mangling_gen.ex_mangling_person_inherited)
              | None -> Error ("mandatory field 'value' does not exist", path))
          | `obj (("kind", `str "student") :: param) ->
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
              List.assoc_opt "caseValue" param
              |> Option.to_result
                   ~none:("mandatory field 'caseValue' does not exist", path)
              >>= (fun path -> function
                    | `str s ->
                        (function
                          | "Case_at0" -> Ok `Case_at0
                          | "case-at1" -> Ok `case_at1
                          | s ->
                              Error
                                ( Printf.sprintf
                                    "given string '%s' is not one of [ \
                                     'Case_at0', 'case-at1' ]"
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
              Ok
                (Student { student_id = x0; name = x1; case_value = x2 }
                  : Ex_mangling_gen.ex_mangling_person_inherited)
          | `obj (("kind", `str "Teacher") :: param) ->
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
              Ok
                (Teacher { faculty_id = x0; name = x1; department = x2 }
                  : Ex_mangling_gen.ex_mangling_person_inherited)
          | `obj (("kind", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'Anonymous', 'With_id', 'student', 'Teacher' ]"
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
        and ex_optional_gen__ex_optional_variant_of_json_nested path
            __bindoj_orig =
          match Kxclib.Jv.pump_field "tag" __bindoj_orig with
          | `obj (("tag", `str "tuple-like") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "arg" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "arg" :: path)
              >>= fun x0 ->
              Ok (Tuple_like x0 : Ex_optional_gen.ex_optional_variant)
          | `obj (("tag", `str "tuple-like-alias") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "arg" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "arg" :: path)
              >>= fun x0 ->
              Ok (Tuple_like_alias x0 : Ex_optional_gen.ex_optional_variant)
          | `obj (("tag", `str "tuple-like-obj") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "_0" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "_0" :: path)
              >>= fun x0 ->
              List.assoc_opt "_1" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "_1" :: path)
              >>= fun x1 ->
              Ok (Tuple_like_obj (x0, x1) : Ex_optional_gen.ex_optional_variant)
          | `obj (("tag", `str "tuple-like-spreading") :: _) ->
              let ( >>= ) = Result.bind in
              ex_optional_gen__ex_optional_xy_opt_of_json_nested path
                __bindoj_orig
              >>= fun x0 ->
              Ok (Tuple_like_spreading x0 : Ex_optional_gen.ex_optional_variant)
          | `obj (("tag", `str "inline-record") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "intOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "intOpt" :: path)
              >>= fun x0 ->
              List.assoc_opt "xOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "xOpt" :: path)
              >>= fun x1 ->
              List.assoc_opt "yOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "yOpt" :: path)
              >>= fun x2 ->
              List.assoc_opt "objtuple" param
              |> Option.to_result
                   ~none:("mandatory field 'objtuple' does not exist", path)
              >>= (fun path -> function
                    | (`obj fields : Kxclib.Json.jv) ->
                        let ( >>= ) = Result.bind in
                        List.assoc_opt "_0" fields
                        |> Option.value ~default:`null
                        |> (option_of_json' int_of_json') (`f "_0" :: path)
                        >>= fun x0 ->
                        List.assoc_opt "_1" fields
                        |> Option.value ~default:`null
                        |> (option_of_json' int_of_json') (`f "_1" :: path)
                        >>= fun x1 -> Ok (x0, x1)
                    | jv ->
                        Error
                          ( Printf.sprintf
                              "an object is expected for a tuple value, but \
                               the given is of type '%s'"
                              (let open Kxclib.Json in
                               string_of_jv_kind (classify_jv jv)),
                            path ))
                    (`f "objtuple" :: path)
              >>= fun x3 ->
              Ok
                (Inline_record
                   { int_opt = x0; x_opt = x1; y_opt = x2; objtuple = x3 }
                  : Ex_optional_gen.ex_optional_variant)
          | `obj (("tag", `str "inline-record-spreading") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "intOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "intOpt" :: path)
              >>= fun x0 ->
              ex_optional_gen__ex_optional_xy_opt_of_json_nested path
                __bindoj_orig
              >>= fun x1 ->
              Ok
                (Inline_record_spreading { int_opt = x0; xy_opt = x1 }
                  : Ex_optional_gen.ex_optional_variant)
          | `obj (("tag", `str "reused-inline-record") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "xOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "xOpt" :: path)
              >>= fun x0 ->
              List.assoc_opt "yOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "yOpt" :: path)
              >>= fun x1 ->
              Ok
                (Reused_inline_record { x_opt = x0; y_opt = x1 }
                  : Ex_optional_gen.ex_optional_variant)
          | `obj (("tag", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'tuple-like', 'tuple-like-alias', 'tuple-like-obj', \
                     'tuple-like-spreading', 'inline-record', \
                     'inline-record-spreading', 'reused-inline-record' ]"
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
        and ex_optional_gen__ex_optional_xy_opt_of_json_nested path
            __bindoj_orig =
          match __bindoj_orig with
          | `obj param ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "xOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "xOpt" :: path)
              >>= fun x0 ->
              List.assoc_opt "yOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "yOpt" :: path)
              >>= fun x1 ->
              Ok
                ({ x_opt = x0; y_opt = x1 }
                  : Ex_optional_gen.ex_optional_xy_opt)
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a record value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and ex_nested_point2_of_json_nested path __bindoj_orig =
          match __bindoj_orig with
          | `obj param ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "x" param
              |> Option.to_result
                   ~none:("mandatory field 'x' does not exist", path)
              >>= float_of_json' (`f "x" :: path)
              >>= fun x0 ->
              List.assoc_opt "y" param
              |> Option.to_result
                   ~none:("mandatory field 'y' does not exist", path)
              >>= float_of_json' (`f "y" :: path)
              >>= fun x1 -> Ok ({ x = x0; y = x1 } : ex_nested_point2)
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
              List.assoc_opt "point2" param
              |> Option.to_result
                   ~none:("mandatory field 'point2' does not exist", path)
              >>= ex_nested_point2_of_json_nested (`f "point2" :: path)
              >>= fun x1 ->
              ex_nested_point2_of_json_nested path __bindoj_orig >>= fun x2 ->
              List.assoc_opt "person" param
              |> Option.to_result
                   ~none:("mandatory field 'person' does not exist", path)
              >>= ex_mangling_gen__ex_mangling_person_inherited_of_json_nested
                    (`f "person" :: path)
              >>= fun x3 ->
              List.assoc_opt "optionalVariant" param
              |> Option.to_result
                   ~none:
                     ("mandatory field 'optionalVariant' does not exist", path)
              >>= ex_optional_gen__ex_optional_variant_of_json_nested
                    (`f "optionalVariant" :: path)
              >>= fun x4 ->
              ex_mangling_gen__ex_mangling_person_inherited_of_json_nested path
                __bindoj_orig
              >>= fun x5 ->
              Ok
                {
                  unit = x0;
                  point2 = x1;
                  point2_spread = x2;
                  person = x3;
                  optional_variant = x4;
                  person_spread = x5;
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
            (msg, path, ex_nested_record_json_shape_explanation))
    : ex_nested_record Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_nested_record_of_json =
  (fun x -> ex_nested_record_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_nested_record option)
[@@warning "-39"]

let ex_nested_record_decl =
  Bindoj_test_common_typedesc_examples.Ex_nested.Record.decl

let ex_nested_record_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_nested.Record.decl
    ex_nested_record_reflect

type ex_nested_variant =
  | Student1 of { student : Ex_record_gen.ex_record_student }
  | Student2 of { student : Ex_record_gen.ex_record_student }
  | Student3 of Ex_record_gen.ex_record_student
  | Student4 of Ex_record_gen.ex_record_student
  | Int_list1 of Ex_variant_gen.ex_variant_int_list
  | Int_list2 of Ex_variant_gen.ex_variant_int_list

let rec (ex_nested_variant_reflect : _ Bindoj_runtime.Refl.t) =
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
                     ( "student",
                       (Expr.of_refl Ex_record_gen.ex_record_student_reflect)
                         student );
                   ]
             | _ -> invalid_arg "Student1 is expected");
           mk =
             (fun xs ->
               xs
               |> StringMap.find_opt "student"
               >>= Expr.to_refl Ex_record_gen.ex_record_student_reflect
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
                     ( "student",
                       (Expr.of_refl Ex_record_gen.ex_record_student_reflect)
                         student );
                   ]
             | _ -> invalid_arg "Student2 is expected");
           mk =
             (fun xs ->
               xs
               |> StringMap.find_opt "student"
               >>= Expr.to_refl Ex_record_gen.ex_record_student_reflect
               >>= fun student -> Some (Student2 { student }));
         }
     in
     let ctor_Student3 =
       Refl.TupleLike
         {
           get =
             (function
             | Student3 x ->
                 [ (Expr.of_refl Ex_record_gen.ex_record_student_reflect) x ]
             | _ -> invalid_arg "Student3 is expected");
           mk =
             (function
             | x :: [] ->
                 (Expr.to_refl Ex_record_gen.ex_record_student_reflect) x
                 |> Option.map (fun x -> Student3 x)
             | _ -> None);
         }
     in
     let ctor_Student4 =
       Refl.TupleLike
         {
           get =
             (function
             | Student4 x ->
                 [ (Expr.of_refl Ex_record_gen.ex_record_student_reflect) x ]
             | _ -> invalid_arg "Student4 is expected");
           mk =
             (function
             | x :: [] ->
                 (Expr.to_refl Ex_record_gen.ex_record_student_reflect) x
                 |> Option.map (fun x -> Student4 x)
             | _ -> None);
         }
     in
     let ctor_Int_list1 =
       Refl.TupleLike
         {
           get =
             (function
             | Int_list1 x ->
                 [ (Expr.of_refl Ex_variant_gen.ex_variant_int_list_reflect) x ]
             | _ -> invalid_arg "Int_list1 is expected");
           mk =
             (function
             | x :: [] ->
                 (Expr.to_refl Ex_variant_gen.ex_variant_int_list_reflect) x
                 |> Option.map (fun x -> Int_list1 x)
             | _ -> None);
         }
     in
     let ctor_Int_list2 =
       Refl.TupleLike
         {
           get =
             (function
             | Int_list2 x ->
                 [ (Expr.of_refl Ex_variant_gen.ex_variant_int_list_reflect) x ]
             | _ -> invalid_arg "Int_list2 is expected");
           mk =
             (function
             | x :: [] ->
                 (Expr.to_refl Ex_variant_gen.ex_variant_int_list_reflect) x
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

let ex_nested_variant_json_discriminator_value =
  (function
   | Student1 _ -> "student1"
   | Student2 _ -> "student2"
   | Student3 _ -> "student3"
   | Student4 _ -> "student4"
   | Int_list1 _ -> "int-list1"
   | Int_list2 _ -> "int-list2"
    : ex_nested_variant -> string)
[@@warning "-39"]

let ex_nested_variant_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExNestedVariant",
           `anyone_of
             [
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "student1"));
                   `mandatory_field
                     ( "student",
                       `named
                         ( "ExRecordStudent",
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
                     ( "arg",
                       `named
                         ( "ExRecordStudent",
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
                     ( "arg",
                       `named
                         ( "ExVariantIntList",
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
                                     ("value", `tuple_of [ `integral; `self ]);
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
                   `mandatory_field ("value", `tuple_of [ `integral; `self ]);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_nested_variant_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   let rec ex_record_gen__ex_record_student_to_json_nested =
     (fun { admission_year = x0; name = x1 } ->
        [ ("admissionYear", int_to_json x0); ("name", string_to_json x1) ]
       : Ex_record_gen.ex_record_student -> (string * Kxclib.Json.jv) list)
   and ex_variant_gen__ex_variant_int_list_to_json_nested =
     (function
      | IntNil -> [ ("kind", `str "intnil") ]
      | IntCons (x0, x1) ->
          [
            ("kind", `str "intcons");
            ( "value",
              `arr
                [
                  int_to_json x0;
                  `obj (ex_variant_gen__ex_variant_int_list_to_json_nested x1);
                ] );
          ]
       : Ex_variant_gen.ex_variant_int_list -> (string * Kxclib.Json.jv) list)
   in
   function
   | Student1 { student = x0 } ->
       `obj
         [
           ("tag", `str "student1");
           ("student", `obj (ex_record_gen__ex_record_student_to_json_nested x0));
         ]
   | Student2 { student = x0 } ->
       `obj
         (("tag", `str "student2")
         :: ex_record_gen__ex_record_student_to_json_nested x0)
   | Student3 x0 ->
       `obj
         [
           ("tag", `str "student3");
           ("arg", `obj (ex_record_gen__ex_record_student_to_json_nested x0));
         ]
   | Student4 x0 ->
       `obj
         (("tag", `str "student4")
         :: ex_record_gen__ex_record_student_to_json_nested x0)
   | Int_list1 x0 ->
       `obj
         [
           ("tag", `str "int-list1");
           ("arg", `obj (ex_variant_gen__ex_variant_int_list_to_json_nested x0));
         ]
   | Int_list2 x0 ->
       `obj
         (("tag", `str "int-list2")
         :: ex_variant_gen__ex_variant_int_list_to_json_nested x0)
    : ex_nested_variant -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_nested_variant_of_json' =
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
        let rec ex_record_gen__ex_record_student_of_json_nested path
            __bindoj_orig =
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
              Ok
                ({ admission_year = x0; name = x1 }
                  : Ex_record_gen.ex_record_student)
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a record value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and ex_variant_gen__ex_variant_int_list_of_json_nested path
            __bindoj_orig =
          match Kxclib.Jv.pump_field "kind" __bindoj_orig with
          | `obj (("kind", `str "intnil") :: _) ->
              Ok (IntNil : Ex_variant_gen.ex_variant_int_list)
          | `obj (("kind", `str "intcons") :: param) -> (
              match List.assoc_opt "value" param with
              | Some (`arr [ x0; x1 ]) ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`i 0 :: `f "value" :: path) x0 >>= fun x0 ->
                  ex_variant_gen__ex_variant_int_list_of_json_nested
                    (`i 1 :: `f "value" :: path)
                    x1
                  >>= fun x1 ->
                  Ok (IntCons (x0, x1) : Ex_variant_gen.ex_variant_int_list)
              | Some (`arr xs) ->
                  Error
                    ( Printf.sprintf
                        "expecting an array of length 2, but the given has a \
                         length of %d"
                        (List.length xs),
                      `f "value" :: path )
              | Some jv ->
                  Error
                    ( Printf.sprintf
                        "an array is expected for a tuple value, but the given \
                         is of type '%s'"
                        (let open Kxclib.Json in
                         string_of_jv_kind (classify_jv jv)),
                      `f "value" :: path )
              | None -> Error ("mandatory field 'value' does not exist", path))
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
              >>= ex_record_gen__ex_record_student_of_json_nested
                    (`f "student" :: path)
              >>= fun x0 -> Ok (Student1 { student = x0 })
          | `obj (("tag", `str "student2") :: _) ->
              let ( >>= ) = Result.bind in
              ex_record_gen__ex_record_student_of_json_nested path __bindoj_orig
              >>= fun x0 -> Ok (Student2 { student = x0 })
          | `obj (("tag", `str "student3") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  ex_record_gen__ex_record_student_of_json_nested
                    (`f "arg" :: path) arg
                  >>= fun x0 -> Ok (Student3 x0)
              | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("tag", `str "student4") :: _) ->
              let ( >>= ) = Result.bind in
              ex_record_gen__ex_record_student_of_json_nested path __bindoj_orig
              >>= fun x0 -> Ok (Student4 x0)
          | `obj (("tag", `str "int-list1") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  ex_variant_gen__ex_variant_int_list_of_json_nested
                    (`f "arg" :: path) arg
                  >>= fun x0 -> Ok (Int_list1 x0)
              | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("tag", `str "int-list2") :: _) ->
              let ( >>= ) = Result.bind in
              ex_variant_gen__ex_variant_int_list_of_json_nested path
                __bindoj_orig
              >>= fun x0 -> Ok (Int_list2 x0)
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
            (msg, path, ex_nested_variant_json_shape_explanation))
    : ex_nested_variant Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_nested_variant_of_json =
  (fun x -> ex_nested_variant_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_nested_variant option)
[@@warning "-39"]

let ex_nested_variant_decl =
  Bindoj_test_common_typedesc_examples.Ex_nested.Variant.decl

let ex_nested_variant_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_nested.Variant.decl
    ex_nested_variant_reflect
