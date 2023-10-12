type ex_nested_multiply_record = {
  nested_record : Ex_nested_gen.ex_nested_record;
  nested_record_spread : Ex_nested_gen.ex_nested_record;
}

let rec (ex_nested_multiply_record_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { nested_record; nested_record_spread } ->
             StringMap.of_list
               [
                 ( "nested_record",
                   (Expr.of_refl Ex_nested_gen.ex_nested_record_reflect)
                     nested_record );
                 ( "nested_record_spread",
                   (Expr.of_refl Ex_nested_gen.ex_nested_record_reflect)
                     nested_record_spread );
               ]);
         mk =
           (fun xs ->
             xs
             |> StringMap.find_opt "nested_record"
             >>= Expr.to_refl Ex_nested_gen.ex_nested_record_reflect
             >>= fun nested_record ->
             xs
             |> StringMap.find_opt "nested_record_spread"
             >>= Expr.to_refl Ex_nested_gen.ex_nested_record_reflect
             >>= fun nested_record_spread ->
             Some { nested_record; nested_record_spread });
       })
[@@warning "-33-39"]

let ex_nested_multiply_record_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExNestedMultiplyRecord",
           `anyone_of
             [
               `object_of
                 [
                   `mandatory_field
                     ( "nestedRecord",
                       `named
                         ( "ExNestedRecord",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
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
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
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
                   `mandatory_field ("kind", `exactly (`str "Anonymous"));
                 ];
               `object_of
                 [
                   `mandatory_field
                     ( "nestedRecord",
                       `named
                         ( "ExNestedRecord",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
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
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
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
                   `mandatory_field ("kind", `exactly (`str "With_id"));
                   `mandatory_field ("value", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field
                     ( "nestedRecord",
                       `named
                         ( "ExNestedRecord",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
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
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
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
                   `mandatory_field ("kind", `exactly (`str "student"));
                   `mandatory_field ("student_id", `integral);
                   `mandatory_field ("name", `string);
                   `mandatory_field
                     ("caseValue", `string_enum [ "Case_at0"; "case-at1" ]);
                 ];
               `object_of
                 [
                   `mandatory_field
                     ( "nestedRecord",
                       `named
                         ( "ExNestedRecord",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
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
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
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
                   `mandatory_field ("kind", `exactly (`str "Teacher"));
                   `mandatory_field ("facultyId", `integral);
                   `mandatory_field ("name", `string);
                   `mandatory_field ("department", `string);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_nested_multiply_record_to_json =
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
   and ex_nested_gen__ex_nested_point2_to_json_nested =
     (fun { x = x0; y = x1 } ->
        [ ("x", float_to_json x0); ("y", float_to_json x1) ]
       : Ex_nested_gen.ex_nested_point2 -> (string * Kxclib.Json.jv) list)
   and ex_nested_gen__ex_nested_record_to_json_nested =
     (fun {
            unit = x0;
            point2 = x1;
            point2_spread = x2;
            person = x3;
            person_spread = x4;
          } ->
        [
          ("unit", unit_to_json x0);
          ("point2", `obj (ex_nested_gen__ex_nested_point2_to_json_nested x1));
        ]
        @ ex_nested_gen__ex_nested_point2_to_json_nested x2
        @ [
            ( "person",
              `obj
                (ex_mangling_gen__ex_mangling_person_inherited_to_json_nested x3)
            );
          ]
        @ ex_mangling_gen__ex_mangling_person_inherited_to_json_nested x4
       : Ex_nested_gen.ex_nested_record -> (string * Kxclib.Json.jv) list)
   in
   fun { nested_record = x0; nested_record_spread = x1 } ->
     `obj
       ([
          ( "nestedRecord",
            `obj (ex_nested_gen__ex_nested_record_to_json_nested x0) );
        ]
       @ ex_nested_gen__ex_nested_record_to_json_nested x1)
    : ex_nested_multiply_record -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_nested_multiply_record_of_json' =
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
        and ex_nested_gen__ex_nested_point2_of_json_nested path __bindoj_orig =
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
              >>= fun x1 ->
              Ok ({ x = x0; y = x1 } : Ex_nested_gen.ex_nested_point2)
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a record value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and ex_nested_gen__ex_nested_record_of_json_nested path __bindoj_orig =
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
              >>= ex_nested_gen__ex_nested_point2_of_json_nested
                    (`f "point2" :: path)
              >>= fun x1 ->
              ex_nested_gen__ex_nested_point2_of_json_nested path __bindoj_orig
              >>= fun x2 ->
              List.assoc_opt "person" param
              |> Option.to_result
                   ~none:("mandatory field 'person' does not exist", path)
              >>= ex_mangling_gen__ex_mangling_person_inherited_of_json_nested
                    (`f "person" :: path)
              >>= fun x3 ->
              ex_mangling_gen__ex_mangling_person_inherited_of_json_nested path
                __bindoj_orig
              >>= fun x4 ->
              Ok
                ({
                   unit = x0;
                   point2 = x1;
                   point2_spread = x2;
                   person = x3;
                   person_spread = x4;
                 }
                  : Ex_nested_gen.ex_nested_record)
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
              List.assoc_opt "nestedRecord" param
              |> Option.to_result
                   ~none:("mandatory field 'nestedRecord' does not exist", path)
              >>= ex_nested_gen__ex_nested_record_of_json_nested
                    (`f "nestedRecord" :: path)
              >>= fun x0 ->
              ex_nested_gen__ex_nested_record_of_json_nested path __bindoj_orig
              >>= fun x1 -> Ok { nested_record = x0; nested_record_spread = x1 }
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
            (msg, path, ex_nested_multiply_record_json_shape_explanation))
    : ex_nested_multiply_record Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_nested_multiply_record_of_json =
  (fun x -> ex_nested_multiply_record_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_nested_multiply_record option)
[@@warning "-39"]

let ex_nested_multiply_record_decl =
  Bindoj_test_common_typedesc_examples.Ex_nested_multiply.Record.decl

let ex_nested_multiply_record_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_nested_multiply.Record.decl
    ex_nested_multiply_record_reflect

type ex_nested_multiply_variant =
  | Nested_record of {
      nested_record : Ex_nested_gen.ex_nested_record;
      nested_record_spread : Ex_nested_gen.ex_nested_record;
    }
  | Nested_variant of {
      nested_variant : Ex_nested_gen.ex_nested_variant;
      nested_variant_spread : Ex_nested_gen.ex_nested_variant;
    }

let rec (ex_nested_multiply_variant_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_Nested_record =
       Refl.InlineRecord
         {
           get =
             (function
             | Nested_record { nested_record; nested_record_spread } ->
                 StringMap.of_list
                   [
                     ( "nested_record",
                       (Expr.of_refl Ex_nested_gen.ex_nested_record_reflect)
                         nested_record );
                     ( "nested_record_spread",
                       (Expr.of_refl Ex_nested_gen.ex_nested_record_reflect)
                         nested_record_spread );
                   ]
             | _ -> invalid_arg "Nested_record is expected");
           mk =
             (fun xs ->
               xs
               |> StringMap.find_opt "nested_record"
               >>= Expr.to_refl Ex_nested_gen.ex_nested_record_reflect
               >>= fun nested_record ->
               xs
               |> StringMap.find_opt "nested_record_spread"
               >>= Expr.to_refl Ex_nested_gen.ex_nested_record_reflect
               >>= fun nested_record_spread ->
               Some (Nested_record { nested_record; nested_record_spread }));
         }
     in
     let ctor_Nested_variant =
       Refl.InlineRecord
         {
           get =
             (function
             | Nested_variant { nested_variant; nested_variant_spread } ->
                 StringMap.of_list
                   [
                     ( "nested_variant",
                       (Expr.of_refl Ex_nested_gen.ex_nested_variant_reflect)
                         nested_variant );
                     ( "nested_variant_spread",
                       (Expr.of_refl Ex_nested_gen.ex_nested_variant_reflect)
                         nested_variant_spread );
                   ]
             | _ -> invalid_arg "Nested_variant is expected");
           mk =
             (fun xs ->
               xs
               |> StringMap.find_opt "nested_variant"
               >>= Expr.to_refl Ex_nested_gen.ex_nested_variant_reflect
               >>= fun nested_variant ->
               xs
               |> StringMap.find_opt "nested_variant_spread"
               >>= Expr.to_refl Ex_nested_gen.ex_nested_variant_reflect
               >>= fun nested_variant_spread ->
               Some (Nested_variant { nested_variant; nested_variant_spread }));
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list
             [
               ("Nested_record", ctor_Nested_record);
               ("Nested_variant", ctor_Nested_variant);
             ];
         classify =
           (function
           | Nested_record _ -> ("Nested_record", ctor_Nested_record)
           | Nested_variant _ -> ("Nested_variant", ctor_Nested_variant));
       })
[@@warning "-33-39"]

let ex_nested_multiply_variant_json_discriminator_value =
  (function
   | Nested_record _ -> "nested-record" | Nested_variant _ -> "nested-variant"
    : ex_nested_multiply_variant -> string)
[@@warning "-39"]

let ex_nested_multiply_variant_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExNestedMultiplyVariant",
           `anyone_of
             [
               `object_of
                 [
                   `mandatory_field ("label", `exactly (`str "nested-record"));
                   `mandatory_field
                     ( "nestedRecord",
                       `named
                         ( "ExNestedRecord",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
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
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
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
                   `mandatory_field ("kind", `exactly (`str "Anonymous"));
                 ];
               `object_of
                 [
                   `mandatory_field ("label", `exactly (`str "nested-record"));
                   `mandatory_field
                     ( "nestedRecord",
                       `named
                         ( "ExNestedRecord",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
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
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
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
                   `mandatory_field ("kind", `exactly (`str "With_id"));
                   `mandatory_field ("value", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field ("label", `exactly (`str "nested-record"));
                   `mandatory_field
                     ( "nestedRecord",
                       `named
                         ( "ExNestedRecord",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
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
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
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
                   `mandatory_field ("kind", `exactly (`str "student"));
                   `mandatory_field ("student_id", `integral);
                   `mandatory_field ("name", `string);
                   `mandatory_field
                     ("caseValue", `string_enum [ "Case_at0"; "case-at1" ]);
                 ];
               `object_of
                 [
                   `mandatory_field ("label", `exactly (`str "nested-record"));
                   `mandatory_field
                     ( "nestedRecord",
                       `named
                         ( "ExNestedRecord",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Anonymous"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "With_id"));
                                   `mandatory_field ("value", `integral);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
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
                                     ( "unit",
                                       `named
                                         ( "ExAliasUnit",
                                           `special ("unit", `exactly `null) )
                                     );
                                   `mandatory_field
                                     ( "point2",
                                       `named
                                         ( "ExNestedPoint2",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("x", `proper_float);
                                               `mandatory_field
                                                 ("y", `proper_float);
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
                                                     ( "kind",
                                                       `exactly
                                                         (`str "Anonymous") );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "With_id")
                                                     );
                                                   `mandatory_field
                                                     ("value", `integral);
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "student")
                                                     );
                                                   `mandatory_field
                                                     ("student_id", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ( "caseValue",
                                                       `string_enum
                                                         [
                                                           "Case_at0";
                                                           "case-at1";
                                                         ] );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "Teacher")
                                                     );
                                                   `mandatory_field
                                                     ("facultyId", `integral);
                                                   `mandatory_field
                                                     ("name", `string);
                                                   `mandatory_field
                                                     ("department", `string);
                                                 ];
                                             ] ) );
                                   `mandatory_field
                                     ("kind", `exactly (`str "Teacher"));
                                   `mandatory_field ("facultyId", `integral);
                                   `mandatory_field ("name", `string);
                                   `mandatory_field ("department", `string);
                                 ];
                             ] ) );
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
                   `mandatory_field ("kind", `exactly (`str "Teacher"));
                   `mandatory_field ("facultyId", `integral);
                   `mandatory_field ("name", `string);
                   `mandatory_field ("department", `string);
                 ];
               `object_of
                 [
                   `mandatory_field ("label", `exactly (`str "nested-variant"));
                   `mandatory_field
                     ( "nestedVariant",
                       `named
                         ( "ExNestedVariant",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student1"));
                                   `mandatory_field
                                     ( "student",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student2"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student3"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student4"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list1"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExVariantIntList",
                                           `anyone_of
                                             [
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intnil")
                                                     );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intcons")
                                                     );
                                                   `mandatory_field
                                                     ( "value",
                                                       `tuple_of
                                                         [ `integral; `self ] );
                                                 ];
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intnil"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intcons"));
                                   `mandatory_field
                                     ("value", `tuple_of [ `integral; `self ]);
                                 ];
                             ] ) );
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
                   `mandatory_field ("label", `exactly (`str "nested-variant"));
                   `mandatory_field
                     ( "nestedVariant",
                       `named
                         ( "ExNestedVariant",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student1"));
                                   `mandatory_field
                                     ( "student",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student2"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student3"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student4"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list1"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExVariantIntList",
                                           `anyone_of
                                             [
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intnil")
                                                     );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intcons")
                                                     );
                                                   `mandatory_field
                                                     ( "value",
                                                       `tuple_of
                                                         [ `integral; `self ] );
                                                 ];
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intnil"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intcons"));
                                   `mandatory_field
                                     ("value", `tuple_of [ `integral; `self ]);
                                 ];
                             ] ) );
                   `mandatory_field ("tag", `exactly (`str "student2"));
                   `mandatory_field ("admissionYear", `integral);
                   `mandatory_field ("name", `string);
                 ];
               `object_of
                 [
                   `mandatory_field ("label", `exactly (`str "nested-variant"));
                   `mandatory_field
                     ( "nestedVariant",
                       `named
                         ( "ExNestedVariant",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student1"));
                                   `mandatory_field
                                     ( "student",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student2"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student3"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student4"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list1"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExVariantIntList",
                                           `anyone_of
                                             [
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intnil")
                                                     );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intcons")
                                                     );
                                                   `mandatory_field
                                                     ( "value",
                                                       `tuple_of
                                                         [ `integral; `self ] );
                                                 ];
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intnil"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intcons"));
                                   `mandatory_field
                                     ("value", `tuple_of [ `integral; `self ]);
                                 ];
                             ] ) );
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
                   `mandatory_field ("label", `exactly (`str "nested-variant"));
                   `mandatory_field
                     ( "nestedVariant",
                       `named
                         ( "ExNestedVariant",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student1"));
                                   `mandatory_field
                                     ( "student",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student2"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student3"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student4"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list1"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExVariantIntList",
                                           `anyone_of
                                             [
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intnil")
                                                     );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intcons")
                                                     );
                                                   `mandatory_field
                                                     ( "value",
                                                       `tuple_of
                                                         [ `integral; `self ] );
                                                 ];
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intnil"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intcons"));
                                   `mandatory_field
                                     ("value", `tuple_of [ `integral; `self ]);
                                 ];
                             ] ) );
                   `mandatory_field ("tag", `exactly (`str "student4"));
                   `mandatory_field ("admissionYear", `integral);
                   `mandatory_field ("name", `string);
                 ];
               `object_of
                 [
                   `mandatory_field ("label", `exactly (`str "nested-variant"));
                   `mandatory_field
                     ( "nestedVariant",
                       `named
                         ( "ExNestedVariant",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student1"));
                                   `mandatory_field
                                     ( "student",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student2"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student3"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student4"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list1"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExVariantIntList",
                                           `anyone_of
                                             [
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intnil")
                                                     );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intcons")
                                                     );
                                                   `mandatory_field
                                                     ( "value",
                                                       `tuple_of
                                                         [ `integral; `self ] );
                                                 ];
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intnil"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intcons"));
                                   `mandatory_field
                                     ("value", `tuple_of [ `integral; `self ]);
                                 ];
                             ] ) );
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
                   `mandatory_field ("label", `exactly (`str "nested-variant"));
                   `mandatory_field
                     ( "nestedVariant",
                       `named
                         ( "ExNestedVariant",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student1"));
                                   `mandatory_field
                                     ( "student",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student2"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student3"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student4"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list1"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExVariantIntList",
                                           `anyone_of
                                             [
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intnil")
                                                     );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intcons")
                                                     );
                                                   `mandatory_field
                                                     ( "value",
                                                       `tuple_of
                                                         [ `integral; `self ] );
                                                 ];
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intnil"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intcons"));
                                   `mandatory_field
                                     ("value", `tuple_of [ `integral; `self ]);
                                 ];
                             ] ) );
                   `mandatory_field ("tag", `exactly (`str "int-list2"));
                   `mandatory_field ("kind", `exactly (`str "intnil"));
                 ];
               `object_of
                 [
                   `mandatory_field ("label", `exactly (`str "nested-variant"));
                   `mandatory_field
                     ( "nestedVariant",
                       `named
                         ( "ExNestedVariant",
                           `anyone_of
                             [
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student1"));
                                   `mandatory_field
                                     ( "student",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student2"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student3"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExRecordStudent",
                                           `object_of
                                             [
                                               `mandatory_field
                                                 ("admissionYear", `integral);
                                               `mandatory_field ("name", `string);
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "student4"));
                                   `mandatory_field ("admissionYear", `integral);
                                   `mandatory_field ("name", `string);
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list1"));
                                   `mandatory_field
                                     ( "arg",
                                       `named
                                         ( "ExVariantIntList",
                                           `anyone_of
                                             [
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intnil")
                                                     );
                                                 ];
                                               `object_of
                                                 [
                                                   `mandatory_field
                                                     ( "kind",
                                                       `exactly (`str "intcons")
                                                     );
                                                   `mandatory_field
                                                     ( "value",
                                                       `tuple_of
                                                         [ `integral; `self ] );
                                                 ];
                                             ] ) );
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intnil"));
                                 ];
                               `object_of
                                 [
                                   `mandatory_field
                                     ("tag", `exactly (`str "int-list2"));
                                   `mandatory_field
                                     ("kind", `exactly (`str "intcons"));
                                   `mandatory_field
                                     ("value", `tuple_of [ `integral; `self ]);
                                 ];
                             ] ) );
                   `mandatory_field ("tag", `exactly (`str "int-list2"));
                   `mandatory_field ("kind", `exactly (`str "intcons"));
                   `mandatory_field ("value", `tuple_of [ `integral; `self ]);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_nested_multiply_variant_to_json =
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
   and ex_nested_gen__ex_nested_point2_to_json_nested =
     (fun { x = x0; y = x1 } ->
        [ ("x", float_to_json x0); ("y", float_to_json x1) ]
       : Ex_nested_gen.ex_nested_point2 -> (string * Kxclib.Json.jv) list)
   and ex_nested_gen__ex_nested_record_to_json_nested =
     (fun {
            unit = x0;
            point2 = x1;
            point2_spread = x2;
            person = x3;
            person_spread = x4;
          } ->
        [
          ("unit", unit_to_json x0);
          ("point2", `obj (ex_nested_gen__ex_nested_point2_to_json_nested x1));
        ]
        @ ex_nested_gen__ex_nested_point2_to_json_nested x2
        @ [
            ( "person",
              `obj
                (ex_mangling_gen__ex_mangling_person_inherited_to_json_nested x3)
            );
          ]
        @ ex_mangling_gen__ex_mangling_person_inherited_to_json_nested x4
       : Ex_nested_gen.ex_nested_record -> (string * Kxclib.Json.jv) list)
   and ex_nested_gen__ex_nested_variant_to_json_nested =
     (function
      | Student1 { student = x0 } ->
          [
            ("tag", `str "student1");
            ( "student",
              `obj (ex_record_gen__ex_record_student_to_json_nested x0) );
          ]
      | Student2 { student = x0 } ->
          ("tag", `str "student2")
          :: ex_record_gen__ex_record_student_to_json_nested x0
      | Student3 x0 ->
          [
            ("tag", `str "student3");
            ("arg", `obj (ex_record_gen__ex_record_student_to_json_nested x0));
          ]
      | Student4 x0 ->
          ("tag", `str "student4")
          :: ex_record_gen__ex_record_student_to_json_nested x0
      | Int_list1 x0 ->
          [
            ("tag", `str "int-list1");
            ("arg", `obj (ex_variant_gen__ex_variant_int_list_to_json_nested x0));
          ]
      | Int_list2 x0 ->
          ("tag", `str "int-list2")
          :: ex_variant_gen__ex_variant_int_list_to_json_nested x0
       : Ex_nested_gen.ex_nested_variant -> (string * Kxclib.Json.jv) list)
   and ex_record_gen__ex_record_student_to_json_nested =
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
   | Nested_record { nested_record = x0; nested_record_spread = x1 } ->
       `obj
         (("label", `str "nested-record")
         :: ([
               ( "nestedRecord",
                 `obj (ex_nested_gen__ex_nested_record_to_json_nested x0) );
             ]
            @ ex_nested_gen__ex_nested_record_to_json_nested x1))
   | Nested_variant { nested_variant = x0; nested_variant_spread = x1 } ->
       `obj
         (("label", `str "nested-variant")
         :: ([
               ( "nestedVariant",
                 `obj (ex_nested_gen__ex_nested_variant_to_json_nested x0) );
             ]
            @ ex_nested_gen__ex_nested_variant_to_json_nested x1))
    : ex_nested_multiply_variant -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_nested_multiply_variant_of_json' =
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
        and ex_nested_gen__ex_nested_point2_of_json_nested path __bindoj_orig =
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
              >>= fun x1 ->
              Ok ({ x = x0; y = x1 } : Ex_nested_gen.ex_nested_point2)
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a record value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and ex_nested_gen__ex_nested_record_of_json_nested path __bindoj_orig =
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
              >>= ex_nested_gen__ex_nested_point2_of_json_nested
                    (`f "point2" :: path)
              >>= fun x1 ->
              ex_nested_gen__ex_nested_point2_of_json_nested path __bindoj_orig
              >>= fun x2 ->
              List.assoc_opt "person" param
              |> Option.to_result
                   ~none:("mandatory field 'person' does not exist", path)
              >>= ex_mangling_gen__ex_mangling_person_inherited_of_json_nested
                    (`f "person" :: path)
              >>= fun x3 ->
              ex_mangling_gen__ex_mangling_person_inherited_of_json_nested path
                __bindoj_orig
              >>= fun x4 ->
              Ok
                ({
                   unit = x0;
                   point2 = x1;
                   point2_spread = x2;
                   person = x3;
                   person_spread = x4;
                 }
                  : Ex_nested_gen.ex_nested_record)
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a record value, but the given \
                     is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and ex_nested_gen__ex_nested_variant_of_json_nested path __bindoj_orig =
          match Kxclib.Jv.pump_field "tag" __bindoj_orig with
          | `obj (("tag", `str "student1") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "student" param
              |> Option.to_result
                   ~none:("mandatory field 'student' does not exist", path)
              >>= ex_record_gen__ex_record_student_of_json_nested
                    (`f "student" :: path)
              >>= fun x0 ->
              Ok (Student1 { student = x0 } : Ex_nested_gen.ex_nested_variant)
          | `obj (("tag", `str "student2") :: _) ->
              let ( >>= ) = Result.bind in
              ex_record_gen__ex_record_student_of_json_nested path __bindoj_orig
              >>= fun x0 ->
              Ok (Student2 { student = x0 } : Ex_nested_gen.ex_nested_variant)
          | `obj (("tag", `str "student3") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  ex_record_gen__ex_record_student_of_json_nested
                    (`f "arg" :: path) arg
                  >>= fun x0 ->
                  Ok (Student3 x0 : Ex_nested_gen.ex_nested_variant)
              | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("tag", `str "student4") :: _) ->
              let ( >>= ) = Result.bind in
              ex_record_gen__ex_record_student_of_json_nested path __bindoj_orig
              >>= fun x0 -> Ok (Student4 x0 : Ex_nested_gen.ex_nested_variant)
          | `obj (("tag", `str "int-list1") :: param) -> (
              match List.assoc_opt "arg" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  ex_variant_gen__ex_variant_int_list_of_json_nested
                    (`f "arg" :: path) arg
                  >>= fun x0 ->
                  Ok (Int_list1 x0 : Ex_nested_gen.ex_nested_variant)
              | None -> Error ("mandatory field 'arg' does not exist", path))
          | `obj (("tag", `str "int-list2") :: _) ->
              let ( >>= ) = Result.bind in
              ex_variant_gen__ex_variant_int_list_of_json_nested path
                __bindoj_orig
              >>= fun x0 -> Ok (Int_list2 x0 : Ex_nested_gen.ex_nested_variant)
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
        and ex_record_gen__ex_record_student_of_json_nested path __bindoj_orig =
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
          match Kxclib.Jv.pump_field "label" __bindoj_orig with
          | `obj (("label", `str "nested-record") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "nestedRecord" param
              |> Option.to_result
                   ~none:("mandatory field 'nestedRecord' does not exist", path)
              >>= ex_nested_gen__ex_nested_record_of_json_nested
                    (`f "nestedRecord" :: path)
              >>= fun x0 ->
              ex_nested_gen__ex_nested_record_of_json_nested path __bindoj_orig
              >>= fun x1 ->
              Ok
                (Nested_record { nested_record = x0; nested_record_spread = x1 })
          | `obj (("label", `str "nested-variant") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "nestedVariant" param
              |> Option.to_result
                   ~none:("mandatory field 'nestedVariant' does not exist", path)
              >>= ex_nested_gen__ex_nested_variant_of_json_nested
                    (`f "nestedVariant" :: path)
              >>= fun x0 ->
              ex_nested_gen__ex_nested_variant_of_json_nested path __bindoj_orig
              >>= fun x1 ->
              Ok
                (Nested_variant
                   { nested_variant = x0; nested_variant_spread = x1 })
          | `obj (("label", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'nested-record', 'nested-variant' ]"
                    discriminator_value,
                  `f "label" :: path )
          | `obj (("label", jv) :: _) ->
              Error
                ( Printf.sprintf
                    "a string is expected for a variant discriminator, but the \
                     given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  `f "label" :: path )
          | `obj _ -> Error ("discriminator field 'label' does not exist", path)
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
            (msg, path, ex_nested_multiply_variant_json_shape_explanation))
    : ex_nested_multiply_variant Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_nested_multiply_variant_of_json =
  (fun x -> ex_nested_multiply_variant_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_nested_multiply_variant option)
[@@warning "-39"]

let ex_nested_multiply_variant_decl =
  Bindoj_test_common_typedesc_examples.Ex_nested_multiply.Variant.decl

let ex_nested_multiply_variant_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_nested_multiply.Variant.decl
    ex_nested_multiply_variant_reflect
