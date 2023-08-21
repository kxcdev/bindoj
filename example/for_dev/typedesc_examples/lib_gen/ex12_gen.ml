type nonrec cases = [ `Case_at0 | `case_at1 | `Case_at2 | `Case_at3 ]

let (cases_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias
       {
         get =
           (function
           | `Case_at0 -> Expr.StringEnum "Case_at0"
           | `case_at1 -> Expr.StringEnum "case_at1"
           | `Case_at2 -> Expr.StringEnum "Case_at2"
           | `Case_at3 -> Expr.StringEnum "Case_at3");
         mk =
           (function
           | Expr.StringEnum "Case_at0" -> Some `Case_at0
           | Expr.StringEnum "case_at1" -> Some `case_at1
           | Expr.StringEnum "Case_at2" -> Some `Case_at2
           | Expr.StringEnum "Case_at3" -> Some `Case_at3
           | _ -> None);
       })
[@@warning "-33-39"]

let cases_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "Cases",
           `string_enum [ "Case_at0"; "case-at1"; "Case-at2"; "Case-third" ] )
     )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec cases_to_json =
  (function
   | `Case_at0 -> `str "Case_at0"
   | `case_at1 -> `str "case-at1"
   | `Case_at2 -> `str "Case-at2"
   | `Case_at3 -> `str "Case-third"
    : cases -> Kxclib.Json.jv)
[@@warning "-39"]

and cases_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl path = function
        | `str s ->
            (function
              | "Case_at0" -> Ok `Case_at0
              | "case-at1" -> Ok `case_at1
              | "Case-at2" -> Ok `Case_at2
              | "Case-third" -> Ok `Case_at3
              | s ->
                  Error
                    ( Printf.sprintf
                        "given string '%s' is not one of [ 'Case_at0', \
                         'case-at1', 'Case-at2', 'Case-third' ]"
                        s,
                      path ))
              s
        | jv ->
            Error
              ( Printf.sprintf
                  "expecting type 'string' but the given is of type '%s'"
                  (let open Kxclib.Json in
                   string_of_jv_kind (classify_jv jv)),
                path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, cases_json_shape_explanation))
    : cases Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and cases_of_json =
  (fun x -> cases_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> cases option)
[@@warning "-39"]

let cases_decl = Bindoj_test_common_typedesc_examples.Ex12.decl

let cases_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex12.decl cases_reflect
