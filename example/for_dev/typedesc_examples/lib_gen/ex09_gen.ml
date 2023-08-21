type with_int53p = { value : Kxclib.int53p }

let rec (with_int53p_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { value } ->
             StringMap.of_list [ ("value", Expr.of_int53p value) ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "value" >>= Expr.to_int53p
             >>= fun value -> Some { value });
       })
[@@warning "-33-39"]

let with_int53p_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "WithInt53p",
           `object_of [ `mandatory_field ("value", `proper_int53p) ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec with_int53p_to_json =
  (let int53p_to_json (x : Kxclib.int53p) : Kxclib.Json.jv =
     `num (Kxclib.Int53p.to_float x)
   in
   fun { value = x0 } -> `obj [ ("value", int53p_to_json x0) ]
    : with_int53p -> Kxclib.Json.jv)
[@@warning "-39"]

and with_int53p_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl =
        let int53p_of_json' path = function
          | (`num x : Kxclib.Json.jv) -> Ok (Kxclib.Int53p.of_float x)
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'int53p' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        in
        fun path __bindoj_orig ->
          match __bindoj_orig with
          | `obj param ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "value" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'value' does not exist", path))
              >>= int53p_of_json' (`f "value" :: path)
              >>= fun x0 -> Ok { value = x0 }
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
            (msg, path, with_int53p_json_shape_explanation))
    : with_int53p Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and with_int53p_of_json =
  (fun x -> with_int53p_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> with_int53p option)
[@@warning "-39"]

let with_int53p_decl = Bindoj_test_common_typedesc_examples.Ex09.decl

let with_int53p_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex09.decl with_int53p_reflect
