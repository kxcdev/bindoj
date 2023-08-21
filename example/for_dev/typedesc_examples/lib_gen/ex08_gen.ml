type named_json = { name : string; json : Bindoj_std_runtime.Json_value.t }

let rec (named_json_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { name; json } ->
             StringMap.of_list
               [
                 ("name", Expr.of_string name);
                 ( "json",
                   (Expr.of_refl Bindoj_std_runtime.Json_value.reflect) json );
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "name" >>= Expr.to_string >>= fun name ->
             xs |> StringMap.find_opt "json"
             >>= Expr.to_refl Bindoj_std_runtime.Json_value.reflect
             >>= fun json -> Some { name; json });
       })
[@@warning "-33-39"]

let named_json_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "NamedJson",
           `object_of
             [
               `mandatory_field ("name", `string);
               `mandatory_field ("json", `named ("json_value", `any_json_value));
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec named_json_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x in
   fun { name = x0; json = x1 } ->
     `obj
       [
         ("name", string_to_json x0);
         ("json", Bindoj_std_runtime.Json_value.to_json x1);
       ]
    : named_json -> Kxclib.Json.jv)
[@@warning "-39"]

and named_json_of_json' =
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
        in
        fun path __bindoj_orig ->
          match __bindoj_orig with
          | `obj param ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "name" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'name' does not exist", path))
              >>= string_of_json' (`f "name" :: path)
              >>= fun x0 ->
              List.assoc_opt "json" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'json' does not exist", path))
              >>= (fun path x ->
                    Bindoj_std_runtime.Json_value.of_json' ~path x
                    |> Result.map_error (fun (msg, path, _) -> (msg, path)))
                    (`f "json" :: path)
              >>= fun x1 -> Ok { name = x0; json = x1 }
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
            (msg, path, named_json_json_shape_explanation))
    : named_json Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and named_json_of_json =
  (fun x -> named_json_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> named_json option)
[@@warning "-39"]

let named_json_decl = Bindoj_test_common_typedesc_examples.Ex08.decl

let named_json_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex08.decl named_json_reflect
