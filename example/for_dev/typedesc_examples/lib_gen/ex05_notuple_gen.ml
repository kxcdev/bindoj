type complex_types = {
  option : int option;
  list : int list;
  map : (string * int) list;
}

let rec (complex_types_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { option; list; map } ->
             StringMap.of_list
               [
                 ("option", (Expr.of_option Expr.of_int) option);
                 ("list", (Expr.of_list Expr.of_int) list);
                 ("map", (Expr.of_map Expr.of_int) map);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "option" >>= Expr.to_option Expr.to_int
             >>= fun option ->
             xs |> StringMap.find_opt "list" >>= Expr.to_list Expr.to_int
             >>= fun list ->
             xs |> StringMap.find_opt "map" >>= Expr.to_map Expr.to_int
             >>= fun map -> Some { option; list; map });
       })
[@@warning "-33-39"]

let complex_types_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ComplexTypesNotuple",
           `object_of
             [
               `optional_field ("option", `integral);
               `mandatory_field ("list", `array_of `integral);
               `mandatory_field ("map", `record_of `integral);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec complex_types_to_json =
  (let option_to_json t_to_json = function
     | Some x -> t_to_json x
     | None -> (`null : Kxclib.Json.jv)
   and map_to_json key_to_string v_to_json fields =
     let fields =
       fields |> List.map (fun (k, v) -> (key_to_string k, v_to_json v))
     in
     (`obj fields : Kxclib.Json.jv)
   and list_to_json t_to_json xs : Kxclib.Json.jv = `arr (List.map t_to_json xs)
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun { option = x0; list = x1; map = x2 } ->
     `obj
       [
         ("option", (option_to_json int_to_json) x0);
         ("list", (list_to_json int_to_json) x1);
         ("map", (map_to_json (fun (k : string) -> k) int_to_json) x2);
       ]
    : complex_types -> Kxclib.Json.jv)
[@@warning "-39"]

and complex_types_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl =
        let option_of_json' t_of_json path = function
          | `null -> Ok None
          | x -> (
              match t_of_json path x with
              | Ok x -> Ok (Some x)
              | Error msg -> Error msg)
        and map_of_json' key_of_string v_of_json path = function
          | `obj fields ->
              let open Kxclib.MonadOps (Kxclib.ResultOf (struct
                type err = string * Kxclib.Json.jvpath
              end)) in
              fields
              |> List.map (fun (k, v) ->
                     match (key_of_string k, v_of_json (`f k :: path) v) with
                     | Some k, Ok v -> Ok (k, v)
                     | None, _ ->
                         Error
                           (Printf.sprintf "'key_of_string %s' failed" k, path)
                     | _, Error x -> Error x)
              |> sequence_list
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'map' but the given is of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
        and list_of_json' t_of_json path = function
          | (`arr xs : Kxclib.Json.jv) ->
              let open Kxclib.MonadOps (Kxclib.ResultOf (struct
                type err = string * Kxclib.Json.jvpath
              end)) in
              xs
              |> List.mapi (fun i -> t_of_json (`i i :: path))
              |> sequence_list
          | jv ->
              Error
                ( Printf.sprintf
                    "expecting type 'list' but the given is of type '%s'"
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
              List.assoc_opt "option" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "option" :: path)
              >>= fun x0 ->
              List.assoc_opt "list" param
              |> (function
                   | Some a -> Ok a
                   | None ->
                       Error ("mandatory field 'list' does not exist", path))
              >>= (list_of_json' int_of_json') (`f "list" :: path)
              >>= fun x1 ->
              List.assoc_opt "map" param
              |> (function
                   | Some a -> Ok a
                   | None -> Error ("mandatory field 'map' does not exist", path))
              >>= (map_of_json' (fun (s : string) -> Some s) int_of_json')
                    (`f "map" :: path)
              >>= fun x2 -> Ok { option = x0; list = x1; map = x2 }
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
            (msg, path, complex_types_json_shape_explanation))
    : complex_types Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and complex_types_of_json =
  (fun x -> complex_types_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> complex_types option)
[@@warning "-39"]

let complex_types_decl = Bindoj_test_common_typedesc_examples.Ex05_notuple.decl

let complex_types_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex05_notuple.decl complex_types_reflect
