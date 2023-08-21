type nonrec objtuple = float * string

let (objtuple_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias
       {
         get =
           (fun (x0, x1) -> Expr.Tuple [ Expr.of_float x0; Expr.of_string x1 ]);
         mk =
           (function
           | Expr.Tuple [ x0; x1 ] ->
               Expr.to_float x0 >>= fun x0 ->
               Expr.to_string x1 >>= fun x1 -> Some (x0, x1)
           | _ -> None);
       })
[@@warning "-33-39"]

let objtuple_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named ("Objtuple", `tuple_of [ `proper_float; `string ]) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec objtuple_to_json =
  (let string_to_json (x : string) : Kxclib.Json.jv = `str x
   and float_to_json (x : float) : Kxclib.Json.jv = `num x in
   fun (x0, x1) : Kxclib.Json.jv ->
     `obj [ ("_0", float_to_json x0); ("_1", string_to_json x1) ]
    : objtuple -> Kxclib.Json.jv)
[@@warning "-39"]

and objtuple_of_json' =
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
        fun path -> function
          | (`obj fields : Kxclib.Json.jv) ->
              let fields = Bindoj_runtime.StringMap.of_list fields in
              let ( >>= ) = Result.bind in
              (Bindoj_runtime.StringMap.find_opt "_0" fields |> function
               | Some a -> Ok a
               | None -> Error ("mandatory field '_0' does not exist", path))
              >>= fun x0 ->
              (Bindoj_runtime.StringMap.find_opt "_1" fields |> function
               | Some a -> Ok a
               | None -> Error ("mandatory field '_1' does not exist", path))
              >>= fun x1 ->
              let ( >>= ) = Result.bind in
              float_of_json' (`f "_0" :: path) x0 >>= fun x0 ->
              string_of_json' (`f "_1" :: path) x1 >>= fun x1 -> Ok (x0, x1)
          | jv ->
              Error
                ( Printf.sprintf
                    "an object is expected for a tuple value, but the given is \
                     of type '%s'"
                    (let open Kxclib.Json in
                     string_of_jv_kind (classify_jv jv)),
                  path )
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, objtuple_json_shape_explanation))
    : objtuple Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and objtuple_of_json =
  (fun x -> objtuple_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> objtuple option)
[@@warning "-39"]

let objtuple_decl = Bindoj_test_common_typedesc_examples.Ex14.decl

let objtuple_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex14.decl objtuple_reflect
