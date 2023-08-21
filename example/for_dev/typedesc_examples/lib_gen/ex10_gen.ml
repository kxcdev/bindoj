type xy_opt = { x_opt : int option; y_opt : int option }

let rec (xy_opt_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun { x_opt; y_opt } ->
             StringMap.of_list
               [
                 ("x_opt", (Expr.of_option Expr.of_int) x_opt);
                 ("y_opt", (Expr.of_option Expr.of_int) y_opt);
               ]);
         mk =
           (fun xs ->
             xs |> StringMap.find_opt "x_opt" >>= Expr.to_option Expr.to_int
             >>= fun x_opt ->
             xs |> StringMap.find_opt "y_opt" >>= Expr.to_option Expr.to_int
             >>= fun y_opt -> Some { x_opt; y_opt });
       })
[@@warning "-33-39"]

let xy_opt_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "XyOpt",
           `object_of
             [
               `optional_field ("xOpt", `integral);
               `optional_field ("yOpt", `integral);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec xy_opt_to_json =
  (let option_to_json t_to_json = function
     | Some x -> t_to_json x
     | None -> (`null : Kxclib.Json.jv)
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   fun { x_opt = x0; y_opt = x1 } ->
     `obj
       [
         ("xOpt", (option_to_json int_to_json) x0);
         ("yOpt", (option_to_json int_to_json) x1);
       ]
    : xy_opt -> Kxclib.Json.jv)
[@@warning "-39"]

and xy_opt_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl =
        let option_of_json' t_of_json path = function
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
        in
        fun path __bindoj_orig ->
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
              >>= fun x1 -> Ok { x_opt = x0; y_opt = x1 }
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
            (msg, path, xy_opt_json_shape_explanation))
    : xy_opt Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and xy_opt_of_json =
  (fun x -> xy_opt_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> xy_opt option)
[@@warning "-39"]

let xy_opt_decl = Bindoj_test_common_typedesc_examples.Ex10.decl

let xy_opt_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex10.decl xy_opt_reflect
