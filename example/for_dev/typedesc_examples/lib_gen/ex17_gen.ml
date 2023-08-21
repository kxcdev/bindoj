type nonrec int_opt = int option

let (int_opt_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias
       { get = Expr.of_option Expr.of_int; mk = Expr.to_option Expr.to_int })
[@@warning "-33-39"]

let int_opt_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named ("IntOpt", `nullable `integral) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec int_opt_to_json =
  (let option_to_json t_to_json = function
     | Some x -> t_to_json x
     | None -> (`null : Kxclib.Json.jv)
   and int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   option_to_json int_to_json
    : int_opt -> Kxclib.Json.jv)
[@@warning "-39"]

and int_opt_of_json' =
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
        option_of_json' int_of_json'
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, int_opt_json_shape_explanation))
    : int_opt Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and int_opt_of_json =
  (fun x -> int_opt_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> int_opt option)
[@@warning "-39"]

let int_opt_decl = Bindoj_test_common_typedesc_examples.Ex11.decl

let int_opt_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex11.decl int_opt_reflect
