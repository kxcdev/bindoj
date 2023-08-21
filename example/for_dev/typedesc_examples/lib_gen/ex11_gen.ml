type nonrec unit = unit

let (unit_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Alias { get = Expr.of_unit; mk = Expr.to_unit })
[@@warning "-33-39"]

let unit_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named ("Unit", `special ("unit", `exactly `null)) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec unit_to_json =
  (let unit_to_json () : Kxclib.Json.jv = `num 1. in
   unit_to_json
    : unit -> Kxclib.Json.jv)
[@@warning "-39"]

and unit_of_json' =
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
        in
        unit_of_json'
      in
      of_json_impl)
       path x
     |> Result.map_error (fun (msg, path) ->
            (msg, path, unit_json_shape_explanation))
    : unit Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and unit_of_json =
  (fun x -> unit_of_json' x |> Result.to_option : Kxclib.Json.jv -> unit option)
[@@warning "-39"]

let unit_decl = Bindoj_test_common_typedesc_examples.Ex11.decl

let unit_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex11.decl unit_reflect
