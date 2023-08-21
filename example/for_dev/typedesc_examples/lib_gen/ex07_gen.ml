type customized_union = Case1 of int | Case2 of { x : int; y : int }

let rec (customized_union_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_Case1 =
       Refl.TupleLike
         {
           get =
             (function
             | Case1 x -> [ Expr.of_int x ]
             | _ -> invalid_arg "Case1 is expected");
           mk =
             (function
             | x :: [] -> Expr.to_int x |> Option.map (fun x -> Case1 x)
             | _ -> None);
         }
     in
     let ctor_Case2 =
       Refl.InlineRecord
         {
           get =
             (function
             | Case2 { x; y } ->
                 StringMap.of_list
                   [ ("x", Expr.of_int x); ("y", Expr.of_int y) ]
             | _ -> invalid_arg "Case2 is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "x" >>= Expr.to_int >>= fun x ->
               xs |> StringMap.find_opt "y" >>= Expr.to_int >>= fun y ->
               Some (Case2 { x; y }));
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list [ ("Case1", ctor_Case1); ("Case2", ctor_Case2) ];
         classify =
           (function
           | Case1 _ -> ("Case1", ctor_Case1) | Case2 _ -> ("Case2", ctor_Case2));
       })
[@@warning "-33-39"]

let customized_union_json_discriminator_value =
  (function Case1 _ -> "case1'" | Case2 _ -> "case2'"
    : customized_union -> string)
[@@warning "-39"]

let customized_union_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "CustomizedUnion",
           `anyone_of
             [
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "case1'"));
                   `mandatory_field ("value", `tuple_of [ `integral ]);
                 ];
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "case2'"));
                   `mandatory_field ("x'", `integral);
                   `mandatory_field ("y'", `integral);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec customized_union_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   function
   | Case1 x0 -> `obj [ ("tag", `str "case1'"); ("value", int_to_json x0) ]
   | Case2 { x = x0; y = x1 } ->
       `obj
         [
           ("tag", `str "case2'"); ("x'", int_to_json x0); ("y'", int_to_json x1);
         ]
    : customized_union -> Kxclib.Json.jv)
[@@warning "-39"]

and customized_union_of_json' =
  (fun ?(path = []) x ->
     (let rec of_json_impl =
        let int_of_json' path = function
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
          match Kxclib.Jv.pump_field "tag" __bindoj_orig with
          | `obj (("tag", `str "case1'") :: param) -> (
              match List.assoc_opt "value" param with
              | Some arg ->
                  let ( >>= ) = Result.bind in
                  int_of_json' (`f "value" :: path) arg >>= fun x0 ->
                  Ok (Case1 x0)
              | None -> Error ("mandatory field 'value' does not exist", path))
          | `obj (("tag", `str "case2'") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "x'" param
              |> (function
                   | Some a -> Ok a
                   | None -> Error ("mandatory field 'x'' does not exist", path))
              >>= int_of_json' (`f "x'" :: path)
              >>= fun x0 ->
              List.assoc_opt "y'" param
              |> (function
                   | Some a -> Ok a
                   | None -> Error ("mandatory field 'y'' does not exist", path))
              >>= int_of_json' (`f "y'" :: path)
              >>= fun x1 -> Ok (Case2 { x = x0; y = x1 })
          | `obj (("tag", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'case1'', 'case2'' ]"
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
            (msg, path, customized_union_json_shape_explanation))
    : customized_union Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and customized_union_of_json =
  (fun x -> customized_union_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> customized_union option)
[@@warning "-39"]

let customized_union_decl = Bindoj_test_common_typedesc_examples.Ex07.decl

let customized_union_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex07.decl customized_union_reflect
