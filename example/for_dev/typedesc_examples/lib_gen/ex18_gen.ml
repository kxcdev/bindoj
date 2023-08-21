type optional_variant =
  | Tuple_like of int option
  | Tuple_like_alias of Ex17_gen.int_opt
  | Tuple_like_obj of int option * Ex17_gen.int_opt
  | Tuple_like_spreading of Ex10_gen.xy_opt
  | Inline_record of {
      int_opt : Ex17_gen.int_opt;
      x_opt : int option;
      y_opt : int option;
      objtuple : int option * int option;
    }
  | Inline_record_spreading of {
      int_opt : Ex17_gen.int_opt;
      xy_opt : Ex10_gen.xy_opt;
    }
  | Reused_inline_record of { x_opt : int option; y_opt : int option }

let rec (optional_variant_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_Tuple_like =
       Refl.TupleLike
         {
           get =
             (function
             | Tuple_like x -> [ (Expr.of_option Expr.of_int) x ]
             | _ -> invalid_arg "Tuple_like is expected");
           mk =
             (function
             | x :: [] ->
                 (Expr.to_option Expr.to_int) x
                 |> Option.map (fun x -> Tuple_like x)
             | _ -> None);
         }
     in
     let ctor_Tuple_like_alias =
       Refl.TupleLike
         {
           get =
             (function
             | Tuple_like_alias x -> [ (Expr.of_option Expr.of_int) x ]
             | _ -> invalid_arg "Tuple_like_alias is expected");
           mk =
             (function
             | x :: [] ->
                 (Expr.to_option Expr.to_int) x
                 |> Option.map (fun x -> Tuple_like_alias x)
             | _ -> None);
         }
     in
     let ctor_Tuple_like_obj =
       Refl.TupleLike
         {
           get =
             (function
             | Tuple_like_obj (x0, x1) ->
                 [
                   (Expr.of_option Expr.of_int) x0;
                   (Expr.of_option Expr.of_int) x1;
                 ]
             | _ -> invalid_arg "Tuple_like_obj is expected");
           mk =
             (function
             | [ x0; x1 ] ->
                 (Expr.to_option Expr.to_int) x0 >>= fun x0 ->
                 (Expr.to_option Expr.to_int) x1 >>= fun x1 ->
                 Some (Tuple_like_obj (x0, x1))
             | _ -> None);
         }
     in
     let ctor_Tuple_like_spreading =
       Refl.TupleLike
         {
           get =
             (function
             | Tuple_like_spreading x ->
                 [ (Expr.of_refl Ex10_gen.xy_opt_reflect) x ]
             | _ -> invalid_arg "Tuple_like_spreading is expected");
           mk =
             (function
             | x :: [] ->
                 (Expr.to_refl Ex10_gen.xy_opt_reflect) x
                 |> Option.map (fun x -> Tuple_like_spreading x)
             | _ -> None);
         }
     in
     let ctor_Inline_record =
       Refl.InlineRecord
         {
           get =
             (function
             | Inline_record { int_opt; x_opt; y_opt; objtuple } ->
                 StringMap.of_list
                   [
                     ("int_opt", (Expr.of_option Expr.of_int) int_opt);
                     ("x_opt", (Expr.of_option Expr.of_int) x_opt);
                     ("y_opt", (Expr.of_option Expr.of_int) y_opt);
                     ( "objtuple",
                       (fun (x0, x1) ->
                         Expr.Tuple
                           [
                             (Expr.of_option Expr.of_int) x0;
                             (Expr.of_option Expr.of_int) x1;
                           ])
                         objtuple );
                   ]
             | _ -> invalid_arg "Inline_record is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "int_opt" >>= Expr.to_option Expr.to_int
               >>= fun int_opt ->
               xs |> StringMap.find_opt "x_opt" >>= Expr.to_option Expr.to_int
               >>= fun x_opt ->
               xs |> StringMap.find_opt "y_opt" >>= Expr.to_option Expr.to_int
               >>= fun y_opt ->
               (xs |> StringMap.find_opt "objtuple" >>= function
                | Expr.Tuple [ x0; x1 ] ->
                    (Expr.to_option Expr.to_int) x0 >>= fun x0 ->
                    (Expr.to_option Expr.to_int) x1 >>= fun x1 -> Some (x0, x1)
                | _ -> None)
               >>= fun objtuple ->
               Some (Inline_record { int_opt; x_opt; y_opt; objtuple }));
         }
     in
     let ctor_Inline_record_spreading =
       Refl.InlineRecord
         {
           get =
             (function
             | Inline_record_spreading { int_opt; xy_opt } ->
                 StringMap.of_list
                   [
                     ("int_opt", (Expr.of_option Expr.of_int) int_opt);
                     ("xy_opt", (Expr.of_refl Ex10_gen.xy_opt_reflect) xy_opt);
                   ]
             | _ -> invalid_arg "Inline_record_spreading is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "int_opt" >>= Expr.to_option Expr.to_int
               >>= fun int_opt ->
               xs
               |> StringMap.find_opt "xy_opt"
               >>= Expr.to_refl Ex10_gen.xy_opt_reflect
               >>= fun xy_opt ->
               Some (Inline_record_spreading { int_opt; xy_opt }));
         }
     in
     let ctor_Reused_inline_record =
       Refl.ReusedInlineRecord
         {
           get =
             (function
             | Reused_inline_record { x_opt; y_opt } ->
                 StringMap.of_list
                   [
                     ("x_opt", (Expr.of_option Expr.of_int) x_opt);
                     ("y_opt", (Expr.of_option Expr.of_int) y_opt);
                   ]
             | _ -> invalid_arg "Reused_inline_record is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "x_opt" >>= Expr.to_option Expr.to_int
               >>= fun x_opt ->
               xs |> StringMap.find_opt "y_opt" >>= Expr.to_option Expr.to_int
               >>= fun y_opt -> Some (Reused_inline_record { x_opt; y_opt }));
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list
             [
               ("Tuple_like", ctor_Tuple_like);
               ("Tuple_like_alias", ctor_Tuple_like_alias);
               ("Tuple_like_obj", ctor_Tuple_like_obj);
               ("Tuple_like_spreading", ctor_Tuple_like_spreading);
               ("Inline_record", ctor_Inline_record);
               ("Inline_record_spreading", ctor_Inline_record_spreading);
               ("Reused_inline_record", ctor_Reused_inline_record);
             ];
         classify =
           (function
           | Tuple_like _ -> ("Tuple_like", ctor_Tuple_like)
           | Tuple_like_alias _ -> ("Tuple_like_alias", ctor_Tuple_like_alias)
           | Tuple_like_obj _ -> ("Tuple_like_obj", ctor_Tuple_like_obj)
           | Tuple_like_spreading _ ->
               ("Tuple_like_spreading", ctor_Tuple_like_spreading)
           | Inline_record _ -> ("Inline_record", ctor_Inline_record)
           | Inline_record_spreading _ ->
               ("Inline_record_spreading", ctor_Inline_record_spreading)
           | Reused_inline_record _ ->
               ("Reused_inline_record", ctor_Reused_inline_record));
       })
[@@warning "-33-39"]

let optional_variant_json_discriminator_value =
  (function
   | Tuple_like _ -> "tuple-like"
   | Tuple_like_alias _ -> "tuple-like-alias"
   | Tuple_like_obj _ -> "tuple-like-obj"
   | Tuple_like_spreading _ -> "tuple-like-spreading"
   | Inline_record _ -> "inline-record"
   | Inline_record_spreading _ -> "inline-record-spreading"
   | Reused_inline_record _ -> "reused-inline-record"
    : optional_variant -> string)
[@@warning "-39"]

let optional_variant_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "OptionalVariant",
           `anyone_of
             [
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "tuple-like"));
                   `optional_field ("value", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "tuple-like-alias"));
                   `optional_field ("value", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "tuple-like-obj"));
                   `optional_field ("_0", `integral);
                   `optional_field ("_1", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field
                     ("tag", `exactly (`str "tuple-like-spreading"));
                   `optional_field ("xOpt", `integral);
                   `optional_field ("yOpt", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field ("tag", `exactly (`str "inline-record"));
                   `optional_field ("intOpt", `integral);
                   `optional_field ("xOpt", `integral);
                   `optional_field ("yOpt", `integral);
                   `mandatory_field
                     ( "objtuple",
                       `object_of
                         [
                           `optional_field ("_0", `integral);
                           `optional_field ("_1", `integral);
                         ] );
                 ];
               `object_of
                 [
                   `mandatory_field
                     ("tag", `exactly (`str "inline-record-spreading"));
                   `optional_field ("intOpt", `integral);
                   `optional_field ("xOpt", `integral);
                   `optional_field ("yOpt", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field
                     ("tag", `exactly (`str "reused-inline-record"));
                   `optional_field ("xOpt", `integral);
                   `optional_field ("yOpt", `integral);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec optional_variant_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
   let rec ex10_gen__xy_opt_to_json_nested =
     (fun { x_opt = x0; y_opt = x1 } ->
        List.filter_map
          (fun x -> x)
          [
            Option.map (fun x0 -> ("xOpt", int_to_json x0)) x0;
            Option.map (fun x1 -> ("yOpt", int_to_json x1)) x1;
          ]
       : Ex10_gen.xy_opt -> (string * Kxclib.Json.jv) list)
   in
   function
   | Tuple_like None -> `obj [ ("tag", `str "tuple-like") ]
   | Tuple_like (Some x0) ->
       `obj [ ("tag", `str "tuple-like"); ("value", int_to_json x0) ]
   | Tuple_like_alias None -> `obj [ ("tag", `str "tuple-like-alias") ]
   | Tuple_like_alias (Some x0) ->
       `obj [ ("tag", `str "tuple-like-alias"); ("value", int_to_json x0) ]
   | Tuple_like_obj (x0, x1) ->
       `obj
         (("tag", `str "tuple-like-obj")
         :: List.filter_map Kxclib.identity
              [
                Option.map (fun x0 -> ("_0", int_to_json x0)) x0;
                Option.map (fun x1 -> ("_1", int_to_json x1)) x1;
              ])
   | Tuple_like_spreading x0 ->
       `obj
         (("tag", `str "tuple-like-spreading")
         :: ex10_gen__xy_opt_to_json_nested x0)
   | Inline_record { int_opt = x0; x_opt = x1; y_opt = x2; objtuple = x3 } ->
       `obj
         (("tag", `str "inline-record")
         :: (List.filter_map
               (fun x -> x)
               [
                 Option.map (fun x0 -> ("intOpt", int_to_json x0)) x0;
                 Option.map (fun x1 -> ("xOpt", int_to_json x1)) x1;
                 Option.map (fun x2 -> ("yOpt", int_to_json x2)) x2;
               ]
            @ [
                ( "objtuple",
                  (fun (x0, x1) : Kxclib.Json.jv ->
                    `obj
                      (List.filter_map Kxclib.identity
                         [
                           Option.map (fun x0 -> ("_0", int_to_json x0)) x0;
                           Option.map (fun x1 -> ("_1", int_to_json x1)) x1;
                         ]))
                    x3 );
              ]))
   | Inline_record_spreading { int_opt = x0; xy_opt = x1 } ->
       `obj
         (("tag", `str "inline-record-spreading")
         :: (List.filter_map
               (fun x -> x)
               [ Option.map (fun x0 -> ("intOpt", int_to_json x0)) x0 ]
            @ ex10_gen__xy_opt_to_json_nested x1))
   | Reused_inline_record { x_opt = x0; y_opt = x1 } ->
       `obj
         (("tag", `str "reused-inline-record")
         :: List.filter_map
              (fun x -> x)
              [
                Option.map (fun x0 -> ("xOpt", int_to_json x0)) x0;
                Option.map (fun x1 -> ("yOpt", int_to_json x1)) x1;
              ])
    : optional_variant -> Kxclib.Json.jv)
[@@warning "-39"]

and optional_variant_of_json' =
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
        let rec ex10_gen__xy_opt_of_json_nested path __bindoj_orig =
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
              >>= fun x1 -> Ok ({ x_opt = x0; y_opt = x1 } : Ex10_gen.xy_opt)
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
          match Kxclib.Jv.pump_field "tag" __bindoj_orig with
          | `obj (("tag", `str "tuple-like") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "value" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "value" :: path)
              >>= fun x0 -> Ok (Tuple_like x0)
          | `obj (("tag", `str "tuple-like-alias") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "value" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "value" :: path)
              >>= fun x0 -> Ok (Tuple_like_alias x0)
          | `obj (("tag", `str "tuple-like-obj") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "_0" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "_0" :: path)
              >>= fun x0 ->
              List.assoc_opt "_1" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "_1" :: path)
              >>= fun x1 -> Ok (Tuple_like_obj (x0, x1))
          | `obj (("tag", `str "tuple-like-spreading") :: _) ->
              let ( >>= ) = Result.bind in
              ex10_gen__xy_opt_of_json_nested path __bindoj_orig >>= fun x0 ->
              Ok (Tuple_like_spreading x0)
          | `obj (("tag", `str "inline-record") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "intOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "intOpt" :: path)
              >>= fun x0 ->
              List.assoc_opt "xOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "xOpt" :: path)
              >>= fun x1 ->
              List.assoc_opt "yOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "yOpt" :: path)
              >>= fun x2 ->
              List.assoc_opt "objtuple" param
              |> Option.to_result
                   ~none:("mandatory field 'objtuple' does not exist", path)
              >>= (fun path -> function
                    | (`obj fields : Kxclib.Json.jv) ->
                        let ( >>= ) = Result.bind in
                        List.assoc_opt "_0" fields
                        |> Option.value ~default:`null
                        |> (option_of_json' int_of_json') (`f "_0" :: path)
                        >>= fun x0 ->
                        List.assoc_opt "_1" fields
                        |> Option.value ~default:`null
                        |> (option_of_json' int_of_json') (`f "_1" :: path)
                        >>= fun x1 -> Ok (x0, x1)
                    | jv ->
                        Error
                          ( Printf.sprintf
                              "an object is expected for a tuple value, but \
                               the given is of type '%s'"
                              (let open Kxclib.Json in
                               string_of_jv_kind (classify_jv jv)),
                            path ))
                    (`f "objtuple" :: path)
              >>= fun x3 ->
              Ok
                (Inline_record
                   { int_opt = x0; x_opt = x1; y_opt = x2; objtuple = x3 })
          | `obj (("tag", `str "inline-record-spreading") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "intOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "intOpt" :: path)
              >>= fun x0 ->
              ex10_gen__xy_opt_of_json_nested path __bindoj_orig >>= fun x1 ->
              Ok (Inline_record_spreading { int_opt = x0; xy_opt = x1 })
          | `obj (("tag", `str "reused-inline-record") :: param) ->
              let ( >>= ) = Result.bind in
              List.assoc_opt "xOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "xOpt" :: path)
              >>= fun x0 ->
              List.assoc_opt "yOpt" param
              |> Option.value ~default:`null
              |> (option_of_json' int_of_json') (`f "yOpt" :: path)
              >>= fun x1 -> Ok (Reused_inline_record { x_opt = x0; y_opt = x1 })
          | `obj (("tag", `str discriminator_value) :: _) ->
              Error
                ( Printf.sprintf
                    "given discriminator field value '%s' is not one of [ \
                     'tuple-like', 'tuple-like-alias', 'tuple-like-obj', \
                     'tuple-like-spreading', 'inline-record', \
                     'inline-record-spreading', 'reused-inline-record' ]"
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
            (msg, path, optional_variant_json_shape_explanation))
    : optional_variant Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and optional_variant_of_json =
  (fun x -> optional_variant_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> optional_variant option)
[@@warning "-39"]

let optional_variant_decl = Bindoj_test_common_typedesc_examples.Ex18.decl

let optional_variant_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex18.decl optional_variant_reflect
