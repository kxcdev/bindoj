type ex_version_substring_record_v3_2_1 = {
  v5_3_version_info :
    [ `Case_version_v1 | `Case_v2_0_version | `v3_0_1_case_version ];
  version_info_v2 : int;
  version_info_v2_0 : int;
  version_info_v2_0_1 : int;
  version_v3_info : int;
  version_v3_0_info : int;
  version_v3_0_1_info : int;
  v4_version_info : int;
  v4_0_version_info : int;
  v4_0_1_version_info : int;
  no_preserving_v1_2_version : int;
}

let rec (ex_version_substring_record_v3_2_1_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     Refl.Record
       {
         get =
           (fun {
                  v5_3_version_info;
                  version_info_v2;
                  version_info_v2_0;
                  version_info_v2_0_1;
                  version_v3_info;
                  version_v3_0_info;
                  version_v3_0_1_info;
                  v4_version_info;
                  v4_0_version_info;
                  v4_0_1_version_info;
                  no_preserving_v1_2_version;
                }
           ->
             StringMap.of_list
               [
                 ( "v5_3_version_info",
                   (function
                     | `Case_version_v1 -> Expr.StringEnum "Case_version_v1"
                     | `Case_v2_0_version -> Expr.StringEnum "Case_v2_0_version"
                     | `v3_0_1_case_version ->
                         Expr.StringEnum "v3_0_1_case_version")
                     v5_3_version_info );
                 ("version_info_v2", Expr.of_int version_info_v2);
                 ("version_info_v2_0", Expr.of_int version_info_v2_0);
                 ("version_info_v2_0_1", Expr.of_int version_info_v2_0_1);
                 ("version_v3_info", Expr.of_int version_v3_info);
                 ("version_v3_0_info", Expr.of_int version_v3_0_info);
                 ("version_v3_0_1_info", Expr.of_int version_v3_0_1_info);
                 ("v4_version_info", Expr.of_int v4_version_info);
                 ("v4_0_version_info", Expr.of_int v4_0_version_info);
                 ("v4_0_1_version_info", Expr.of_int v4_0_1_version_info);
                 ( "no_preserving_v1_2_version",
                   Expr.of_int no_preserving_v1_2_version );
               ]);
         mk =
           (fun xs ->
             ( xs |> StringMap.find_opt "v5_3_version_info" >>= function
               | Expr.StringEnum "Case_version_v1" -> Some `Case_version_v1
               | Expr.StringEnum "Case_v2_0_version" -> Some `Case_v2_0_version
               | Expr.StringEnum "v3_0_1_case_version" ->
                   Some `v3_0_1_case_version
               | _ -> None )
             >>= fun v5_3_version_info ->
             xs |> StringMap.find_opt "version_info_v2" >>= Expr.to_int
             >>= fun version_info_v2 ->
             xs |> StringMap.find_opt "version_info_v2_0" >>= Expr.to_int
             >>= fun version_info_v2_0 ->
             xs |> StringMap.find_opt "version_info_v2_0_1" >>= Expr.to_int
             >>= fun version_info_v2_0_1 ->
             xs |> StringMap.find_opt "version_v3_info" >>= Expr.to_int
             >>= fun version_v3_info ->
             xs |> StringMap.find_opt "version_v3_0_info" >>= Expr.to_int
             >>= fun version_v3_0_info ->
             xs |> StringMap.find_opt "version_v3_0_1_info" >>= Expr.to_int
             >>= fun version_v3_0_1_info ->
             xs |> StringMap.find_opt "v4_version_info" >>= Expr.to_int
             >>= fun v4_version_info ->
             xs |> StringMap.find_opt "v4_0_version_info" >>= Expr.to_int
             >>= fun v4_0_version_info ->
             xs |> StringMap.find_opt "v4_0_1_version_info" >>= Expr.to_int
             >>= fun v4_0_1_version_info ->
             xs
             |> StringMap.find_opt "no_preserving_v1_2_version"
             >>= Expr.to_int
             >>= fun no_preserving_v1_2_version ->
             Some
               {
                 v5_3_version_info;
                 version_info_v2;
                 version_info_v2_0;
                 version_info_v2_0_1;
                 version_v3_info;
                 version_v3_0_info;
                 version_v3_0_1_info;
                 v4_version_info;
                 v4_0_version_info;
                 v4_0_1_version_info;
                 no_preserving_v1_2_version;
               });
       })
[@@warning "-33-39"]

let ex_version_substring_record_v3_2_1_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExVersionSubstringRecordV3_2_1",
           `object_of
             [
               `mandatory_field
                 ( "v5_3VersionInfo",
                   `string_enum
                     [
                       "Case-version-v1";
                       "Case-v2_0-version";
                       "v3_0_1-case-version";
                     ] );
               `mandatory_field ("versionInfoV2", `integral);
               `mandatory_field ("versionInfoV2_0", `integral);
               `mandatory_field ("versionInfoV2_0_1", `integral);
               `mandatory_field ("versionV3Info", `integral);
               `mandatory_field ("versionV3_0Info", `integral);
               `mandatory_field ("versionV3_0_1Info", `integral);
               `mandatory_field ("v4VersionInfo", `integral);
               `mandatory_field ("v4_0VersionInfo", `integral);
               `mandatory_field ("v4_0_1VersionInfo", `integral);
               `mandatory_field ("noPreservingV12Version", `integral);
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_version_substring_record_v3_2_1_to_json =
  (let int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   fun {
         v5_3_version_info = x0;
         version_info_v2 = x1;
         version_info_v2_0 = x2;
         version_info_v2_0_1 = x3;
         version_v3_info = x4;
         version_v3_0_info = x5;
         version_v3_0_1_info = x6;
         v4_version_info = x7;
         v4_0_version_info = x8;
         v4_0_1_version_info = x9;
         no_preserving_v1_2_version = x10;
       } ->
     `obj
       [
         ( "v5_3VersionInfo",
           (function
             | `Case_version_v1 -> `str "Case-version-v1"
             | `Case_v2_0_version -> `str "Case-v2_0-version"
             | `v3_0_1_case_version -> `str "v3_0_1-case-version")
             x0 );
         ("versionInfoV2", int_to_json x1);
         ("versionInfoV2_0", int_to_json x2);
         ("versionInfoV2_0_1", int_to_json x3);
         ("versionV3Info", int_to_json x4);
         ("versionV3_0Info", int_to_json x5);
         ("versionV3_0_1Info", int_to_json x6);
         ("v4VersionInfo", int_to_json x7);
         ("v4_0VersionInfo", int_to_json x8);
         ("v4_0_1VersionInfo", int_to_json x9);
         ("noPreservingV12Version", int_to_json x10);
       ]
    : ex_version_substring_record_v3_2_1 -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_version_substring_record_v3_2_1_of_json' =
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl =
         let int_of_json' path = function
           | (`num x : Kxclib.Json.jv) ->
               if Float.is_integer x then Ok (int_of_float x)
               else
                 Error
                   ( Printf.sprintf "expecting an integer but the given is '%f'"
                       x,
                     path )
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'int' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         in
         fun path ->
           fun __bindoj_orig ->
            match __bindoj_orig with
            | `obj param ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "v5_3VersionInfo" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'v5_3VersionInfo' does not exist", path)
                >>= (fun path -> function
                      | `str s ->
                          (function
                            | "Case-version-v1" -> Ok `Case_version_v1
                            | "Case-v2_0-version" -> Ok `Case_v2_0_version
                            | "v3_0_1-case-version" -> Ok `v3_0_1_case_version
                            | s ->
                                Error
                                  ( Printf.sprintf
                                      "given string '%s' is not one of [ \
                                       'Case-version-v1', 'Case-v2_0-version', \
                                       'v3_0_1-case-version' ]"
                                      s,
                                    path ))
                            s
                      | jv ->
                          Error
                            ( Printf.sprintf
                                "expecting type 'string' but the given is of \
                                 type '%s'"
                                (let open Kxclib.Json in
                                 string_of_jv_kind (classify_jv jv)),
                              path ))
                      (`f "v5_3VersionInfo" :: path)
                >>= fun x0 ->
                List.assoc_opt "versionInfoV2" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'versionInfoV2' does not exist", path)
                >>= int_of_json' (`f "versionInfoV2" :: path)
                >>= fun x1 ->
                List.assoc_opt "versionInfoV2_0" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'versionInfoV2_0' does not exist", path)
                >>= int_of_json' (`f "versionInfoV2_0" :: path)
                >>= fun x2 ->
                List.assoc_opt "versionInfoV2_0_1" param
                |> Option.to_result
                     ~none:
                       ( "mandatory field 'versionInfoV2_0_1' does not exist",
                         path )
                >>= int_of_json' (`f "versionInfoV2_0_1" :: path)
                >>= fun x3 ->
                List.assoc_opt "versionV3Info" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'versionV3Info' does not exist", path)
                >>= int_of_json' (`f "versionV3Info" :: path)
                >>= fun x4 ->
                List.assoc_opt "versionV3_0Info" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'versionV3_0Info' does not exist", path)
                >>= int_of_json' (`f "versionV3_0Info" :: path)
                >>= fun x5 ->
                List.assoc_opt "versionV3_0_1Info" param
                |> Option.to_result
                     ~none:
                       ( "mandatory field 'versionV3_0_1Info' does not exist",
                         path )
                >>= int_of_json' (`f "versionV3_0_1Info" :: path)
                >>= fun x6 ->
                List.assoc_opt "v4VersionInfo" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'v4VersionInfo' does not exist", path)
                >>= int_of_json' (`f "v4VersionInfo" :: path)
                >>= fun x7 ->
                List.assoc_opt "v4_0VersionInfo" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'v4_0VersionInfo' does not exist", path)
                >>= int_of_json' (`f "v4_0VersionInfo" :: path)
                >>= fun x8 ->
                List.assoc_opt "v4_0_1VersionInfo" param
                |> Option.to_result
                     ~none:
                       ( "mandatory field 'v4_0_1VersionInfo' does not exist",
                         path )
                >>= int_of_json' (`f "v4_0_1VersionInfo" :: path)
                >>= fun x9 ->
                List.assoc_opt "noPreservingV12Version" param
                |> Option.to_result
                     ~none:
                       ( "mandatory field 'noPreservingV12Version' does not \
                          exist",
                         path )
                >>= int_of_json' (`f "noPreservingV12Version" :: path)
                >>= fun x10 ->
                Ok
                  {
                    v5_3_version_info = x0;
                    version_info_v2 = x1;
                    version_info_v2_0 = x2;
                    version_info_v2_0_1 = x3;
                    version_v3_info = x4;
                    version_v3_0_info = x5;
                    version_v3_0_1_info = x6;
                    v4_version_info = x7;
                    v4_0_version_info = x8;
                    v4_0_1_version_info = x9;
                    no_preserving_v1_2_version = x10;
                  }
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
             ( msg,
               path,
               ex_version_substring_record_v3_2_1_json_shape_explanation ))
    : ex_version_substring_record_v3_2_1 Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_version_substring_record_v3_2_1_of_json =
  (fun x -> ex_version_substring_record_v3_2_1_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_version_substring_record_v3_2_1 option)
[@@warning "-39"]

let ex_version_substring_record_v3_2_1_decl =
  Bindoj_test_common_typedesc_examples.Ex_version_substring.Record.decl

let ex_version_substring_record_v3_2_1_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_version_substring.Record.decl
    ex_version_substring_record_v3_2_1_reflect

type ex_version_substring_variant_v1_0 =
  | Version_info_v1_0 of {
      version_info_v1 : int;
      version_info_v1_0 : int;
      version_info_v1_0_1 : int;
    }
  | Version_v1_0_info of {
      version_v1_info : int;
      version_v1_0_info : int;
      version_v1_0_1_info : int;
    }
  | V1_0_version_info of {
      v1_version_info : int;
      v1_0_version_info : int;
      v1_0_1_version_info : int;
    }
  | No_preserving_version_substring_v1_0 of {
      version_info_v1 : int;
      version_info_v1_0 : int;
      version_info_v1_0_1 : int;
    }

let rec (ex_version_substring_variant_v1_0_reflect : _ Bindoj_runtime.Refl.t) =
  lazy
    (let open Bindoj_runtime in
     let open Kxclib.Option.Ops_monad in
     let ctor_Version_info_v1_0 =
       Refl.InlineRecord
         {
           get =
             (function
             | Version_info_v1_0
                 { version_info_v1; version_info_v1_0; version_info_v1_0_1 } ->
                 StringMap.of_list
                   [
                     ("version_info_v1", Expr.of_int version_info_v1);
                     ("version_info_v1_0", Expr.of_int version_info_v1_0);
                     ("version_info_v1_0_1", Expr.of_int version_info_v1_0_1);
                   ]
             | _ -> invalid_arg "Version_info_v1_0 is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "version_info_v1" >>= Expr.to_int
               >>= fun version_info_v1 ->
               xs |> StringMap.find_opt "version_info_v1_0" >>= Expr.to_int
               >>= fun version_info_v1_0 ->
               xs |> StringMap.find_opt "version_info_v1_0_1" >>= Expr.to_int
               >>= fun version_info_v1_0_1 ->
               Some
                 (Version_info_v1_0
                    { version_info_v1; version_info_v1_0; version_info_v1_0_1 }));
         }
     in
     let ctor_Version_v1_0_info =
       Refl.InlineRecord
         {
           get =
             (function
             | Version_v1_0_info
                 { version_v1_info; version_v1_0_info; version_v1_0_1_info } ->
                 StringMap.of_list
                   [
                     ("version_v1_info", Expr.of_int version_v1_info);
                     ("version_v1_0_info", Expr.of_int version_v1_0_info);
                     ("version_v1_0_1_info", Expr.of_int version_v1_0_1_info);
                   ]
             | _ -> invalid_arg "Version_v1_0_info is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "version_v1_info" >>= Expr.to_int
               >>= fun version_v1_info ->
               xs |> StringMap.find_opt "version_v1_0_info" >>= Expr.to_int
               >>= fun version_v1_0_info ->
               xs |> StringMap.find_opt "version_v1_0_1_info" >>= Expr.to_int
               >>= fun version_v1_0_1_info ->
               Some
                 (Version_v1_0_info
                    { version_v1_info; version_v1_0_info; version_v1_0_1_info }));
         }
     in
     let ctor_V1_0_version_info =
       Refl.InlineRecord
         {
           get =
             (function
             | V1_0_version_info
                 { v1_version_info; v1_0_version_info; v1_0_1_version_info } ->
                 StringMap.of_list
                   [
                     ("v1_version_info", Expr.of_int v1_version_info);
                     ("v1_0_version_info", Expr.of_int v1_0_version_info);
                     ("v1_0_1_version_info", Expr.of_int v1_0_1_version_info);
                   ]
             | _ -> invalid_arg "V1_0_version_info is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "v1_version_info" >>= Expr.to_int
               >>= fun v1_version_info ->
               xs |> StringMap.find_opt "v1_0_version_info" >>= Expr.to_int
               >>= fun v1_0_version_info ->
               xs |> StringMap.find_opt "v1_0_1_version_info" >>= Expr.to_int
               >>= fun v1_0_1_version_info ->
               Some
                 (V1_0_version_info
                    { v1_version_info; v1_0_version_info; v1_0_1_version_info }));
         }
     in
     let ctor_No_preserving_version_substring_v1_0 =
       Refl.InlineRecord
         {
           get =
             (function
             | No_preserving_version_substring_v1_0
                 { version_info_v1; version_info_v1_0; version_info_v1_0_1 } ->
                 StringMap.of_list
                   [
                     ("version_info_v1", Expr.of_int version_info_v1);
                     ("version_info_v1_0", Expr.of_int version_info_v1_0);
                     ("version_info_v1_0_1", Expr.of_int version_info_v1_0_1);
                   ]
             | _ ->
                 invalid_arg "No_preserving_version_substring_v1_0 is expected");
           mk =
             (fun xs ->
               xs |> StringMap.find_opt "version_info_v1" >>= Expr.to_int
               >>= fun version_info_v1 ->
               xs |> StringMap.find_opt "version_info_v1_0" >>= Expr.to_int
               >>= fun version_info_v1_0 ->
               xs |> StringMap.find_opt "version_info_v1_0_1" >>= Expr.to_int
               >>= fun version_info_v1_0_1 ->
               Some
                 (No_preserving_version_substring_v1_0
                    { version_info_v1; version_info_v1_0; version_info_v1_0_1 }));
         }
     in
     Refl.Variant
       {
         constructors =
           StringMap.of_list
             [
               ("Version_info_v1_0", ctor_Version_info_v1_0);
               ("Version_v1_0_info", ctor_Version_v1_0_info);
               ("V1_0_version_info", ctor_V1_0_version_info);
               ( "No_preserving_version_substring_v1_0",
                 ctor_No_preserving_version_substring_v1_0 );
             ];
         classify =
           (function
           | Version_info_v1_0 _ -> ("Version_info_v1_0", ctor_Version_info_v1_0)
           | Version_v1_0_info _ -> ("Version_v1_0_info", ctor_Version_v1_0_info)
           | V1_0_version_info _ -> ("V1_0_version_info", ctor_V1_0_version_info)
           | No_preserving_version_substring_v1_0 _ ->
               ( "No_preserving_version_substring_v1_0",
                 ctor_No_preserving_version_substring_v1_0 ));
       })
[@@warning "-33-39"]

let ex_version_substring_variant_v1_0_json_discriminator_value =
  (function
   | Version_info_v1_0 _ -> "version-info-v1_0"
   | Version_v1_0_info _ -> "version-v1_0-info"
   | V1_0_version_info _ -> "v1_0-version-info"
   | No_preserving_version_substring_v1_0 _ ->
       "no-preserving-version-substring-v1-0"
    : ex_version_substring_variant_v1_0 -> string)
[@@warning "-39"]

let ex_version_substring_variant_v1_0_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "ExVersionSubstringVariantV1_0",
           `anyone_of
             [
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "version-info-v1_0"));
                   `mandatory_field ("versionInfoV1", `integral);
                   `mandatory_field ("versionInfoV1_0", `integral);
                   `mandatory_field ("versionInfoV1_0_1", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "version-v1_0-info"));
                   `mandatory_field ("versionV1Info", `integral);
                   `mandatory_field ("versionV1_0Info", `integral);
                   `mandatory_field ("versionV1_0_1Info", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field ("kind", `exactly (`str "v1_0-version-info"));
                   `mandatory_field ("v1VersionInfo", `integral);
                   `mandatory_field ("v1_0VersionInfo", `integral);
                   `mandatory_field ("v1_0_1VersionInfo", `integral);
                 ];
               `object_of
                 [
                   `mandatory_field
                     ( "kind",
                       `exactly (`str "no-preserving-version-substring-v1-0") );
                   `mandatory_field ("versionInfoV1", `integral);
                   `mandatory_field ("versionInfoV10", `integral);
                   `mandatory_field ("versionInfoV101", `integral);
                 ];
             ] ) )
    : Bindoj_runtime.json_shape_explanation)
[@@warning "-39"]

let rec ex_version_substring_variant_v1_0_to_json =
  (let int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   function
   | Version_info_v1_0
       {
         version_info_v1 = x0;
         version_info_v1_0 = x1;
         version_info_v1_0_1 = x2;
       } ->
       `obj
         [
           ("kind", `str "version-info-v1_0");
           ("versionInfoV1", int_to_json x0);
           ("versionInfoV1_0", int_to_json x1);
           ("versionInfoV1_0_1", int_to_json x2);
         ]
   | Version_v1_0_info
       {
         version_v1_info = x0;
         version_v1_0_info = x1;
         version_v1_0_1_info = x2;
       } ->
       `obj
         [
           ("kind", `str "version-v1_0-info");
           ("versionV1Info", int_to_json x0);
           ("versionV1_0Info", int_to_json x1);
           ("versionV1_0_1Info", int_to_json x2);
         ]
   | V1_0_version_info
       {
         v1_version_info = x0;
         v1_0_version_info = x1;
         v1_0_1_version_info = x2;
       } ->
       `obj
         [
           ("kind", `str "v1_0-version-info");
           ("v1VersionInfo", int_to_json x0);
           ("v1_0VersionInfo", int_to_json x1);
           ("v1_0_1VersionInfo", int_to_json x2);
         ]
   | No_preserving_version_substring_v1_0
       {
         version_info_v1 = x0;
         version_info_v1_0 = x1;
         version_info_v1_0_1 = x2;
       } ->
       `obj
         [
           ("kind", `str "no-preserving-version-substring-v1-0");
           ("versionInfoV1", int_to_json x0);
           ("versionInfoV10", int_to_json x1);
           ("versionInfoV101", int_to_json x2);
         ]
    : ex_version_substring_variant_v1_0 -> Kxclib.Json.jv)
[@@warning "-39"]

and ex_version_substring_variant_v1_0_of_json' =
  (fun ?(path = []) ->
     fun x ->
      (let rec of_json_impl =
         let int_of_json' path = function
           | (`num x : Kxclib.Json.jv) ->
               if Float.is_integer x then Ok (int_of_float x)
               else
                 Error
                   ( Printf.sprintf "expecting an integer but the given is '%f'"
                       x,
                     path )
           | jv ->
               Error
                 ( Printf.sprintf
                     "expecting type 'int' but the given is of type '%s'"
                     (let open Kxclib.Json in
                      string_of_jv_kind (classify_jv jv)),
                   path )
         in
         fun path ->
           fun __bindoj_orig ->
            match Kxclib.Jv.pump_field "kind" __bindoj_orig with
            | `obj (("kind", `str "version-info-v1_0") :: param) ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "versionInfoV1" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'versionInfoV1' does not exist", path)
                >>= int_of_json' (`f "versionInfoV1" :: path)
                >>= fun x0 ->
                List.assoc_opt "versionInfoV1_0" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'versionInfoV1_0' does not exist", path)
                >>= int_of_json' (`f "versionInfoV1_0" :: path)
                >>= fun x1 ->
                List.assoc_opt "versionInfoV1_0_1" param
                |> Option.to_result
                     ~none:
                       ( "mandatory field 'versionInfoV1_0_1' does not exist",
                         path )
                >>= int_of_json' (`f "versionInfoV1_0_1" :: path)
                >>= fun x2 ->
                Ok
                  (Version_info_v1_0
                     {
                       version_info_v1 = x0;
                       version_info_v1_0 = x1;
                       version_info_v1_0_1 = x2;
                     })
            | `obj (("kind", `str "version-v1_0-info") :: param) ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "versionV1Info" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'versionV1Info' does not exist", path)
                >>= int_of_json' (`f "versionV1Info" :: path)
                >>= fun x0 ->
                List.assoc_opt "versionV1_0Info" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'versionV1_0Info' does not exist", path)
                >>= int_of_json' (`f "versionV1_0Info" :: path)
                >>= fun x1 ->
                List.assoc_opt "versionV1_0_1Info" param
                |> Option.to_result
                     ~none:
                       ( "mandatory field 'versionV1_0_1Info' does not exist",
                         path )
                >>= int_of_json' (`f "versionV1_0_1Info" :: path)
                >>= fun x2 ->
                Ok
                  (Version_v1_0_info
                     {
                       version_v1_info = x0;
                       version_v1_0_info = x1;
                       version_v1_0_1_info = x2;
                     })
            | `obj (("kind", `str "v1_0-version-info") :: param) ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "v1VersionInfo" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'v1VersionInfo' does not exist", path)
                >>= int_of_json' (`f "v1VersionInfo" :: path)
                >>= fun x0 ->
                List.assoc_opt "v1_0VersionInfo" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'v1_0VersionInfo' does not exist", path)
                >>= int_of_json' (`f "v1_0VersionInfo" :: path)
                >>= fun x1 ->
                List.assoc_opt "v1_0_1VersionInfo" param
                |> Option.to_result
                     ~none:
                       ( "mandatory field 'v1_0_1VersionInfo' does not exist",
                         path )
                >>= int_of_json' (`f "v1_0_1VersionInfo" :: path)
                >>= fun x2 ->
                Ok
                  (V1_0_version_info
                     {
                       v1_version_info = x0;
                       v1_0_version_info = x1;
                       v1_0_1_version_info = x2;
                     })
            | `obj
                (("kind", `str "no-preserving-version-substring-v1-0") :: param)
              ->
                let ( >>= ) = Result.bind in
                List.assoc_opt "versionInfoV1" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'versionInfoV1' does not exist", path)
                >>= int_of_json' (`f "versionInfoV1" :: path)
                >>= fun x0 ->
                List.assoc_opt "versionInfoV10" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'versionInfoV10' does not exist", path)
                >>= int_of_json' (`f "versionInfoV10" :: path)
                >>= fun x1 ->
                List.assoc_opt "versionInfoV101" param
                |> Option.to_result
                     ~none:
                       ("mandatory field 'versionInfoV101' does not exist", path)
                >>= int_of_json' (`f "versionInfoV101" :: path)
                >>= fun x2 ->
                Ok
                  (No_preserving_version_substring_v1_0
                     {
                       version_info_v1 = x0;
                       version_info_v1_0 = x1;
                       version_info_v1_0_1 = x2;
                     })
            | `obj (("kind", `str discriminator_value) :: _) ->
                Error
                  ( Printf.sprintf
                      "given discriminator field value '%s' is not one of [ \
                       'version-info-v1_0', 'version-v1_0-info', \
                       'v1_0-version-info', \
                       'no-preserving-version-substring-v1-0' ]"
                      discriminator_value,
                    `f "kind" :: path )
            | `obj (("kind", jv) :: _) ->
                Error
                  ( Printf.sprintf
                      "a string is expected for a variant discriminator, but \
                       the given is of type '%s'"
                      (let open Kxclib.Json in
                       string_of_jv_kind (classify_jv jv)),
                    `f "kind" :: path )
            | `obj _ -> Error ("discriminator field 'kind' does not exist", path)
            | jv ->
                Error
                  ( Printf.sprintf
                      "an object is expected for a variant value, but the \
                       given is of type '%s'"
                      (let open Kxclib.Json in
                       string_of_jv_kind (classify_jv jv)),
                    path )
       in
       of_json_impl)
        path x
      |> Result.map_error (fun (msg, path) ->
             ( msg,
               path,
               ex_version_substring_variant_v1_0_json_shape_explanation ))
    : ex_version_substring_variant_v1_0 Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and ex_version_substring_variant_v1_0_of_json =
  (fun x -> ex_version_substring_variant_v1_0_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> ex_version_substring_variant_v1_0 option)
[@@warning "-39"]

let ex_version_substring_variant_v1_0_decl =
  Bindoj_test_common_typedesc_examples.Ex_version_substring.Variant.decl

let ex_version_substring_variant_v1_0_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex_version_substring.Variant.decl
    ex_version_substring_variant_v1_0_reflect
