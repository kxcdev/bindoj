type v3_2_1_preserving_version_info = {
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

let rec (v3_2_1_preserving_version_info_reflect : _ Bindoj_runtime.Refl.t) =
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
                } ->
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
             (xs |> StringMap.find_opt "v5_3_version_info" >>= function
              | Expr.StringEnum "Case_version_v1" -> Some `Case_version_v1
              | Expr.StringEnum "Case_v2_0_version" -> Some `Case_v2_0_version
              | Expr.StringEnum "v3_0_1_case_version" ->
                  Some `v3_0_1_case_version
              | _ -> None)
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

let v3_2_1_preserving_version_info_json_shape_explanation =
  (`with_warning
     ( "not considering any config if exists",
       `named
         ( "V3_2_1PreservingVersionInfo",
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

let rec v3_2_1_preserving_version_info_to_json =
  (let int_to_json (x : int) : Kxclib.Json.jv = `num (float_of_int x) in
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
    : v3_2_1_preserving_version_info -> Kxclib.Json.jv)
[@@warning "-39"]

and v3_2_1_preserving_version_info_of_json' =
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
                   ~none:("mandatory field 'versionInfoV2' does not exist", path)
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
                     ("mandatory field 'versionInfoV2_0_1' does not exist", path)
              >>= int_of_json' (`f "versionInfoV2_0_1" :: path)
              >>= fun x3 ->
              List.assoc_opt "versionV3Info" param
              |> Option.to_result
                   ~none:("mandatory field 'versionV3Info' does not exist", path)
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
                     ("mandatory field 'versionV3_0_1Info' does not exist", path)
              >>= int_of_json' (`f "versionV3_0_1Info" :: path)
              >>= fun x6 ->
              List.assoc_opt "v4VersionInfo" param
              |> Option.to_result
                   ~none:("mandatory field 'v4VersionInfo' does not exist", path)
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
                     ("mandatory field 'v4_0_1VersionInfo' does not exist", path)
              >>= int_of_json' (`f "v4_0_1VersionInfo" :: path)
              >>= fun x9 ->
              List.assoc_opt "noPreservingV12Version" param
              |> Option.to_result
                   ~none:
                     ( "mandatory field 'noPreservingV12Version' does not exist",
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
            (msg, path, v3_2_1_preserving_version_info_json_shape_explanation))
    : v3_2_1_preserving_version_info Bindoj_runtime.json_full_decoder)
[@@warning "-39"]

and v3_2_1_preserving_version_info_of_json =
  (fun x -> v3_2_1_preserving_version_info_of_json' x |> Result.to_option
    : Kxclib.Json.jv -> v3_2_1_preserving_version_info option)
[@@warning "-39"]

let v3_2_1_preserving_version_info_decl =
  Bindoj_test_common_typedesc_examples.Ex19.decl

let v3_2_1_preserving_version_info_typed_decl =
  Bindoj_runtime.mk_generic_typed_type_decl
    Bindoj_test_common_typedesc_examples.Ex19.decl
    v3_2_1_preserving_version_info_reflect
