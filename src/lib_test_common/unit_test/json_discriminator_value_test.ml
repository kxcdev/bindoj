(* Copyright 2022-2023 Kotoi-Xie Consultancy, Inc. This file is a part of the

==== Bindoj (https://kxc.dev/bindoj) ====

software project that is developed, maintained, and distributed by
Kotoi-Xie Consultancy, Inc. (https://kxc.inc) which is also known as KXC.

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy
of the License at http://www.apache.org/licenses/LICENSE-2.0. Unless required
by applicable law or agreed to in writing, software distributed under the
License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
OF ANY KIND, either express or implied. See the License for the specific
language governing permissions and limitations under the License.
                                                                              *)
(* Acknowledgements  --- AnchorZ Inc. ---  The current/initial version or a
significant portion of this file is developed under the funding provided by
AnchorZ Inc. to satisfy its needs in its product development workflow.
                                                                              *)
open Alcotest
open Kxclib
open Bindoj_codec
open Bindoj_typedesc
open Bindoj_runtime
open Bindoj_test_common.Typedesc_generated_examples

module type SampleVariantDecl = sig
  type t
  val decl : Type_desc.type_decl
  val reflect : t Refl.t
  val json_discriminator_value : t -> string
  val sample_values : t Sample_value.t list

  val name : string
  val expected : t -> string
end

module Ex02 = struct
  include Ex02
  let name = "Ex02"
  let expected = function
    | Anonymous -> "anonymous"
    | With_id _ -> "with-id"
    | Student _ -> "student"
    | Teacher _ -> "teacher"
end

module Ex02_no_mangling = struct
  include Ex02_no_mangling
  let name = "Ex02_no_mangling"
  let expected = function
    | Anonymous -> "Anonymous"
    | With_id _ -> "With_id"
    | Student _ -> "Student"
    | Teacher _ -> "Teacher"
end

module Ex03 = struct
  include Ex03
  let name = "Ex03"
  let expected = function
    | IntNil -> "intnil"
    | IntCons _ -> "intcons"
end

module Ex04 = struct
  include Ex04
  let name = "Ex04"
  let expected = function
    | `Foo0 -> "foo0"
    | `Foo1 _ -> "foo1"
    | `Foo2 _ -> "foo2"
end

module Ex07 = struct
  include Ex07
  let name = "Ex07"
  let expected = function
    | Case1 _ -> "case1'"
    | Case2 _ -> "case2'"
end

let all_generated_variant_decl : (module SampleVariantDecl) list =
  [ (module Ex02);
    (module Ex02_no_mangling);
    (module Ex03);
    (module Ex04);
    (module Ex07); ]

let create_test_cases (module S : SampleVariantDecl) =
  S.sample_values
  |> List.mapi (fun i Sample_value.{ orig; _ } ->
    let test_name s = sprintf "[%s] sample_value%02d" s i in
    let expected = S.expected orig in
    [ test_case (test_name "interpreted") `Quick (fun () ->
        let interpreted =
          let type_decl = Typed_type_desc.Typed.mk S.decl S.reflect in
          Json.get_json_discriminator_value type_decl orig in

        check' string ~msg:"interpreted" ~expected ~actual:interpreted;
      );
      test_case (test_name "compiled") `Quick (fun () ->
        let compiled = S.json_discriminator_value orig in
        check' string ~msg:"compiled" ~expected ~actual:compiled;
      );
    ])
  |> List.concat
  |> fun cases -> (S.name, cases)

let () =
  all_generated_variant_decl |&> create_test_cases
  |> Alcotest.run "json_discriminator_value_test"
