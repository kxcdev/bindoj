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

module type Sample_variant = sig
  type t
  val decl : Type_desc.type_decl
  val reflect : t Refl.t
  val json_discriminator_value : t -> string
  val sample_values : t Util.Sample_value.t list

  val expected : t -> string
end

module Ex_variant_person = struct
  include Ex_variant.Person
  let expected = function
    | Anonymous -> "anonymous"
    | With_id _ -> "with-id"
    | Student _ -> "student"
    | Teacher _ -> "teacher"
end

module Ex_mangling_person_no_mangling = struct
  include Ex_mangling.Person_no_mangling
  let expected = function
    | Anonymous -> "Anonymous"
    | With_id _ -> "With_id"
    | Student _ -> "Student"
    | Teacher _ -> "Teacher"
end

module Ex_variant_int_list = struct
  include Ex_variant.Int_list
  let expected = function
    | IntNil -> "intnil"
    | IntCons _ -> "intcons"
end

module Ex_variant_polymorphic = struct
  include Ex_variant.Polymorphic
  let expected = function
    | `Foo0 -> "foo0"
    | `Foo1 _ -> "foo1"
    | `Foo2 _ -> "foo2"
end

module Ex_variant_customized_union = struct
  include Ex_variant.Customized_union
  let expected = function
    | Case_tuple_like_arg _ -> "case-tuple-like-arg'"
    | Case_tuple_like_exactly _ -> "case-tuple-like-exactly'"
    | Case_tuple_like_kind_name _ -> "case-tuple-like-kind-name'"
    | Case_tuple_like_kind_name_no_mangling _ -> "case-tuple-like-kind-name-no-mangling"
    | Case_tuple_like_kind_name_no_mangling_with_ctor_name _ -> "case-tuple-like-kind-name-no-mangling-with-ctor-name"
    | Case_inline_record _ -> "case-inline-record'"
end

let all_generated_variant_decl : (module Sample_variant) list =
  [ (module Ex_variant_person);
    (module Ex_mangling_person_no_mangling);
    (module Ex_variant_int_list);
    (module Ex_variant_polymorphic);
    (module Ex_variant_customized_union); ]

let create_test_cases (module S : Sample_variant) =
  S.sample_values
  |> List.mapi (fun i Util.Sample_value.{ orig; _ } ->
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
  |> fun cases -> (S.decl.td_name, cases)

let () =
  all_generated_variant_decl |&> create_test_cases
  |> Alcotest.run "json_discriminator_value_test"
