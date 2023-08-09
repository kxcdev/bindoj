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
open Kxclib
open Bindoj_runtime
open Bindoj_typedesc

let eps = 0.00001

module type Testable = sig
  type t
  val label : string
  val coretype : t Coretypes.t
  val testable : t Alcotest.testable
  val to_string : t -> string
  val values : t list
end

module TestUnit : Testable = struct
  type t = unit
  let label = "unit"
  let coretype = Coretypes.Prims.unit
  let testable = Alcotest.unit
  let to_string = Unit.to_string
  let values = [ () ]
end

module TestBool : Testable = struct
  type t = bool
  let label = "bool"
  let coretype = Coretypes.Prims.bool
  let testable = Alcotest.bool
  let to_string = Bool.to_string
  let values = [ false; true ]
end

module TestInt : Testable = struct
  type t = int
  let label = "int"
  let coretype = Coretypes.Prims.int
  let testable = Alcotest.int
  let to_string = Int.to_string
  let values = [ -2; -1; 0; 1; 2; 42 ]
end

module TestInt53p : Testable = struct
  type t = int53p
  let label = "int53p"
  let coretype = Coretypes.Prims.int53p
  let testable =
    let open Alcotest in
    let cast : _ testable -> t testable = Obj.magic in
    match Int53p.impl_flavor with
    | `int_impl -> cast int
    | `float_impl -> cast (float eps)
    | `int64_impl -> cast int64
    | `custom_impl _ -> failwith "custom_impl should not be used in test."
  let to_string = Int53p.to_string
  let values = [ -2; -1; 0; 1; 2; 42 ] |&> Int53p.of_int
end

module TestFloat : Testable = struct
  type t = float
  let label = "float"
  let coretype = Coretypes.Prims.float
  let testable = Alcotest.float eps
  let to_string = Float.to_string
  let values = [ -1.0; -0.5; 0.0; 0.5; 1.0; 2.0; 42.0 ]
end

module TestString : Testable = struct
  type t = string
  let label = "string"
  let coretype = Coretypes.Prims.string
  let testable = Alcotest.string
  let to_string = identity
  let values = [ ""; "0"; "abc" ]
end

module TestUchar : Testable = struct
  type t = Uchar.t
  let label = "uchar"
  let coretype = Coretypes.Prims.uchar
  let testable: t Alcotest.testable = Obj.magic Alcotest.int
  let to_string: t -> string = Obj.magic Int.to_string
  let values =
    [ Uchar.of_char '2';
      Uchar.of_char 'a';
      Uchar.of_int 0;
      Uchar.of_int 1;
      Uchar.of_int 512 ]
end

module TestByte : Testable = struct
  type t = int
  let label = "byte"
  let coretype = Coretypes.Prims.byte
  let testable = Alcotest.int
  let to_string: t -> string = Obj.magic Int.to_string
  let values = [ -2; -1; 0; 1; 2 ]
end

module TestBytes : Testable = struct
  type t = bytes
  let label = "bytes"
  let coretype = Coretypes.Prims.bytes
  let testable = Alcotest.bytes
  let to_string = Bytes.to_string
  let values =
    [ Bytes.of_string "";
      Bytes.of_string "abc"
    ]
end

module TestOption(M : Testable) : Testable = struct
  type t = M.t option
  let label = sprintf "%s option" M.label
  let coretype = Coretypes.option M.coretype
  let testable = Alcotest.option M.testable
  let to_string = function
    | None -> "None"
    | Some x -> sprintf "Some(%s)" (M.to_string x)
  let values = None :: (M.values |&> some)
end

module TestList(M : Testable) : Testable = struct
  type t = M.t list
  let label = sprintf "%s list" M.label
  let coretype = Coretypes.list M.coretype
  let testable = Alcotest.list M.testable
  let to_string =
    List.map M.to_string
    &> String.concat ", "
    &> sprintf "[%s]"
  let values =
    [ M.values;
      List.init 5 (constant (List.hd M.values))
    ]
end

module TestTup2(M0 : Testable) (M1 : Testable) : Testable = struct
  type t = (M0.t * M1.t)
  let label = sprintf "%s * %s" M0.label M1.label
  let coretype = Coretypes.Tuple.tup2 (M0.coretype, M1.coretype)
  let testable = Alcotest.pair M0.testable M1.testable
  let to_string (a0, a1) = sprintf "(%s, %s)" (M0.to_string a0) (M1.to_string a1)
  let values =
    let open MonadOps(List0) in
    M0.values
    >>= (fun v0 ->
      M1.values
      |&> (fun v1 ->
        (v0, v1)))
end

module TestTup3(M0 : Testable) (M1 : Testable) (M2 : Testable) : Testable = struct
  type t = (M0.t * M1.t * M2.t)
  let label = sprintf "%s * %s * %s" M0.label M1.label M2.label
  let coretype = Coretypes.Tuple.tup3 (M0.coretype, M1.coretype, M2.coretype)
  let testable = Alcotest.triple M0.testable M1.testable M2.testable
  let to_string (a0, a1, a2) = sprintf "(%s, %s, %s)" (M0.to_string a0) (M1.to_string a1) (M2.to_string a2)
  let values =
    let open MonadOps(List0) in
    M0.values
    >>= (fun v0 ->
      M1.values
      >>= (fun v1 ->
        M2.values
        |&> (fun v2 -> (v0, v1, v2))))
end

module TestStringMap(M : Testable) : Testable = struct
  type t = (string * M.t) list
  let label = sprintf "%s string_map" M.label
  let coretype = Coretypes.Map.string_map M.coretype
  let testable = Alcotest.(list (pair string M.testable))
  let to_string =
    List.map (fun (k, v) -> sprintf "%s : %s" k (M.to_string v))
    &> String.concat "; "
    &> sprintf "{ %s }"
  let values = [
    M.values |> List.mapi (fun i v -> (sprintf "key%d" i, v))
  ]
end

module TestEnumDay : Testable = struct
  type tags = [
    | `monday
    | `tuesday
    | `wednesday
    | `thursday
    | `friday
    | `saturday
    | `sunday
  ] [@@ deriving show]

  type t = tags

  let label = "day_tag"

  let items =
    let open Type_desc.Coretype in
    [ `monday, string_enum_case "monday";
      `tuesday, string_enum_case "tuesday";
      `wednesday, string_enum_case "wednesday";
      `thursday, string_enum_case "thursday";
      `friday, string_enum_case "friday";
      `saturday, string_enum_case "saturday";
      `sunday, string_enum_case "sunday" ]

  let coretype = Coretypes.Enum.string_enum items
  let testable = Alcotest.testable pp_tags ( = )
  let to_string = Functionals.flip List.assoc items &> (fun (a, _, _) -> a)
  let values = items |&> fst
end

module TestEnumColor : Testable = struct
  type tags = [
    | `red
    | `green
    | `blue
    | `yellow
    | `black
    | `white
    | `gray
  ] [@@ deriving show]

  type t = tags

  let label = "color_tag"

  let items =
    let open Type_desc.Coretype in
    [ `red, string_enum_case "red";
      `green, string_enum_case "green";
      `blue, string_enum_case "blue";
      `yellow, string_enum_case "yellow";
      `black, string_enum_case "black";
      `white, string_enum_case "white";
      `gray, string_enum_case "gray" ]

  let coretype = Coretypes.Enum.string_enum items
  let testable = Alcotest.testable pp_tags ( = )
  let to_string = Functionals.flip List.assoc items &> (fun (a, _, _) -> a)
  let values = items |&> fst
end

let prims : (module Testable) list =
  [ (module TestUnit);
    (module TestBool);
    (module TestInt);
    (module TestInt53p);
    (module TestFloat);
    (module TestString);
    (module TestUchar);
    (module TestByte);
    (module TestBytes);
  ]

let enums : (module Testable) list =
  [ (module TestEnumDay);
    (module TestEnumColor) ]

let create_test_cases (module M : Testable) : string * unit Alcotest.test_case list =
  let open Alcotest in
  let refl = Coretypes.to_refl M.coretype in
  match Lazy.force refl with
  | Refl.Alias { get; mk } ->
    M.label, [
      test_case (M.label) `Quick (fun () ->
        M.values
        |> List.iter (fun v ->
          check' (option M.testable)
            ~msg:(M.to_string v)
            ~expected:(some v)
            ~actual:(v |> get |> mk)
        ))
    ]
  | _ -> failwith "Refl should be Alias."

let create_test_cases_of cases =
  let open Seq.Ops in
  let (@) = Seq.append in
  (cases |&> create_test_cases)
  @ (cases |&> (fun m -> create_test_cases(module TestOption(val m))))
  @ (cases |&> (fun m -> create_test_cases(module TestList(val m))))
  @ (cases |&> (fun m -> create_test_cases(module TestOption(TestList(val m)))))
  @ (cases |&> (fun m -> create_test_cases(module TestList(TestOption(val m)))))
  @ (cases |&> (fun m -> create_test_cases(module TestStringMap(val m))))
  @ (cases >>= (fun m0 ->
      cases |&> (fun m1 ->
        create_test_cases(module TestTup2(val m0)(val m1)))))
  @ (cases >>= (fun m0 ->
      cases >>= (fun m1 ->
        cases |&> (fun m2 ->
          create_test_cases(module TestTup3(val m0)(val m1)(val m2))))))
  @ (cases |&> (fun m -> create_test_cases(module TestTup2(TestOption(val m))(TestList(val m)))))
  @ (cases |&> (fun m -> create_test_cases(module TestTup3(TestOption(val m))(TestList(val m))(TestStringMap(val m)))))
  @ (cases >>= (fun m0 ->
      cases |&> (fun m1 ->
        create_test_cases(
          module TestTup2
            (TestOption(TestStringMap(TestOption(val m0))))
            (TestList(val m1))
        ))))

let () =
  let (@) = Seq.append in
  let prims_tests = create_test_cases_of (prims |> List.to_seq) in
  let enums_tests = create_test_cases_of (enums |> List.to_seq) in
  (prims_tests
   @ enums_tests
  ) |> List.of_seq
  |> Alcotest.run "lib_typedesc.coretypes"
