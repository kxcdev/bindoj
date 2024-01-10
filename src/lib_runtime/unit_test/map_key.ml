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
open Bindoj_runtime
open Alcotest

let map_key_values =
  let open Map_key in
  [
    "#str:0:", Mk_string "";
    "#str:4:hoge", Mk_string "hoge";
    "#str:6:?2E?3F", Mk_string ".?";
    "#i53p:0", Mk_int53 (Int53p.of_int 0);
    "#i53p:-1", Mk_int53 (Int53p.of_int (-1));
    "#i53p:42", Mk_int53 (Int53p.of_int 42);
    "#tup:0[]", Mk_tuple [];
    "#strenum:3:foo", Mk_string_enum "foo";
    "#dict:0{}", Mk_dictionary [];
    "#dict:3{a=#str:2:42,b=#i53p:123,c=#tup:0[]}", Mk_dictionary [
      "a", Mk_string "42";
      "b", Mk_int53 (Int53p.of_int 123);
      "c", Mk_tuple [];
    ];
  ] |> (fun values ->
    values @ (
      values |&>> (fun (expected, value) ->
        [ (sprintf "#tup:1[%s]" expected), Mk_tuple [ value ];
          (sprintf "#tup:2[%s,%s]" expected expected), Mk_tuple [ value; value ];
        ])
    )
    @ [ (sprintf "#tup:%d[%s]"
            (List.length values)
            (values |&> fst |> String.concat ",")),
          Mk_tuple (values |&> snd);
    ])

let encode_map_key_alcotest =
  test_case "is_valid_dictionary_key" `Quick (fun () ->
    map_key_values
    |!> (fun (expected, value) ->
      check' string
        ~msg:(sprintf "encoding_map_key (expected %s)" expected)
        ~expected ~actual:(Map_key.encode_map_key value)
    )
  )

let gen_valid =
  QCheck2.Gen.(string_of & oneof [
    oneofl [ '_'; '-' ];
    char_range '0' '9';
    char_range 'a' 'z';
    char_range 'A' 'Z';
  ])

let gen_invalid =
  QCheck2.Gen.(string_size ~gen:(
    oneofl & String.to_list "~`!@#$%^&*()+={}[]|\\:;\"'<>?,,./"
    ) (int_range 1 100))

let is_valid_dictionary_key_qcheck =
  let open QCheck2 in
  let gen =
    Gen.(oneof [
      gen_valid |> map (fun s -> `succ, s);
      gen_invalid |> map (fun s -> `fail, s);
      map2 (fun s1 s2 -> `fail, (s1^s2)) gen_valid gen_invalid;
    ])
  in
  [ Test.make ~count:2000 ~name:"Map_key.is_valid_dictionary_key" gen
      ~print:(!! (function
        | `succ -> sprintf "succ(%s)"
        | `fail -> sprintf "fail(%s)"))
      (fun (kind, value) ->
        Map_key.is_valid_dictionary_key value
          = (match kind with `succ -> true | `fail -> false)
      );
  ]

let () =
  let () =
    Log0.verbose ~modul:__FILE__ "backend: %a" Runtime_info.pp_runtime_type
      Runtime_info.current_runtime_type in
  Printexc.record_backtrace true;
  run "bindoj_runtime map_key tests"
    [ "tests of Map_key with Alcotest", [
      encode_map_key_alcotest
    ];
      "tests of Map_key with Qcheck", [
        is_valid_dictionary_key_qcheck
      ] |&>> (List.map QCheck_alcotest.to_alcotest);
    ]
