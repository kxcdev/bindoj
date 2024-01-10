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
module Bindoj_list = struct
  include Kxclib.List
  (* awaiting https://github.com/kxcinc/kxclib-ocaml/projects/1#card-91408419 *)
  let uniq_by f =
    let rec go acc = function
      | [] -> acc |&> snd
      | x :: xs ->
        let k = f x in
        match List.assoc_opt k acc with
        | None -> go ((k, x) :: acc) xs
        | Some _ -> go acc xs
    in
    go [] &> List.rev
  
  let skip_tail_while, skip_while =
    let rec go f acc = function
      | [] -> acc
      | x :: xs when f x -> go f acc xs
      | x :: xs -> go f (x :: acc) xs
    in
    (fun f xs -> go f [] (List.rev xs)),
    (fun f -> go f [] &> List.rev)
end

module Mangling = struct
  let charmap_js_identifier = function
    | '\'' -> Some "$PRIME$"
    | _ -> None

  let escape : charmap:(char -> string option) -> string -> string =
    fun ~charmap s ->
      let rec go_rev =
        fun acc -> function
        | [] -> acc
        | h :: t ->
          begin match charmap h with
            | None -> go_rev (h :: acc) t
            | Some escaped -> go_rev ((String.to_list escaped) @ acc) t
          end
      in
      let chars = s |> String.to_list in
      if List.exists (charmap &> Option.is_some) chars then
        chars |> List.rev |> go_rev [] |> String.of_list
      else s

  let numeric = String.for_all (function '0' .. '9' -> true | _ -> false)
  let version_head ?(case=(`any : [ `lower | `upper | `any ])) =
    match case with
    | `any -> (
      String.partition_opt 1 &> function
      | Some (("V" | "v"), n) when numeric n -> true
      | _ -> false)
    | `lower -> (
      String.partition_opt 1 &> function
      | Some ("v", n) when numeric n -> true
      | _ -> false)
    | `upper -> (
      String.partition_opt 1 &> function
      | Some ("V", n) when numeric n -> true
      | _ -> false)

  (* e.g.
    [partition_snake "v1_2_foo_bar" = [`v ["1"; "2"]; `n "foo"; `n "bar"]]
    [partition_snake "foo_v1_2_bar" = [`n "foo"; `v ["1"; "2"]; `n "bar"]]
    [partition_snake "foo_bar_v1_2" = [`n "foo"; `n "bar"; `v ["1"; "2"];]]
    [partition_snake "V1_2_foo_bar" = [`v ["1"; "2"]; `n "foo"; `n "bar"]]
    *)
  let partition_snake ?(f=identity) : string list -> [
    | `n of string (* normal component *)
    | `v of string (* numeric string *) list (* version string component *)
    ] list =
    let rec go_rev acc cacc rest = match cacc, rest with
      | [], [] -> acc
      | v, [] -> `v (List.rev v) :: acc
      | (_ :: _ as v), (next :: rest) ->
        if numeric next then go_rev acc (next :: cacc) rest
        else go_rev (`n (f next) :: `v (List.rev v) :: acc) [] rest
      | [], (next :: rest) ->
        if version_head next
        then go_rev acc [String.partition 1 next |> snd] rest
        else go_rev (`n (f next) :: acc) [] rest
    in
    go_rev [] [] &> List.rev

  let with_partition_snake ~preserve_version_substring ~f ~add_version_head =
    String.split_on_char '_'
    &> (
      if preserve_version_substring then
        partition_snake ~f &> List.map add_version_head
      else List.map f
    )

  let snake_to_upper_camel ?(preserve_version_substring = true) snake_str =
    snake_str
    |> with_partition_snake
      ~preserve_version_substring
      ~f:String.capitalize_ascii
      ~add_version_head:(function
      | `n comp -> comp
      | `v v -> "V"^String.(concat "_" v))
    |> String.concat ""

  let snake_to_lower_camel ?preserve_version_substring snake_str =
    match snake_to_upper_camel ?preserve_version_substring snake_str
          |> String.partition_opt 1 with
    | None -> ""
    | Some (h, rest) -> String.lowercase_ascii h ^ rest

  let snake_to_kebab' ?(f=identity) ?(preserve_version_substring = true) snake_str =
    snake_str
    |> with_partition_snake
      ~preserve_version_substring
      ~f
      ~add_version_head:(function
      | `n comp -> comp
      | `v v -> "v"^String.(concat "_" v))
    |> String.concat "-"

  let snake_to_kebab ?preserve_version_substring snake_str =
    snake_to_kebab' ?preserve_version_substring snake_str

  let cap_snake_to_kebab ?preserve_version_substring cap_snake_str =
    snake_to_kebab' ~f:String.lowercase_ascii ?preserve_version_substring cap_snake_str
end
