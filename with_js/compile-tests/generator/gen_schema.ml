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
open Bindoj_test_common.Typedesc_examples
open Bindoj_test_common.Typedesc_examples.Util
open Bindoj_gen
open Bindoj_openapi.V3

let print_json (module E : Ex) =
  E.example_descs
  |&> (fun (module D : Ex_desc) ->
    D.decl_with_docstr
    |> Json_codec.gen_json_schema
    |> Schema_object.to_json)
  |> fun jv -> `arr jv
  |> Json.to_yojson
  |> Yojson.Safe.to_string
  |> print_endline

let mapping =
  all |> List.map (fun (s, m) -> sprintf "%s_schema.json" s, m)

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [] | _ :: _ :: _ ->
    failwith "usage: gen_schema <filename>"
  | [name] ->
    match List.assoc_opt name mapping with
    | None -> failwith (sprintf "unknown example %s" name)
    | Some m -> print_json m
