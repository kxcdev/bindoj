(* Copyright 2022 Kotoi-Xie Consultancy, Inc. This file is a part of the

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
open Bindoj_base
open Bindoj_typedesc.Type_desc
open Bindoj_gen_ts.Typescript_datatype
open Bindoj_test_common

let modules =
  let module ExG = Typedesc_generated_examples in
  ExG.all |&> (fun (name, (module G: ExG.T)) ->
    let gen () =
      let types = ref [ G.decl.td_name ] in
      StringMap.iter (fun name boxed ->
          let typed_decl = Typed_type_desc.Typed.unbox boxed in
          let decl = Typed_type_desc.Typed.decl typed_decl in
          if List.mem name !types then ()
          else begin
              print_endline
                (gen_ts_type ~export:false decl);
              refappend types name
            end)
        G.env.alias_ident_typemap;
      print_endline (gen_ts_type ~export:true G.decl);
      match G.decl.td_kind with
      | Variant_decl _ ->
         print_endline (gen_ts_case_analyzer ~export:true G.decl)
      | Record_decl _ -> ()
      | Alias_decl _ -> ()
    in
    name, gen)

let mapping =
  modules |> List.map (fun (s, m) -> sprintf "%s_gen.ts" s, m)

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [] | _ :: _ :: _ ->
    failwith "usage: gen <filename>"
  | [name] ->
    match List.assoc_opt name mapping with
    | None -> failwith (sprintf "unknown example %s" name)
    | Some gen -> gen ()
