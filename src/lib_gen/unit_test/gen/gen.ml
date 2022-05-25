(* Copyright 2022 Kotoi-Xie Consultancy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. *)

module type T = sig
  val name: string
  val gen:  unit -> unit
end

let modules : (string * (module T)) list = [
  "ex01", (module Ex01);
  "ex01_docstr", (module Ex01_docstr);
  "ex02", (module Ex02);
  "ex02_docstr", (module Ex02_docstr);
  "ex03", (module Ex03);
  "ex03_docstr", (module Ex03_docstr);
]

let mapping =
  modules |> List.map (fun (s, m) -> sprintf "%s_gen.ml" s, m)

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [] | _ :: _ :: _ ->
    failwith "usage: gen <filename>"
  | [name] ->
    match List.assoc_opt name mapping with
    | None -> failwith (sprintf "unknown example %s" name)
    | Some (module Ex : T) -> Ex.gen ()
