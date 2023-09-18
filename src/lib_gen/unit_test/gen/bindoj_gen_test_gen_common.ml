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
let generate ~formatter f decls =
  let rec loop = function
    | [] -> ()
    | hd :: tl ->
      (f hd) ~formatter;
      Format.pp_print_newline formatter ();
      if not (List.empty tl) then
        Format.pp_print_newline formatter ();
      loop tl
  in
  loop decls

let mapping =
  let open Bindoj_test_common_typedesc_examples in
  All.all
  |&>> (fun (name, (module Ex : Util.Ex)) -> [
    name, Ex.example_descs |&> (fun (module Desc : Util.Ex_desc) ->
      sprintf "%s.%s.decl" Ex.example_module_path Desc.module_name, Desc.decl);
    name ^ "_docstr", Ex.example_descs |&> (fun (module Desc : Util.Ex_desc) ->
      sprintf "%s.%s.decl_with_docstr" Ex.example_module_path Desc.module_name, Desc.decl_with_docstr) ])
  |&>> (fun (s, decls) -> [
    sprintf "%s_gen.ml" s, (`structure, decls);
    sprintf "%s_gen.mli" s, (`signature, decls) ])
