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
open Bindoj_codec
open Bindoj_apidir_shared

module Ttd = Bindoj_typedesc.Typed_type_desc

type resolution_strategy = [
  | `no_resolution
  | `import_location of string
  | `inline_type_definition
  | `infile_type_definition of [ `export | `no_export ]
]

let box ttd = Ttd.Boxed ttd

let gen_raw :
    ?resolution_strategy:(Ttd.boxed_type_decl -> resolution_strategy)
    -> ?bindoj_namespace:string
    -> ?mod_prefix:string
    -> mod_name:string
    -> registry_info
    -> string
  = fun ?resolution_strategy ?bindoj_namespace ?mod_prefix ~mod_name (invps, _) ->
  let mod_prefixed s = match mod_prefix with
    | Some p -> p^"_"^s
    | None -> String.capitalize_ascii mod_name^s in
  let bindoj_prefixed s = match bindoj_namespace with
    | Some ns -> ns^"."^s
    | None -> invalid_arg' "?bindoj_namespace is not specified" in
  let open Bindoj_gen_ts.Typescript_datatype in
  let open Bindoj_typedesc.Typed_type_desc in
  let resolution_strategy =
    resolution_strategy |? (constant `no_resolution)
  in
  let reqtype invp =
    invp.ip_request_body
    >? (fun r -> box r.rq_media_type.mt_type) in
  let resptypes invp =
    invp.ip_responses |&> (
           function
           | Response_case { response = r; _ } ->
              box r.rs_media_type.mt_type) in
  let imports =
    invps |> List.fmap (fun (Invp invp) ->
      (reqtype invp |> Option.to_list)
      @ (resptypes invp))
    |> List.rev
  in
  let import_statements =
    imports
    |> List.group_by resolution_strategy
    |&?> (function | (`import_location loc, tds) -> Some (loc, tds) | _ -> None)
    |&> (fun (loc, tds) ->
      let tnames =
        tds
        |&> (fun (Ttd.Boxed (module T)) -> Json.Json_config.get_mangled_name_of_type T.decl)
        |> List.sort_uniq compare
      in
      sprintf "import { %s } from \"%s\";"
          (String.concat ", " tnames)
          loc
    ) in
  let infile_type_definition_statements =
      imports
      |> List.sort_uniq compare
      |&?> (fun td ->
        match resolution_strategy td with
        | `infile_type_definition modifier ->
          let decl = td |> Typed.unbox |> Typed.decl in
          Some (gen_ts_type ~export:(modifier = `export) decl)
        | _ -> None)
  in
  let make_ts_type_desc (Ttd.Boxed (module T) as typ): ts_type_desc =
    let { td_name; td_configs; _ } = T.decl in
    let json_name = Json.Json_config.get_name_opt td_configs |? td_name in
    match resolution_strategy typ with
    | `inline_type_definition ->
      let () = if is_recursive T.decl then
        failwith' "%s - Recursive type '%s' cannot be defined inline." __FILE__ td_name
      in
      T.decl
      |> ts_fwrt_decl_of_type_decl ~export:false ~readonly:false
      |> ts_type_alias_decl_of_fwrt_decl ~self_type_name:json_name
      |> fun t -> t.tsa_type_desc
    | _ -> `type_reference (json_name |> Json.Json_config.mangled `type_name td_configs)
  in
  let typescript_resptypes (Invp invp) : ts_type_desc =
    let branches = resptypes invp |&> make_ts_type_desc in
    `union branches
  in
  let typescript_reqtype (Invp invp) : ts_type_desc =
    reqtype invp >? make_ts_type_desc |? `special `undefined
  in
  let invp_info_object: ts_expression =
    let objexpr fs : ts_expression = `literal_expression (`object_literal fs) in
    let litstr x : ts_expression = `literal_expression (`string_literal x) in
    let type_designator : ts_type_desc -> ts_expression = fun td ->
      `casted_expression (
          (`casted_expression (
             `identifier "undefined",
             `special `unknown)),
          td) in
    invps |&> (fun (Invp invp as invp') ->
      let endp_name = invp.ip_name in
      let endp_urlpath = invp.ip_urlpath |> litstr in
      let endp_method = litstr @@ match invp.ip_method with
        | `get -> "GET" | `post -> "POST" in
      let resp_type_entry = "resp_type", typescript_resptypes invp' |> type_designator in
      endp_name,
      objexpr ([
          "name", litstr endp_name;
          "method", endp_method;
          "urlpath", endp_urlpath;
        ] |> (match invp.ip_method with
              | `get ->
                 Fn.flip List.append [
                     resp_type_entry;
                   ]
              | `post ->
                 Fn.flip List.append [
                     "req_type", typescript_reqtype invp' |> type_designator;
                     resp_type_entry;
                   ]
        ))
    ) |> objexpr
  in
  let invp_info_object =
    `value_declaration {
        tsv_modifiers = [ `export ];
        tsv_kind = `const;
        tsv_name = mod_prefixed "InvpInfo";
        tsv_type_desc = None;
        tsv_value =
          `const_assertion invp_info_object
      } in
  let ast =
    match bindoj_namespace with
    | None -> [ invp_info_object ]
    | Some _ -> begin
        let invp_info_map_type: ts_statement =
          `type_alias_declaration {
              tsa_modifiers = [`export];
              tsa_name = mod_prefixed "InvpInfoMap";
              tsa_type_parameters = [];
              tsa_type_desc =
                `type_construct (
                    bindoj_prefixed "IsApiDirInfoMap",
                    [`typeof (`identifier (mod_prefixed "InvpInfo"))]);
            }
        in
        let invp_client_intf: ts_statement =
          `type_alias_declaration {
              tsa_modifiers = [`export];
              tsa_name = mod_prefixed "ClientIntf";
              tsa_type_parameters = [];
              tsa_type_desc =
                `type_construct (
                    bindoj_prefixed "ApiDirClientPromiseIntf",
                    [`type_reference (mod_prefixed "InvpInfoMap")]);
            }
        in
        [ invp_info_object; invp_info_map_type; invp_client_intf; ]
      end in
  let statements =
    ast
    |> Internals.rope_of_ts_ast
    |> Rope.to_string in
  (String.concat "\n" import_statements)
  ^"\n"
  ^(String.concat "\n" infile_type_definition_statements)
  ^"\n"
  ^statements

