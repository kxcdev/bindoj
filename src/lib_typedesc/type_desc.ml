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

type codec = [ `default_codec | `codec_val of string | `codec_in_module of string ]
type 'x with_docstr = 'x*[ `docstr of string | `nodoc ]

type ('pos, 'flavor) flavor_config = ..

module FlavorConfigs = struct
  type 'pos t = [] : 'pos t | (::) : (('pos, _) flavor_config * 'pos t) -> 'pos t

  type ('pos, 'flavor) flavor_config +=
    | Flvconfig_dummy : ('pos, 'flavor) flavor_config

  let find : (('pos, 'flavor) flavor_config -> 'a option) -> 'pos t -> 'a option =
    fun finder configs ->
    let rec go : 'pos t -> 'a option = function
      | [] -> None
      | flavor :: rest ->
        match finder (Obj.magic flavor) with
        | None -> go rest
        | Some v -> Some v
    in
    go configs

  let find_or_default : default:'a -> (('pos, 'flavor) flavor_config -> 'a option) -> 'pos t -> 'a =
    fun ~default finder configs -> find finder configs |> Option.value ~default

  let get : ?default:('pos, 'flavor) flavor_config -> (('pos, 'flavor) flavor_config -> bool) -> 'pos t -> ('pos, 'flavor) flavor_config =
    fun ?(default = Flvconfig_dummy) pred configs ->
    let rec go : 'pos t -> _ = function
      | [] -> default
      | flavor :: rest ->
        let flavor = Obj.magic flavor in
        if pred flavor then flavor else go rest
    in
    go configs

end
type 'pos flavor_configs = 'pos FlavorConfigs.t

type record_type_desc = record_field_desc with_docstr list
and record_field_desc = {
  rf_name : string;
  rf_type : string;
  rf_codec : codec;
}

type variant_type_desc = variant_constructor_desc with_docstr list
and variant_constructor_desc =
  | Cstr_tuple of {
      ct_name : string;
      ct_args : string list;
      ct_codec : codec;
      ct_flvconfigs : [ `branch ] flavor_configs;
    }
  | Cstr_record of {
      cr_name : string;
      cr_fields : record_type_desc;
      cr_codec : codec;
      cr_flvconfigs : [ `branch ] flavor_configs;
    }

type generic_kind =
  | Record_kind of record_type_desc
  | Variant_kind of variant_type_desc

type type_decl = {
  td_name : string;
  td_kind : generic_kind with_docstr;
  td_flvconfigs: [ `type_decl ] flavor_configs;
}
