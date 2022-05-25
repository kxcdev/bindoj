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

type variant_type_flavor = [
  | `regular_variant_type (** the default *)
  | `polymorphic_variant_type
  (* | `extensible_variant_type (* future work *) *)
  ]

type ('pos, 'flavor) flavor_config +=
   | Flvconfig_variant_flavor :
       variant_type_flavor
       -> ([ `type_decl ], [ `variant_flavor ]) flavor_config

val type_declaration_of_type_decl :
  ?show:bool -> type_decl -> Ppxlib.type_declaration
