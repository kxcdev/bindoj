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
open Ppx_yojson_conv_lib.Yojson_conv

let schema = "https://json-schema.org/draft/2020-12/schema"

type yojson = Kxclib.Json.yojson
let yojson_of_yojson : yojson -> yojson = identity

type 'a assoc = (string * 'a) list
let yojson_of_assoc yojson_of_a fields : yojson =
  `Assoc (fields |&> fun (k, v) -> (k, yojson_of_a v))

type 't or_false = [`T of 't | `False]
let yojson_of_or_false yojson_of_t x : yojson =
  match x with
  | `T t -> yojson_of_t t
  | `False -> yojson_of_bool false

(* JSON Schema draft 2020-12 + OpenAPI Schema Object vocabulary *)

(* https://spec.openapis.org/oas/latest.html#discriminator-object *)
type discriminator = {
  propertyName: string;
  mapping: (string * string) list
}

(* https://spec.openapis.org/oas/latest.html#external-documentation-object *)
type externalDocs = {
  ed_description: string option [@name "description"] [@yojson.option];
  ed_url: string [@name "url"];
} [@@deriving yojson_of]

let yojson_of_discriminator (d: discriminator) : yojson =
  let base = ["propertyName", yojson_of_string d.propertyName] in
  if List.empty d.mapping then
    `Assoc base
  else
    let mapping =
      d.mapping |> List.map (fun (name, value) -> name, yojson_of_string value)
    in
    `Assoc (base @ ["mapping", `Assoc mapping])

(* https://json-schema.org/understanding-json-schema/reference/generic.html *)
type generic_fields = {
  title: string option [@yojson.option];
  description: string option [@yojson.option];
  default: yojson option [@yojson.option];
  examples: yojson list option [@yojson.option];
  enum: yojson list option [@yojson.option];
  const: yojson option [@yojson.option];
  deprecated: bool option [@yojson.option];
  readOnly: bool option [@yojson.option];
  writeOnly: bool option [@yojson.option];

  (* OpenAPI extensions *)
  (* https://spec.openapis.org/oas/latest.html#fixed-fields-19 *)
  discriminator: discriminator option [@yojson.option];
  externalDocs: externalDocs option [@yojson.option];
} [@@deriving yojson_of]

(* https://json-schema.org/understanding-json-schema/reference/string.html#built-in-formats *)
type string_format = [
  (* dates and times *)
  | `date_time | `time | `date | `duration
  (* email addresses *)
  | `email | `idn_email
  (* host names *)
  | `hostname | `idn_hostname
  (* IP addresses *)
  | `ipv4 | `ipv6
  (* resource identifiers *)
  | `uuid | `uri | `uri_reference | `iri | `iri_reference
  (* URI template *)
  | `uri_template
  (* regex *)
  | `regex
  (* OpenAPI extensions: https://spec.openapis.org/oas/v3.1.0#data-types *)
  | `password
]
let yojson_of_string_format (sf: string_format) =
  let str =
    match sf with
    | `date_time -> "date_time" | `time -> "time" | `date -> "date" | `duration -> "duration"
    | `email -> "email" | `idn_email -> "idn-email"
    | `hostname -> "hostname" | `idn_hostname -> "idn-hostname"
    | `ipv4 -> "ipv4" | `ipv6 -> "ipv6"
    | `uuid -> "uuid" | `uri -> "uri" | `uri_reference -> "uri-reference" | `iri -> "iri" | `iri_reference -> "iri-reference"
    | `uri_template -> "uri-template"
    | `regex -> "regex"
    | `password -> "password"
  in `String str

(* https://json-schema.org/understanding-json-schema/reference/string.html *)
type string_fields = {
  minLength: int option [@yojson.option];
  maxLength: int option [@yojson.option];
  pattern: string option [@yojson.option];
  format: string_format option [@yojson.option];
} [@@deriving yojson_of]

(* https://json-schema.org/understanding-json-schema/reference/numeric.html *)
type 'a number_base = {
  multipleOf: 'a option [@yojson.option];
  minimum: 'a option [@yojson.option];
  maximum: 'a option [@yojson.option];
  exclusiveMinimum: 'a option [@yojson.option];
  exclusiveMaximum: 'a option [@yojson.option];
} [@@deriving yojson_of]
and integer_fields = int number_base [@@deriving yojson_of]
and number_fields = float number_base [@@deriving yojson_of]

(* https://json-schema.org/understanding-json-schema/reference/array.html *)
type 't array_fields = {
  items: 't or_false option [@yojson.option];
  prefixItems: 't list option [@yojson.option];
  minItems: int option [@yojson.option];
  maxItems: int option [@yojson.option];
  (* `contains` is not supported *)
} [@@deriving yojson_of]

(* https://json-schema.org/understanding-json-schema/reference/object.html *)
and 't object_fields = {
  properties: 't assoc option [@yojson.option];
  required: string list option [@yojson.option];
  additionalProperties: 't or_false option [@yojson.option];
  (* patternProperties: 't assoc option [@yojson.option]; *) (* TODO #125: extend map_key *)
} [@@deriving yojson_of]

type 't typ =
  | Ref of string
  | String of string_fields
  | Integer of integer_fields
  | Number of number_fields
  | Boolean
  | Null
  | Array of 't array_fields
  | Object of 't object_fields
  | AllOf of 't list (* AND *)
  | AnyOf of 't list (* OR *)
  | OneOf of 't list (* XOR *)
  | Not of 't        (* NOT *)
  | Any (* matches everything: https://json-schema.org/understanding-json-schema/basics.html#hello-world *)

type t = {
  schema: string option;
  generic_fields: generic_fields;
  structuring_fields: structuring_fields;
  typ: t typ
}

and structuring_fields = {
  (** https://json-schema.org/understanding-json-schema/structuring.html#id *)
  id: string option;

  (** https://json-schema.org/understanding-json-schema/structuring.html#anchor *)
  anchor: string option;

  (** https://json-schema.org/understanding-json-schema/structuring.html#defs *)
  defs: t assoc option;
}

let field_opt yojson_of_t name topt =
  match topt with
  | None -> []
  | Some t -> [name, yojson_of_t t]

let rec yojson_of_t (t: t) : yojson =
  let schema = t.schema |> Option.map (fun s -> ["$schema", yojson_of_string s]) |? [] in
  let generic_fields = yojson_of_generic_fields t.generic_fields in
  let structuring_fields = yojson_of_structuring_fields t.structuring_fields in
  match generic_fields, structuring_fields with
  | `Assoc xs, `Assoc ys ->
    let fields = schema @ xs @ ys @ typ_to_fields t.typ in
    (*
      https://json-schema.org/understanding-json-schema/basics.html#hello-world
      New in draft 6: You can also use true in place of the empty object to
      represent a schema that matches anything, or false for a schema that
      matches nothing.
    *)
    begin match t.typ, fields with
    | Any, [] -> `Bool true
    | Not { typ = Any; _ }, [] -> `Bool false
    | _, _ -> `Assoc (schema @ xs @ ys @ typ_to_fields t.typ)
    end
  | _ -> failwith "impossible"

and yojson_of_structuring_fields (x: structuring_fields) : yojson =
  `Assoc (
      field_opt yojson_of_string "$id" x.id
    @ field_opt yojson_of_string "$anchor" x.anchor
    @ field_opt (yojson_of_assoc yojson_of_t) "$defs" x.defs
  )

and typ_to_fields (t: t typ) : (string * yojson) list =
  let str = yojson_of_string in
  let fields = function
    | `Assoc fields -> fields
    | _ -> failwith "impossible"
  in
  match t with
  | Ref s -> ["$ref", yojson_of_string s]
  | String x -> ["type", str "string"] @ fields (yojson_of_string_fields x)
  | Integer x -> ["type", str "integer"] @ fields (yojson_of_integer_fields x)
  | Number x -> ["type", str "number"] @ fields (yojson_of_number_fields x)
  | Boolean -> ["type", str "boolean"]
  | Null -> ["type", str "null"]
  | Array x -> ["type", str "array"] @ fields (yojson_of_array_fields yojson_of_t x)
  | Object x -> ["type", str "object"] @ fields (yojson_of_object_fields yojson_of_t x)
  | AllOf ts -> ["allOf", yojson_of_list yojson_of_t ts]
  | AnyOf ts -> ["anyOf", yojson_of_list yojson_of_t ts]
  | OneOf ts -> ["oneOf", yojson_of_list yojson_of_t ts]
  | Not t -> ["not", yojson_of_t t]
  | Any -> []

let mk
  ?schema
  ?title ?description
  ?default ?examples
  ?enum ?const
  ?deprecated
  ?readOnly ?writeOnly
  ?id ?anchor ?defs
  ?discriminator
  ?externalDocs
  ~f =
  let conv = Option.map Json.to_yojson in
  let convMany = Option.map (List.map Json.to_yojson) in
  f (fun typ -> {
    schema;
    generic_fields = {
      title; description;
      default = conv default; examples = convMany examples;
      enum = convMany enum; const = conv const;
      deprecated;
      readOnly; writeOnly;
      discriminator;
      externalDocs
    };
    structuring_fields = {
      id; anchor; defs;
    };
    typ;
  })

let externalDocs ?description url =
  { ed_description = description; ed_url = url }

let discriminator ?mapping propertyName =
  { propertyName; mapping = Option.value ~default:[] mapping }

module TypImpl = struct
  let ref s ~cont = Ref s |> cont

  let string ?minLength ?maxLength ?pattern ?format ~cont () =
    String { minLength; maxLength; pattern; format } |> cont

  let integer ?multipleOf ?minimum ?maximum ?exclusiveMinimum ?exclusiveMaximum ~cont () =
    Integer { multipleOf; minimum; maximum; exclusiveMinimum; exclusiveMaximum } |> cont

  let number ?multipleOf ?minimum ?maximum ?exclusiveMinimum ?exclusiveMaximum ~cont () =
    Number { multipleOf; minimum; maximum; exclusiveMinimum; exclusiveMaximum } |> cont

  let boolean ~cont () = Boolean |> cont

  let null ~cont () = Null |> cont

  let array ?items ?prefixItems ?minItems ?maxItems ~cont () =
    Array { items; prefixItems; minItems; maxItems } |> cont

  let obj ?properties ?required ?additionalProperties ~cont () =
    Object { properties; required; additionalProperties } |> cont

  let allOf ts ~cont = AllOf ts |> cont
  let anyOf ts ~cont = AnyOf ts |> cont
  let oneOf ts ~cont = OneOf ts |> cont
  let not t ~cont = Not t |> cont
  let any () ~cont = cont Any
end

let ref = mk ~f:(fun cont s -> TypImpl.ref s ~cont)
let string = mk ~f:(fun cont -> TypImpl.string ~cont)
let integer = mk ~f:(fun cont -> TypImpl.integer ~cont)
let number = mk ~f:(fun cont -> TypImpl.number ~cont)
let boolean = mk ~f:(fun cont -> TypImpl.boolean ~cont)
let null = mk ~f:(fun cont -> TypImpl.null ~cont)
let array = mk ~f:(fun cont -> TypImpl.array ~cont)
let obj = mk ~f:(fun cont -> TypImpl.obj ~cont)
let allOf = mk ~f:(fun cont -> TypImpl.allOf ~cont)
let anyOf = mk ~f:(fun cont -> TypImpl.anyOf ~cont)
let oneOf = mk ~f:(fun cont -> TypImpl.oneOf ~cont)
let not = mk ~f:(fun cont -> TypImpl.not ~cont)
let any = mk ~f:(fun cont -> TypImpl.any ~cont)

(* Helper to express `never` *)
let never = mk ~f:(fun cont () -> TypImpl.not ~cont (any ()))

(* OCaml helpers *)
let tuple =
  mk ~f:(fun cont ts ->
    let len = List.length ts in
    TypImpl.array ~prefixItems:ts ~minItems:len ~maxItems:len ~cont ()
  )

let record =
  mk ~f:(fun cont ?(additionalProperties=`False) properties ->
    let rec is_nullable t = match t.typ with
      | Null -> true
      | AllOf ts -> List.for_all is_nullable ts
      | AnyOf ts -> List.exists is_nullable ts
      | OneOf ts -> List.exists is_nullable ts
      | Not t -> Bool.not (is_nullable t)
      | Any -> true
      | _ -> false in
    let required =
      properties |> List.filter_map (fun (k, t) ->
        if is_nullable t then none
        else some k
      ) in
    TypImpl.obj ~properties ~required ~additionalProperties ~cont ()
  )

let option t = oneOf [t; null ()]

let rec map_ref (f: string -> string) (t: t) : t =
  let map_t_or_false = Option.map (function `T t -> `T (map_ref f t) | `False -> `False) in
  let map = function
    | Ref s -> Ref (f s)
    | Array x ->
      Array { x with
        items = map_t_or_false x.items;
        prefixItems = Option.map (List.map (map_ref f)) x.prefixItems
      }
    | Object x ->
      Object { x with
        properties = Option.map (List.map (fun (k, v) -> k, map_ref f v)) x.properties;
        additionalProperties = map_t_or_false x.additionalProperties;
      }
    | AllOf ts -> AllOf (List.map (map_ref f) ts)
    | AnyOf ts -> AnyOf (List.map (map_ref f) ts)
    | OneOf ts -> OneOf (List.map (map_ref f) ts)
    | Not t -> Not (map_ref f t)
    | (String _ | Integer _ | Number _ | Boolean | Null) as x -> x
    | Any -> Any
  in
  { t with
    typ = map t.typ;
    generic_fields = { t.generic_fields with
      discriminator = t.generic_fields.discriminator |> Option.map (fun x ->
        { x with mapping = x.mapping |&> fun (k, v) -> k, f v });
    };
    structuring_fields = { t.structuring_fields with
      defs = t.structuring_fields.defs |> Option.map (List.map (fun (k, v) -> k, map_ref f v;))
    }
  }

let to_json (t: t) =
  t
  |> yojson_of_t
  |> Json.of_yojson
