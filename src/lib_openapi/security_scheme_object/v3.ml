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
open Bindoj_openapi_util.V3

type t =
  | ApiKey of {
      description : string option;
      name : string;
      location : api_key_location;
    }
  | Http of {
      description : string option;
      scheme : string;
      bearerFormat : string option;
    }
  | Oauth2 of {
      description : string option;
      flows : oauth_flows_object
    }
  | OpenIdConnect of {
      description : string option;
      openIdConnectUrl : string option;
    } [@@ deriving show]

and api_key_location = Query | Header | Cookie

and oauth_flows_object = {
  implicit : implicit_oauth_flow_object option;
  password : password_oauth_flow_object option;
  clientCredentials : clientCredentials_oauth_flow_object option;
  authorizationCode : authorizationCode_oauth_flow_object option;
}

and implicit_oauth_flow_object = {
  i_authorizationUrl : string;
  i_refreshUrl : string option;
  i_scopes : (string * string) list;
}

and password_oauth_flow_object = {
  p_tokenUrl : string;
  p_refreshUrl : string option;
  p_scopes : (string * string) list;
}

and clientCredentials_oauth_flow_object = {
  c_tokenUrl : string;
  c_refreshUrl : string option;
  c_scopes : (string * string) list;
}

and authorizationCode_oauth_flow_object = {
  a_authorizationUrl : string;
  a_tokenUrl : string;
  a_refreshUrl : string option;
  a_scopes : (string * string) list;
}

let rec yojson_of_t : t -> yojson =
  let string x = `String x in
  let mk_field fname fval = [(fname, fval)] in
  let mk_option ~cont fname fval = match fval with
    | None -> []
    | Some x -> [(fname, cont x)] in
  let mk_type x = mk_field "type" (`String x) in
  let mk_desc x = mk_option ~cont:string "description" x in
  function
  | ApiKey { description; name; location; } ->
    let type_ = mk_type "apiKey" in
    let desc = mk_desc description in
    let name = mk_field "name" (`String name) in
    let location = mk_field "in" (yojson_of_api_key_location location) in
    `Assoc (type_ @ desc @ name @ location)
  | Http { description; scheme; bearerFormat; } ->
    let type_ = mk_type "http" in
    let desc = mk_desc description in
    let scm = mk_field "scheme" (`String scheme) in
    let brfmt = mk_option ~cont:string "bearerFormat" bearerFormat in
    `Assoc (type_ @ desc @ scm @ brfmt)
  | Oauth2 { description; flows={ implicit; password; clientCredentials; authorizationCode; } } ->
    let type_ = mk_type "oath2" in
    let desc = mk_desc description in
    let implicit =
      mk_option ~cont:(fun { i_authorizationUrl; i_refreshUrl; i_scopes; } ->
          let authorizationUrl = mk_field "authorizationUrl" (`String i_authorizationUrl) in
          let refreshUrl = mk_option ~cont:string "refreshUrl" i_refreshUrl in
          let scopes = mk_field "scopes" (`Assoc (i_scopes |&> fun (k, v) -> (k, `String v))) in
          `Assoc (authorizationUrl @ refreshUrl @ scopes))
        "implicit" implicit in
    let password =
      mk_option ~cont:(fun { p_tokenUrl; p_refreshUrl; p_scopes; } ->
          let tokenUrl = mk_field "tokenUrl" (`String p_tokenUrl) in
          let refreshUrl = mk_option ~cont:string "refreshUrl" p_refreshUrl in
          let scopes = mk_field "scopes" (`Assoc (p_scopes |&> fun (k, v) -> (k, `String v))) in
          `Assoc (tokenUrl @ refreshUrl @ scopes))
        "password" password in
    let clientCredentials =
      mk_option ~cont:(fun { c_tokenUrl; c_refreshUrl; c_scopes; } ->
          let tokenUrl = mk_field "tokenUrl" (`String c_tokenUrl) in
          let refreshUrl = mk_option ~cont:string "refreshUrl" c_refreshUrl in
          let scopes = mk_field "scopes" (`Assoc (c_scopes |&> fun (k, v) -> (k, `String v))) in
          `Assoc (tokenUrl @ refreshUrl @ scopes))
        "clientCredentials" clientCredentials in
    let authorizationCode =
      mk_option ~cont:(fun { a_authorizationUrl; a_tokenUrl; a_refreshUrl; a_scopes; } ->
          let authorizationUrl = mk_field "authorizationUrl" (`String a_authorizationUrl) in
          let tokenUrl = mk_field "tokenUrl" (`String a_tokenUrl) in
          let refreshUrl = mk_option ~cont:string "refreshUrl" a_refreshUrl in
          let scopes = mk_field "scopes" (`Assoc (a_scopes |&> fun (k, v) -> (k, `String v))) in
          `Assoc (authorizationUrl @ tokenUrl @ refreshUrl @ scopes))
        "authorizationCode" authorizationCode in
    `Assoc (type_ @ desc @ implicit @ password @ clientCredentials @ authorizationCode)
  | OpenIdConnect { description; openIdConnectUrl; } ->
    let type_ = mk_type "openIdConnect" in
    let desc = mk_desc description in
    let openIdConnectUrl = mk_option ~cont:string "openIdConnectUrl" openIdConnectUrl in
    `Assoc (type_ @ desc @ openIdConnectUrl)

and yojson_of_api_key_location : api_key_location -> yojson = function
  | Query -> `String "query"
  | Header -> `String "header"
  | Cookie -> `String "cookie"

let apiKey ?description name location =
  ApiKey {
    name = name;
    location = location;
    description = description;
  }

let http ?description ?bearerFormat scheme =
  Http {
    description = description;
    scheme = scheme;
    bearerFormat = bearerFormat;
  }

let oauth2 ?description flows =
  Oauth2 {
    description = description;
    flows = flows;
  }

and flows ?implicit ?password ?clientCredentials ?authorizationCode () = {
  implicit = implicit;
  password = password;
  clientCredentials = clientCredentials;
  authorizationCode = authorizationCode;
}

and implicit_flow ?refreshUrl scopes authorizationUrl =
  { i_authorizationUrl = authorizationUrl;
    i_refreshUrl = refreshUrl;
    i_scopes = scopes; }

and password_flow ?refreshUrl scopes tokenUrl =
  { p_tokenUrl = tokenUrl;
    p_refreshUrl = refreshUrl;
    p_scopes = scopes; }

and clientCredentials_flow ?refreshUrl scopes tokenUrl =
  { c_tokenUrl = tokenUrl;
    c_refreshUrl = refreshUrl;
    c_scopes = scopes; }

and authorizationCode_flow ?refreshUrl scopes authorizationUrl tokenUrl =
  { a_authorizationUrl = authorizationUrl;
    a_tokenUrl = tokenUrl;
    a_refreshUrl = refreshUrl;
    a_scopes = scopes; }

let openIdConnect ?description ?openIdConnectUrl () =
  OpenIdConnect {
    description = description;
    openIdConnectUrl = openIdConnectUrl;
  }

let to_json : t -> jv = fun t ->
  t |> yojson_of_t |> Json.of_yojson
