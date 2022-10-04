[@@@ocaml.warning "-7-11-32-33-39"]
[@@@js.implem
  [@@@ocaml.warning "-7-11-32-33-39"]
]
[@@@js.scope "jsonschema"]

open Ts2ocaml
module rec AnonymousInterface5 : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val get: t -> string -> Schema.t [@@js.index_get]
  val set: t -> string -> Schema.t -> unit [@@js.index_set]
end
and AnonymousInterface6 : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val get: t -> string -> (string list, Schema.t) union2 [@@js.index_get]
  val set: t -> string -> ([`U1 of Schema.t | `U2 of string list] [@js.union]) -> unit [@@js.index_set]
end
and Schema : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val get__id: t -> string [@@js.get "$id"]
  val set__id: t -> string -> unit [@@js.set "$id"]
  val get_id: t -> string [@@js.get "id"]
  val set_id: t -> string -> unit [@@js.set "id"]
  val get__schema: t -> string [@@js.get "$schema"]
  val set__schema: t -> string -> unit [@@js.set "$schema"]
  val get__ref: t -> string [@@js.get "$ref"]
  val set__ref: t -> string -> unit [@@js.set "$ref"]
  val get_title: t -> string [@@js.get "title"]
  val set_title: t -> string -> unit [@@js.set "title"]
  val get_description: t -> string [@@js.get "description"]
  val set_description: t -> string -> unit [@@js.set "description"]
  val get_multipleOf: t -> float [@@js.get "multipleOf"]
  val set_multipleOf: t -> float -> unit [@@js.set "multipleOf"]
  val get_maximum: t -> float [@@js.get "maximum"]
  val set_maximum: t -> float -> unit [@@js.set "maximum"]
  val get_exclusiveMaximum: t -> ([`Number of float | `Boolean of bool] [@js.union on_field "dummy"]) Primitive.t [@@js.get "exclusiveMaximum"]
  val set_exclusiveMaximum: t -> ([`U1 of float | `U2 of bool] [@js.union]) -> unit [@@js.set "exclusiveMaximum"]
  val get_minimum: t -> float [@@js.get "minimum"]
  val set_minimum: t -> float -> unit [@@js.set "minimum"]
  val get_exclusiveMinimum: t -> ([`Number of float | `Boolean of bool] [@js.union on_field "dummy"]) Primitive.t [@@js.get "exclusiveMinimum"]
  val set_exclusiveMinimum: t -> ([`U1 of float | `U2 of bool] [@js.union]) -> unit [@@js.set "exclusiveMinimum"]
  val get_maxLength: t -> float [@@js.get "maxLength"]
  val set_maxLength: t -> float -> unit [@@js.set "maxLength"]
  val get_minLength: t -> float [@@js.get "minLength"]
  val set_minLength: t -> float -> unit [@@js.set "minLength"]
  val get_pattern: t -> ([`String of string | `Other of regexp] [@js.union on_field "dummy"]) Primitive.t [@@js.get "pattern"]
  val set_pattern: t -> ([`U1 of string | `U2 of regexp] [@js.union]) -> unit [@@js.set "pattern"]
  val get_additionalItems: t -> ([`Boolean of bool | `Other of t] [@js.union on_field "dummy"]) Primitive.t [@@js.get "additionalItems"]
  val set_additionalItems: t -> ([`U1 of bool | `U2 of t] [@js.union]) -> unit [@@js.set "additionalItems"]
  val get_items: t -> (t list, t) union2 [@@js.get "items"]
  val set_items: t -> ([`U1 of t | `U2 of t list] [@js.union]) -> unit [@@js.set "items"]
  val get_maxItems: t -> float [@@js.get "maxItems"]
  val set_maxItems: t -> float -> unit [@@js.set "maxItems"]
  val get_minItems: t -> float [@@js.get "minItems"]
  val set_minItems: t -> float -> unit [@@js.set "minItems"]
  val get_uniqueItems: t -> bool [@@js.get "uniqueItems"]
  val set_uniqueItems: t -> bool -> unit [@@js.set "uniqueItems"]
  val get_maxProperties: t -> float [@@js.get "maxProperties"]
  val set_maxProperties: t -> float -> unit [@@js.set "maxProperties"]
  val get_minProperties: t -> float [@@js.get "minProperties"]
  val set_minProperties: t -> float -> unit [@@js.set "minProperties"]
  val get_required: t -> ([`Boolean of bool | `Other of string list] [@js.union on_field "dummy"]) Primitive.t [@@js.get "required"]
  val set_required: t -> ([`U1 of string list | `U2 of bool] [@js.union]) -> unit [@@js.set "required"]
  val get_additionalProperties: t -> ([`Boolean of bool | `Other of t] [@js.union on_field "dummy"]) Primitive.t [@@js.get "additionalProperties"]
  val set_additionalProperties: t -> ([`U1 of bool | `U2 of t] [@js.union]) -> unit [@@js.set "additionalProperties"]
  val get_definitions: t -> AnonymousInterface5.t [@@js.get "definitions"]
  val set_definitions: t -> AnonymousInterface5.t -> unit [@@js.set "definitions"]
  val get_properties: t -> AnonymousInterface5.t [@@js.get "properties"]
  val set_properties: t -> AnonymousInterface5.t -> unit [@@js.set "properties"]
  val get_patternProperties: t -> AnonymousInterface5.t [@@js.get "patternProperties"]
  val set_patternProperties: t -> AnonymousInterface5.t -> unit [@@js.set "patternProperties"]
  val get_dependencies: t -> AnonymousInterface6.t [@@js.get "dependencies"]
  val set_dependencies: t -> AnonymousInterface6.t -> unit [@@js.set "dependencies"]
  val get_const: t -> any [@@js.get "const"]
  val set_const: t -> any -> unit [@@js.set "const"]
  val get_enum: t -> any list [@@js.get "enum"]
  val set_enum: t -> any list -> unit [@@js.set "enum"]
  val get_type: t -> ([`String of string | `Other of string list] [@js.union on_field "dummy"]) Primitive.t [@@js.get "type"]
  val set_type: t -> ([`U1 of string | `U2 of string list] [@js.union]) -> unit [@@js.set "type"]
  val get_format: t -> string [@@js.get "format"]
  val set_format: t -> string -> unit [@@js.set "format"]
  val get_allOf: t -> t list [@@js.get "allOf"]
  val set_allOf: t -> t list -> unit [@@js.set "allOf"]
  val get_anyOf: t -> t list [@@js.get "anyOf"]
  val set_anyOf: t -> t list -> unit [@@js.set "anyOf"]
  val get_oneOf: t -> t list [@@js.get "oneOf"]
  val set_oneOf: t -> t list -> unit [@@js.set "oneOf"]
  val get_not: t -> t [@@js.get "not"]
  val set_not: t -> t -> unit [@@js.set "not"]
  val get_if: t -> t [@@js.get "if"]
  val set_if: t -> t -> unit [@@js.set "if"]
  val get_then: t -> t [@@js.get "then"]
  val set_then: t -> t -> unit [@@js.set "then"]
  val get_else: t -> t [@@js.get "else"]
  val set_else: t -> t -> unit [@@js.set "else"]
  val create: _id:(string[@js "$id"]) -> id:string -> _schema:(string[@js "$schema"]) -> _ref:(string[@js "$ref"]) -> title:string -> description:string -> multipleOf:float -> maximum:float -> exclusiveMaximum:([`Number of float | `Boolean of bool] [@js.union on_field "dummy"]) Primitive.t -> minimum:float -> exclusiveMinimum:([`Number of float | `Boolean of bool] [@js.union on_field "dummy"]) Primitive.t -> maxLength:float -> minLength:float -> pattern:([`String of string | `Other of regexp] [@js.union on_field "dummy"]) Primitive.t -> additionalItems:([`Boolean of bool | `Other of t] [@js.union on_field "dummy"]) Primitive.t -> items:(t list, t) union2 -> maxItems:float -> minItems:float -> uniqueItems:bool -> maxProperties:float -> minProperties:float -> required:([`Boolean of bool | `Other of string list] [@js.union on_field "dummy"]) Primitive.t -> additionalProperties:([`Boolean of bool | `Other of t] [@js.union on_field "dummy"]) Primitive.t -> definitions:AnonymousInterface5.t -> properties:AnonymousInterface5.t -> patternProperties:AnonymousInterface5.t -> dependencies:AnonymousInterface6.t -> const:any -> enum:any list -> type_:(([`String of string | `Other of string list] [@js.union on_field "dummy"]) Primitive.t[@js "type"]) -> format:string -> allOf:t list -> anyOf:t list -> oneOf:t list -> not:t -> if_:(t[@js "if"]) -> then_:(t[@js "then"]) -> else_:(t[@js "else"]) -> unit -> t [@@js.builder]
end
module[@js.scope "ValidationError"] ValidationError : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val create: ?message:string -> ?instance:any -> ?schema:Schema.t -> ?propertyPath:any -> ?name:string -> ?argument:any -> unit -> t [@@js.create]
  val get_path: t -> ([`Number of float | `String of string] [@js.union on_field "dummy"]) Primitive.t list [@@js.get "path"]
  val set_path: t -> ([`U1 of string | `U2 of float] [@js.union]) list -> unit [@@js.set "path"]
  val get_property: t -> string [@@js.get "property"]
  val set_property: t -> string -> unit [@@js.set "property"]
  val get_message: t -> string [@@js.get "message"]
  val set_message: t -> string -> unit [@@js.set "message"]
  val get_schema: t -> ([`String of string | `Other of Schema.t] [@js.union on_field "dummy"]) Primitive.t [@@js.get "schema"]
  val set_schema: t -> ([`U1 of string | `U2 of Schema.t] [@js.union]) -> unit [@@js.set "schema"]
  val get_instance: t -> any [@@js.get "instance"]
  val set_instance: t -> any -> unit [@@js.set "instance"]
  val get_name: t -> string [@@js.get "name"]
  val set_name: t -> string -> unit [@@js.set "name"]
  val get_argument: t -> any [@@js.get "argument"]
  val set_argument: t -> any -> unit [@@js.set "argument"]
  val toString: t -> string [@@js.call "toString"]
  val get_stack: t -> string [@@js.get "stack"]
  val set_stack: t -> string -> unit [@@js.set "stack"]
end
module ErrorDetail : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val get_message: t -> string [@@js.get "message"]
  val set_message: t -> string -> unit [@@js.set "message"]
  val get_name: t -> string [@@js.get "name"]
  val set_name: t -> string -> unit [@@js.set "name"]
  val get_argument: t -> string [@@js.get "argument"]
  val set_argument: t -> string -> unit [@@js.set "argument"]
  val create: message:string -> name:string -> argument:string -> unit -> t [@@js.builder]
end
module rec Options : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val get_skipAttributes: t -> string list [@@js.get "skipAttributes"]
  val set_skipAttributes: t -> string list -> unit [@@js.set "skipAttributes"]
  val get_allowUnknownAttributes: t -> bool [@@js.get "allowUnknownAttributes"]
  val set_allowUnknownAttributes: t -> bool -> unit [@@js.set "allowUnknownAttributes"]
  val get_preValidateProperty: t -> PreValidatePropertyFunction.t [@@js.get "preValidateProperty"]
  val set_preValidateProperty: t -> PreValidatePropertyFunction.t -> unit [@@js.set "preValidateProperty"]
  val get_rewrite: t -> RewriteFunction.t [@@js.get "rewrite"]
  val set_rewrite: t -> RewriteFunction.t -> unit [@@js.set "rewrite"]
  val get_base: t -> string [@@js.get "base"]
  val set_base: t -> string -> unit [@@js.set "base"]
  val get_throwError: t -> bool [@@js.get "throwError"]
  val set_throwError: t -> bool -> unit [@@js.set "throwError"]
  val get_required: t -> bool [@@js.get "required"]
  val set_required: t -> bool -> unit [@@js.set "required"]
  val get_throwFirst: t -> bool [@@js.get "throwFirst"]
  val set_throwFirst: t -> bool -> unit [@@js.set "throwFirst"]
  val get_throwAll: t -> bool [@@js.get "throwAll"]
  val set_throwAll: t -> bool -> unit [@@js.set "throwAll"]
  val get_nestedErrors: t -> bool [@@js.get "nestedErrors"]
  val set_nestedErrors: t -> bool -> unit [@@js.set "nestedErrors"]
  val create: skipAttributes:string list -> allowUnknownAttributes:bool -> preValidateProperty:PreValidatePropertyFunction.t -> rewrite:RewriteFunction.t -> base:string -> throwError:bool -> required:bool -> throwFirst:bool -> throwAll:bool -> nestedErrors:bool -> unit -> t [@@js.builder]
end
and PreValidatePropertyFunction : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val apply: t -> instance:any -> key:string -> schema:Schema.t -> options:Options.t -> ctx:SchemaContext.t -> any [@@js.apply]
end
and RewriteFunction : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val apply: t -> instance:any -> schema:Schema.t -> options:Options.t -> ctx:SchemaContext.t -> any [@@js.apply]
end
and SchemaContext : sig
  module AnonymousInterface0 : sig
    type t = private Ojs.t
    val t_to_js: t -> Ojs.t
    val t_of_js: Ojs.t -> t
    val get: t -> string -> Schema.t [@@js.index_get]
    val set: t -> string -> Schema.t -> unit [@@js.index_set]
  end
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val get_schema: t -> Schema.t [@@js.get "schema"]
  val set_schema: t -> Schema.t -> unit [@@js.set "schema"]
  val get_options: t -> Options.t [@@js.get "options"]
  val set_options: t -> Options.t -> unit [@@js.set "options"]
  val get_propertyPath: t -> string [@@js.get "propertyPath"]
  val set_propertyPath: t -> string -> unit [@@js.set "propertyPath"]
  val get_base: t -> string [@@js.get "base"]
  val set_base: t -> string -> unit [@@js.set "base"]
  val get_schemas: t -> AnonymousInterface0.t [@@js.get "schemas"]
  val set_schemas: t -> AnonymousInterface0.t -> unit [@@js.set "schemas"]
  val makeChild: t -> schema:Schema.t -> key:string -> t [@@js.call "makeChild"]
  val create: schema:Schema.t -> options:Options.t -> propertyPath:string -> base:string -> schemas:AnonymousInterface0.t -> makeChild:(schema:Schema.t -> key:string -> t) -> unit -> t [@@js.builder]
end
module[@js.scope "ValidatorResult"] ValidatorResult : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val create: instance:any -> schema:Schema.t -> options:Options.t -> ctx:SchemaContext.t -> t [@@js.create]
  val get_instance: t -> any [@@js.get "instance"]
  val set_instance: t -> any -> unit [@@js.set "instance"]
  val get_schema: t -> Schema.t [@@js.get "schema"]
  val set_schema: t -> Schema.t -> unit [@@js.set "schema"]
  val get_propertyPath: t -> string [@@js.get "propertyPath"]
  val set_propertyPath: t -> string -> unit [@@js.set "propertyPath"]
  val get_errors: t -> ValidationError.t list [@@js.get "errors"]
  val set_errors: t -> ValidationError.t list -> unit [@@js.set "errors"]
  val get_throwError: t -> bool [@@js.get "throwError"]
  val set_throwError: t -> bool -> unit [@@js.set "throwError"]
  val get_disableFormat: t -> bool [@@js.get "disableFormat"]
  val set_disableFormat: t -> bool -> unit [@@js.set "disableFormat"]
  val get_valid: t -> bool [@@js.get "valid"]
  val set_valid: t -> bool -> unit [@@js.set "valid"]
  val addError: t -> detail:([`U1 of string | `U2 of ErrorDetail.t] [@js.union]) -> ValidationError.t [@@js.call "addError"]
  val toString: t -> string [@@js.call "toString"]
end
module CustomProperty : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val apply: t -> instance:any -> schema:Schema.t -> options:Options.t -> ctx:SchemaContext.t -> ([`String of string | `Other of ValidatorResult.t] [@js.union on_field "dummy"]) Primitive.t [@@js.apply]
end
module CustomFormat : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val apply: t -> input:any -> bool [@@js.apply]
end
module[@js.scope "Validator"] Validator : sig
  module AnonymousInterface7 : sig
    type t = private Ojs.t
    val t_to_js: t -> Ojs.t
    val t_of_js: Ojs.t -> t
    val get: t -> string -> CustomProperty.t [@@js.index_get]
    val set: t -> string -> CustomProperty.t -> unit [@@js.index_set]
  end
  module AnonymousInterface2 : sig
    type t = private Ojs.t
    val t_to_js: t -> Ojs.t
    val t_of_js: Ojs.t -> t
    val get: t -> string -> Schema.t [@@js.index_get]
    val set: t -> string -> Schema.t -> unit [@@js.index_set]
  end
  module AnonymousInterface1 : sig
    type t = private Ojs.t
    val t_to_js: t -> Ojs.t
    val t_of_js: Ojs.t -> t
    val get: t -> string -> CustomFormat.t [@@js.index_get]
    val set: t -> string -> CustomFormat.t -> unit [@@js.index_set]
  end
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val create: unit -> t [@@js.create]
  val get_customFormats: t -> AnonymousInterface1.t [@@js.get "customFormats"]
  val set_customFormats: t -> AnonymousInterface1.t -> unit [@@js.set "customFormats"]
  val get_schemas: t -> AnonymousInterface2.t [@@js.get "schemas"]
  val set_schemas: t -> AnonymousInterface2.t -> unit [@@js.set "schemas"]
  val get_unresolvedRefs: t -> string list [@@js.get "unresolvedRefs"]
  val set_unresolvedRefs: t -> string list -> unit [@@js.set "unresolvedRefs"]
  val get_attributes: t -> AnonymousInterface7.t [@@js.get "attributes"]
  val set_attributes: t -> AnonymousInterface7.t -> unit [@@js.set "attributes"]
  val addSchema: t -> ?schema:Schema.t -> ?uri:string -> unit -> (Schema.t, unit) union2 [@@js.call "addSchema"]
  val validate: t -> instance:any -> schema:Schema.t -> ?options:Options.t -> ?ctx:SchemaContext.t -> unit -> ValidatorResult.t [@@js.call "validate"]
end
module[@js.scope "SchemaError"] SchemaError : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val create: msg:string -> schema:Schema.t -> t [@@js.create]
  val get_schema: t -> Schema.t [@@js.get "schema"]
  val set_schema: t -> Schema.t -> unit [@@js.set "schema"]
  val get_message: t -> string [@@js.get "message"]
  val set_message: t -> string -> unit [@@js.set "message"]
end
val validate: instance:any -> schema:any -> ?options:Options.t -> unit -> ValidatorResult.t [@@js.global "validate"]

module Export : sig
  (* export interface Schema *)
  [@@@js.stop] module Schema = Schema [@@@js.start] [@@@js.implem module Schema = Schema]
  (* export interface Options *)
  [@@@js.stop] module Options = Options [@@@js.start] [@@@js.implem module Options = Options]
  (* export interface RewriteFunction *)
  [@@@js.stop] module RewriteFunction = RewriteFunction [@@@js.start] [@@@js.implem module RewriteFunction = RewriteFunction]
  (* export interface PreValidatePropertyFunction *)
  [@@@js.stop] module PreValidatePropertyFunction = PreValidatePropertyFunction [@@@js.start] [@@@js.implem module PreValidatePropertyFunction = PreValidatePropertyFunction]
  (* export interface SchemaContext *)
  [@@@js.stop] module SchemaContext = SchemaContext [@@@js.start] [@@@js.implem module SchemaContext = SchemaContext]
  (* export interface CustomFormat *)
  [@@@js.stop] module CustomFormat = CustomFormat [@@@js.start] [@@@js.implem module CustomFormat = CustomFormat]
  (* export interface CustomProperty *)
  [@@@js.stop] module CustomProperty = CustomProperty [@@@js.start] [@@@js.implem module CustomProperty = CustomProperty]
  (* export interface ErrorDetail *)
  [@@@js.stop] module ErrorDetail = ErrorDetail [@@@js.start] [@@@js.implem module ErrorDetail = ErrorDetail]
end
