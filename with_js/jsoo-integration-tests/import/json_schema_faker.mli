[@@@ocaml.warning "-7-11-32-33-39"]
[@@@js.implem
  [@@@ocaml.warning "-7-11-32-33-39"]
]
[@@@js.scope "json-schema-faker"]
open Ts2ocaml

[@@@js.stop]
module JsonValue = Ojs
module JsonObject = Ojs
module JSONSchema4 = Ojs
module JSONSchema6 = Ojs
module JSONSchema7 = Ojs
module Date = Ojs
[@@@js.start]
[@@@js.implem
module JsonValue = Ojs
module JsonObject = Ojs
module JSONSchema4 = Ojs
module JSONSchema6 = Ojs
module JSONSchema7 = Ojs
module Date = Ojs
]

module JSONSchemaFakerOptions : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val get_defaultInvalidTypeProduct: t -> bool [@@js.get "defaultInvalidTypeProduct"]
  val set_defaultInvalidTypeProduct: t -> bool -> unit [@@js.set "defaultInvalidTypeProduct"]
  val get_defaultRandExpMax: t -> float [@@js.get "defaultRandExpMax"]
  val set_defaultRandExpMax: t -> float -> unit [@@js.set "defaultRandExpMax"]
  val get_pruneProperties: t -> string list [@@js.get "pruneProperties"]
  val set_pruneProperties: t -> string list -> unit [@@js.set "pruneProperties"]
  val get_ignoreProperties: t -> string list [@@js.get "ignoreProperties"]
  val set_ignoreProperties: t -> string list -> unit [@@js.set "ignoreProperties"]
  val get_ignoreMissingRefs: t -> bool [@@js.get "ignoreMissingRefs"]
  val set_ignoreMissingRefs: t -> bool -> unit [@@js.set "ignoreMissingRefs"]
  val get_failOnInvalidTypes: t -> bool [@@js.get "failOnInvalidTypes"]
  val set_failOnInvalidTypes: t -> bool -> unit [@@js.set "failOnInvalidTypes"]
  val get_failOnInvalidFormat: t -> bool [@@js.get "failOnInvalidFormat"]
  val set_failOnInvalidFormat: t -> bool -> unit [@@js.set "failOnInvalidFormat"]
  val get_alwaysFakeOptionals: t -> bool [@@js.get "alwaysFakeOptionals"]
  val set_alwaysFakeOptionals: t -> bool -> unit [@@js.set "alwaysFakeOptionals"]
  val get_optionalsProbability: t -> ([`Number of float | `Other of ([`L_b_false[@js false]] [@js.enum])] [@js.union on_field "dummy"]) Primitive.t [@@js.get "optionalsProbability"]
  val set_optionalsProbability: t -> ([`U1 of float | `U2 of ([`L_b_false[@js false]] [@js.enum])] [@js.union]) -> unit [@@js.set "optionalsProbability"]
  val get_fixedProbabilities: t -> bool [@@js.get "fixedProbabilities"]
  val set_fixedProbabilities: t -> bool -> unit [@@js.set "fixedProbabilities"]
  val get_useExamplesValue: t -> bool [@@js.get "useExamplesValue"]
  val set_useExamplesValue: t -> bool -> unit [@@js.set "useExamplesValue"]
  val get_useDefaultValue: t -> bool [@@js.get "useDefaultValue"]
  val set_useDefaultValue: t -> bool -> unit [@@js.set "useDefaultValue"]
  val get_requiredOnly: t -> bool [@@js.get "requiredOnly"]
  val set_requiredOnly: t -> bool -> unit [@@js.set "requiredOnly"]
  val get_minItems: t -> float [@@js.get "minItems"]
  val set_minItems: t -> float -> unit [@@js.set "minItems"]
  val get_maxItems: t -> float [@@js.get "maxItems"]
  val set_maxItems: t -> float -> unit [@@js.set "maxItems"]
  val get_minLength: t -> float [@@js.get "minLength"]
  val set_minLength: t -> float -> unit [@@js.set "minLength"]
  val get_maxLength: t -> float [@@js.get "maxLength"]
  val set_maxLength: t -> float -> unit [@@js.set "maxLength"]
  val get_resolveJsonPath: t -> bool [@@js.get "resolveJsonPath"]
  val set_resolveJsonPath: t -> bool -> unit [@@js.set "resolveJsonPath"]
  val get_reuseProperties: t -> bool [@@js.get "reuseProperties"]
  val set_reuseProperties: t -> bool -> unit [@@js.set "reuseProperties"]
  val get_fillProperties: t -> bool [@@js.get "fillProperties"]
  val set_fillProperties: t -> bool -> unit [@@js.set "fillProperties"]
  val get_replaceEmptyByRandomValue: t -> bool [@@js.get "replaceEmptyByRandomValue"]
  val set_replaceEmptyByRandomValue: t -> bool -> unit [@@js.set "replaceEmptyByRandomValue"]
  val random: t -> float [@@js.call "random"]
  val get_renderTitle: t -> bool [@@js.get "renderTitle"]
  val set_renderTitle: t -> bool -> unit [@@js.set "renderTitle"]
  val get_renderDescription: t -> bool [@@js.get "renderDescription"]
  val set_renderDescription: t -> bool -> unit [@@js.set "renderDescription"]
  val get_renderComment: t -> bool [@@js.get "renderComment"]
  val set_renderComment: t -> bool -> unit [@@js.set "renderComment"]
  val get_refDepthMax: t -> float [@@js.get "refDepthMax"]
  val set_refDepthMax: t -> float -> unit [@@js.set "refDepthMax"]
  val get_refDepthMin: t -> float [@@js.get "refDepthMin"]
  val set_refDepthMin: t -> float -> unit [@@js.set "refDepthMin"]
  val create: ?defaultInvalidTypeProduct:bool -> ?defaultRandExpMax:float -> ?pruneProperties:string list -> ?ignoreProperties:string list -> ?ignoreMissingRefs:bool -> ?failOnInvalidTypes:bool -> ?failOnInvalidFormat:bool -> ?alwaysFakeOptionals:bool -> ?optionalsProbability:([`Number of float | `Other of ([`L_b_false[@js false]] [@js.enum])] [@js.union on_field "dummy"]) Primitive.t -> ?fixedProbabilities:bool -> ?useExamplesValue:bool -> ?useDefaultValue:bool -> ?requiredOnly:bool -> ?minItems:float -> ?maxItems:float -> ?minLength:float -> ?maxLength:float -> ?resolveJsonPath:bool -> ?reuseProperties:bool -> ?fillProperties:bool -> ?replaceEmptyByRandomValue:bool -> ?random:(unit -> float) -> ?renderTitle:bool -> ?renderDescription:bool -> ?renderComment:bool -> ?refDepthMax:float -> ?refDepthMin:float -> unit -> t [@@js.builder]
end
module[@js.scope "JSONSchemaFakerOption"] JSONSchemaFakerOption : sig
  val getDefaults: unit -> JSONSchemaFakerOptions.t [@@js.global "getDefaults"]
end
module Schema : sig
  type t = (JSONSchema4.t, JSONSchema6.t, JSONSchema7.t) union3
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
end
module JSONSchemaFakerRefs : sig
  module AnonymousInterface1 : sig
    type t = private Ojs.t
    val t_to_js: t -> Ojs.t
    val t_of_js: Ojs.t -> t
    val get: t -> string -> Schema.t [@@js.index_get]
    val set: t -> string -> Schema.t -> unit [@@js.index_set]
  end
  type t = (Schema.t list, AnonymousInterface1.t) union2
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
end
module JSONSchemaFakerFormat : sig
  module AnonymousInterface2 : sig
    type t = private Ojs.t
    val t_to_js: t -> Ojs.t
    val t_of_js: Ojs.t -> t
    val get: t -> string -> (Schema.t -> unknown [@js.dummy]) [@@js.index_get]
    val set: t -> string -> (Schema.t -> unknown) -> unit [@@js.index_set]
  end
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val apply: t -> opts:AnonymousInterface2.t -> unit [@@js.apply]
  val apply': t -> name:string -> callback:(Schema.t -> unknown) -> unit [@@js.apply]
end
module JSONSchemaFakerDefine : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val apply: t -> value:JsonValue.t -> schema:JsonObject.t -> property:string -> rootSchema:Schema.t -> propertyPath:string list -> JsonValue.t [@@js.apply]
end
module[@js.scope "JSONSchemaFaker"] JSONSchemaFaker : sig
  module[@js.scope "format"] rec FormatStatic : sig
    module AnonymousInterface2 : sig
      type t = private Ojs.t
      val t_to_js: t -> Ojs.t
      val t_of_js: Ojs.t -> t
      val get: t -> string -> (Schema.t -> unknown [@js.dummy]) [@@js.index_get]
      val set: t -> string -> (Schema.t -> unknown) -> unit [@@js.index_set]
    end
    val invoke: AnonymousInterface2.t -> unit [@@js.invoke]
    val invoke': name:string -> callback:(Schema.t -> unknown) -> unit [@@js.invoke]
  end
  and[@js.scope "random"] Random : sig
    val date: ?step:([`U1 of ([`L_s29_seconds[@js "seconds"]] [@js.enum]) | `U2 of ([`L_s15_minutes[@js "minutes"]] [@js.enum]) | `U3 of ([`L_s8_hours[@js "hours"]] [@js.enum]) | `U4 of ([`L_s1_days[@js "days"]] [@js.enum]) | `U5 of ([`L_s32_weeks[@js "weeks"]] [@js.enum]) | `U6 of ([`L_s16_months[@js "months"]] [@js.enum]) | `U7 of ([`L_s33_years[@js "years"]] [@js.enum])] [@js.union]) -> unit -> ([`Number of float | `Other of Date.t] [@js.union on_field "dummy"]) Primitive.t [@@js.global "date"]
    val pick: any list -> any [@@js.global "pick"]
    val shuffle: any list -> any list [@@js.global "shuffle"]
    val number: ?min:float -> ?max:float -> ?defMin:float -> ?defMax:float -> ?hasPrecision:bool -> unit -> float [@@js.global "number"]
    val randexp: string -> string [@@js.global "randexp"]
  end
  val version: unit -> string [@@js.get "VERSION"]
  val format: unit -> JSONSchemaFakerFormat.t [@@js.get "format"]
  val option: unit -> ((JSONSchemaFakerOptions.t -> unit), (name:([`L_s0_alwaysFakeOptionals[@js "alwaysFakeOptionals"] | `L_s10_ignoreProperties[@js "ignoreProperties"] | `L_s11_maxItems[@js "maxItems"] | `L_s12_maxLength[@js "maxLength"] | `L_s13_minItems[@js "minItems"] | `L_s14_minLength[@js "minLength"] | `L_s17_optionalsProbability[@js "optionalsProbability"] | `L_s18_pruneProperties[@js "pruneProperties"] | `L_s19_random[@js "random"] | `L_s20_refDepthMax[@js "refDepthMax"] | `L_s21_refDepthMin[@js "refDepthMin"] | `L_s22_renderComment[@js "renderComment"] | `L_s23_renderDescription[@js "renderDescription"] | `L_s24_renderTitle[@js "renderTitle"] | `L_s25_replaceEmptyByRandomValue[@js "replaceEmptyByRandomValue"] | `L_s26_requiredOnly[@js "requiredOnly"] | `L_s27_resolveJsonPath[@js "resolveJsonPath"] | `L_s28_reuseProperties[@js "reuseProperties"] | `L_s2_defaultInvalidTypeProduct[@js "defaultInvalidTypeProduct"] | `L_s30_useDefaultValue[@js "useDefaultValue"] | `L_s31_useExamplesValue[@js "useExamplesValue"] | `L_s3_defaultRandExpMax[@js "defaultRandExpMax"] | `L_s4_failOnInvalidFormat[@js "failOnInvalidFormat"] | `L_s5_failOnInvalidTypes[@js "failOnInvalidTypes"] | `L_s6_fillProperties[@js "fillProperties"] | `L_s7_fixedProbabilities[@js "fixedProbabilities"] | `L_s9_ignoreMissingRefs[@js "ignoreMissingRefs"]] [@js.enum]) -> value:any -> unit)) intersection2 [@@js.get "option"]
  val generate: schema:Schema.t -> ?refs:JSONSchemaFakerRefs.t -> unit -> JsonValue.t [@@js.global "generate"]
  val generateYAML: schema:Schema.t -> ?refs:JSONSchemaFakerRefs.t -> unit -> string [@@js.global "generateYAML"]
  (* val resolve: schema:Schema.t -> ?refs:JSONSchemaFakerRefs.t -> ?cwd:string -> unit -> JsonValue.t Promise.t [@@js.global "resolve"]
  val resolveYAML: schema:Schema.t -> ?refs:JSONSchemaFakerRefs.t -> ?cwd:string -> unit -> string Promise.t [@@js.global "resolveYAML"] *)
  val extend: name:string -> cb:(any -> any) -> (schema:Schema.t -> ?refs:JSONSchemaFakerRefs.t -> ?cwd:string -> unit -> JsonValue.t [@js.dummy]) [@@js.global "extend"]
  val define: name:string -> cb:JSONSchemaFakerDefine.t -> (schema:Schema.t -> ?refs:JSONSchemaFakerRefs.t -> ?cwd:string -> unit -> JsonValue.t [@js.dummy]) [@@js.global "define"]
  val reset: string -> (schema:Schema.t -> ?refs:JSONSchemaFakerRefs.t -> ?cwd:string -> unit -> JsonValue.t [@js.dummy]) [@@js.global "reset"]
  val locate: string -> any [@@js.global "locate"]
end
val jSONSchemaFakerOption: JSONSchemaFakerOptions.t -> unit [@@js.global "JSONSchemaFakerOption"]
val jSONSchemaFakerOption': name:([`L_s0_alwaysFakeOptionals[@js "alwaysFakeOptionals"] | `L_s10_ignoreProperties[@js "ignoreProperties"] | `L_s11_maxItems[@js "maxItems"] | `L_s12_maxLength[@js "maxLength"] | `L_s13_minItems[@js "minItems"] | `L_s14_minLength[@js "minLength"] | `L_s17_optionalsProbability[@js "optionalsProbability"] | `L_s18_pruneProperties[@js "pruneProperties"] | `L_s19_random[@js "random"] | `L_s20_refDepthMax[@js "refDepthMax"] | `L_s21_refDepthMin[@js "refDepthMin"] | `L_s22_renderComment[@js "renderComment"] | `L_s23_renderDescription[@js "renderDescription"] | `L_s24_renderTitle[@js "renderTitle"] | `L_s25_replaceEmptyByRandomValue[@js "replaceEmptyByRandomValue"] | `L_s26_requiredOnly[@js "requiredOnly"] | `L_s27_resolveJsonPath[@js "resolveJsonPath"] | `L_s28_reuseProperties[@js "reuseProperties"] | `L_s2_defaultInvalidTypeProduct[@js "defaultInvalidTypeProduct"] | `L_s30_useDefaultValue[@js "useDefaultValue"] | `L_s31_useExamplesValue[@js "useExamplesValue"] | `L_s3_defaultRandExpMax[@js "defaultRandExpMax"] | `L_s4_failOnInvalidFormat[@js "failOnInvalidFormat"] | `L_s5_failOnInvalidTypes[@js "failOnInvalidTypes"] | `L_s6_fillProperties[@js "fillProperties"] | `L_s7_fixedProbabilities[@js "fixedProbabilities"] | `L_s9_ignoreMissingRefs[@js "ignoreMissingRefs"]] [@js.enum]) -> value:any -> unit [@@js.global "JSONSchemaFakerOption"]

(** @deprecated calling JSONSchemaFaker() is deprecated, call either .generate() or .resolve()' *)
val jSONSchemaFaker: schema:Schema.t -> ?refs:JSONSchemaFakerRefs.t -> ?cwd:string -> unit -> JsonValue.t [@@js.global "JSONSchemaFaker"]

module Export : sig
  (* export type Schema *)
  [@@@js.stop] module Schema = Schema [@@@js.start] [@@@js.implem module Schema = Schema]
  (* export interface JSONSchemaFakerOptions *)
  [@@@js.stop] module JSONSchemaFakerOptions = JSONSchemaFakerOptions [@@@js.start] [@@@js.implem module JSONSchemaFakerOptions = JSONSchemaFakerOptions]
  (* export type JSONSchemaFakerRefs *)
  [@@@js.stop] module JSONSchemaFakerRefs = JSONSchemaFakerRefs [@@@js.start] [@@@js.implem module JSONSchemaFakerRefs = JSONSchemaFakerRefs]
  (* export interface JSONSchemaFakerDefine *)
  [@@@js.stop] module JSONSchemaFakerDefine = JSONSchemaFakerDefine [@@@js.start] [@@@js.implem module JSONSchemaFakerDefine = JSONSchemaFakerDefine]
  (* export interface JSONSchemaFakerFormat *)
  [@@@js.stop] module JSONSchemaFakerFormat = JSONSchemaFakerFormat [@@@js.start] [@@@js.implem module JSONSchemaFakerFormat = JSONSchemaFakerFormat]
  (* export default JSONSchemaFaker; *)
  [@@@js.stop] module Default = JSONSchemaFaker [@@@js.start] [@@@js.implem module Default = JSONSchemaFaker]
end
