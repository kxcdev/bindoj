import { ExAliasIntOpt } from "./ex_alias_gen";
export type ExOptionalXyOpt = { xOpt?: number; yOpt?: number };

export type ExOptionalVariant =
  | { tag: "inline-record"; intOpt?: number; objtuple: { _0?: number; _1?: number }; xOpt?: number; yOpt?: number }
  | ({ tag: "inline-record-spreading"; intOpt?: number } & ExOptionalXyOpt)
  | ({ tag: "reused-inline-record" } & ExOptionalXyOpt)
  | { tag: "tuple-like"; arg?: number }
  | { tag: "tuple-like-alias"; arg?: number }
  | { tag: "tuple-like-obj"; _0?: number; _1?: number }
  | ({ tag: "tuple-like-spreading" } & ExOptionalXyOpt);
export function analyzeExOptionalVariant<__bindoj_ret>(__bindoj_fns: {
  "inline-record": (__bindoj_v: {
    tag: "inline-record";
    intOpt?: number;
    objtuple: { _0?: number; _1?: number };
    xOpt?: number;
    yOpt?: number;
  }) => __bindoj_ret;
  "inline-record-spreading": (
    __bindoj_v: { tag: "inline-record-spreading"; intOpt?: number } & ExOptionalXyOpt
  ) => __bindoj_ret;
  "reused-inline-record": (__bindoj_v: { tag: "reused-inline-record" } & ExOptionalXyOpt) => __bindoj_ret;
  "tuple-like": (__bindoj_v: { tag: "tuple-like"; arg?: number }) => __bindoj_ret;
  "tuple-like-alias": (__bindoj_v: { tag: "tuple-like-alias"; arg?: number }) => __bindoj_ret;
  "tuple-like-obj": (__bindoj_v: { tag: "tuple-like-obj"; _0?: number; _1?: number }) => __bindoj_ret;
  "tuple-like-spreading": (__bindoj_v: { tag: "tuple-like-spreading" } & ExOptionalXyOpt) => __bindoj_ret;
}): (__bindoj_x: ExOptionalVariant) => __bindoj_ret {
  return (__bindoj_x: ExOptionalVariant) => {
    if (__bindoj_x.tag === "inline-record") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "inline-record-spreading") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "reused-inline-record") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "tuple-like") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "tuple-like-alias") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "tuple-like-obj") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "tuple-like-spreading") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeExOptionalVariant - unrecognized: " + __bindoj_x);
    }
  };
}
