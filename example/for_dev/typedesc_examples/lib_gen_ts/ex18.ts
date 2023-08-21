type IntOpt = number | null | undefined;
type XyOpt = { xOpt?: number; yOpt?: number };
export type OptionalVariant =
  | { tag: "inline-record"; intOpt?: number; objtuple: { _0?: number; _1?: number }; xOpt?: number; yOpt?: number }
  | ({ tag: "inline-record-spreading"; intOpt?: number } & XyOpt)
  | ({ tag: "reused-inline-record" } & XyOpt)
  | { tag: "tuple-like"; value?: number }
  | { tag: "tuple-like-alias"; value?: number }
  | { tag: "tuple-like-obj"; _0?: number; _1?: number }
  | ({ tag: "tuple-like-spreading" } & XyOpt);
export function analyzeOptionalVariant<__bindoj_ret>(__bindoj_fns: {
  "inline-record": (__bindoj_v: {
    tag: "inline-record";
    intOpt?: number;
    objtuple: { _0?: number; _1?: number };
    xOpt?: number;
    yOpt?: number;
  }) => __bindoj_ret;
  "inline-record-spreading": (__bindoj_v: { tag: "inline-record-spreading"; intOpt?: number } & XyOpt) => __bindoj_ret;
  "reused-inline-record": (__bindoj_v: { tag: "reused-inline-record" } & XyOpt) => __bindoj_ret;
  "tuple-like": (__bindoj_v: { tag: "tuple-like"; value?: number }) => __bindoj_ret;
  "tuple-like-alias": (__bindoj_v: { tag: "tuple-like-alias"; value?: number }) => __bindoj_ret;
  "tuple-like-obj": (__bindoj_v: { tag: "tuple-like-obj"; _0?: number; _1?: number }) => __bindoj_ret;
  "tuple-like-spreading": (__bindoj_v: { tag: "tuple-like-spreading" } & XyOpt) => __bindoj_ret;
}): (__bindoj_x: OptionalVariant) => __bindoj_ret {
  return (__bindoj_x: OptionalVariant) => {
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
      throw new TypeError("panic @analyzeOptionalVariant - unrecognized: " + __bindoj_x);
    }
  };
}
