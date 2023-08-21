export type CustomizedUnion = { tag: "case1'"; value: number } | { tag: "case2'"; "x'": number; "y'": number };
export function analyzeCustomizedUnion<__bindoj_ret>(__bindoj_fns: {
  "case1'": (__bindoj_v: { tag: "case1'"; value: number }) => __bindoj_ret;
  "case2'": (__bindoj_v: { tag: "case2'"; "x'": number; "y'": number }) => __bindoj_ret;
}): (__bindoj_x: CustomizedUnion) => __bindoj_ret {
  return (__bindoj_x: CustomizedUnion) => {
    if (__bindoj_x.tag === "case1'") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "case2'") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeCustomizedUnion - unrecognized: " + __bindoj_x);
    }
  };
}
