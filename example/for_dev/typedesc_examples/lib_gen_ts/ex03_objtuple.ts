export type IntListObjtuple = { kind: "intcons"; _0: number; _1: IntListObjtuple } | { kind: "intnil" };
export function analyzeIntListObjtuple<__bindoj_ret>(__bindoj_fns: {
  intcons: (__bindoj_v: { kind: "intcons"; _0: number; _1: IntListObjtuple }) => __bindoj_ret;
  intnil: (__bindoj_v: { kind: "intnil" }) => __bindoj_ret;
}): (__bindoj_x: IntListObjtuple) => __bindoj_ret {
  return (__bindoj_x: IntListObjtuple) => {
    if (__bindoj_x.kind === "intcons") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "intnil") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeIntListObjtuple - unrecognized: " + __bindoj_x);
    }
  };
}
