export type IntList = { kind: "intcons"; arg: [number, IntList] } | { kind: "intnil" };
export function analyzeIntList<__bindoj_ret>(__bindoj_fns: {
  intcons: (__bindoj_v: { kind: "intcons"; arg: [number, IntList] }) => __bindoj_ret;
  intnil: (__bindoj_v: { kind: "intnil" }) => __bindoj_ret;
}): (__bindoj_x: IntList) => __bindoj_ret {
  return (__bindoj_x: IntList) => {
    if (__bindoj_x.kind === "intcons") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "intnil") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeIntList - unrecognized: " + __bindoj_x);
    }
  };
}
