import { Teacher } from "./reused_types/teacher";
export type PersonReused =
  | { kind: "anonymous" }
  | { kind: "student"; name: string; studentId: number }
  | ({ kind: "teacher" } & Teacher)
  | { kind: "with-id"; arg: number };
export function analyzePersonReused<__bindoj_ret>(__bindoj_fns: {
  anonymous: (__bindoj_v: { kind: "anonymous" }) => __bindoj_ret;
  student: (__bindoj_v: { kind: "student"; name: string; studentId: number }) => __bindoj_ret;
  teacher: (__bindoj_v: { kind: "teacher" } & Teacher) => __bindoj_ret;
  "with-id": (__bindoj_v: { kind: "with-id"; arg: number }) => __bindoj_ret;
}): (__bindoj_x: PersonReused) => __bindoj_ret {
  return (__bindoj_x: PersonReused) => {
    if (__bindoj_x.kind === "anonymous") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "student") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "teacher") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "with-id") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzePersonReused - unrecognized: " + __bindoj_x);
    }
  };
}
