import { ExRecordTeacher } from "./ex_record_gen";
export type ExVariantPerson =
  | { kind: "anonymous" }
  | { kind: "student"; name: string; studentId: number }
  | { kind: "teacher"; department: string; facultyId: number; name: string }
  | { kind: "with-id"; arg: number };
export function analyzeExVariantPerson<__bindoj_ret>(__bindoj_fns: {
  anonymous: (__bindoj_v: { kind: "anonymous" }) => __bindoj_ret;
  student: (__bindoj_v: { kind: "student"; name: string; studentId: number }) => __bindoj_ret;
  teacher: (__bindoj_v: { kind: "teacher"; department: string; facultyId: number; name: string }) => __bindoj_ret;
  "with-id": (__bindoj_v: { kind: "with-id"; arg: number }) => __bindoj_ret;
}): (__bindoj_x: ExVariantPerson) => __bindoj_ret {
  return (__bindoj_x: ExVariantPerson) => {
    if (__bindoj_x.kind === "anonymous") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "student") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "teacher") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "with-id") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeExVariantPerson - unrecognized: " + __bindoj_x);
    }
  };
}

export type ExVariantPersonReused =
  | { kind: "anonymous" }
  | { kind: "student"; name: string; studentId: number }
  | ({ kind: "teacher" } & ExRecordTeacher)
  | { kind: "with-id"; arg: number };
export function analyzeExVariantPersonReused<__bindoj_ret>(__bindoj_fns: {
  anonymous: (__bindoj_v: { kind: "anonymous" }) => __bindoj_ret;
  student: (__bindoj_v: { kind: "student"; name: string; studentId: number }) => __bindoj_ret;
  teacher: (__bindoj_v: { kind: "teacher" } & ExRecordTeacher) => __bindoj_ret;
  "with-id": (__bindoj_v: { kind: "with-id"; arg: number }) => __bindoj_ret;
}): (__bindoj_x: ExVariantPersonReused) => __bindoj_ret {
  return (__bindoj_x: ExVariantPersonReused) => {
    if (__bindoj_x.kind === "anonymous") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "student") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "teacher") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "with-id") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeExVariantPersonReused - unrecognized: " + __bindoj_x);
    }
  };
}

export type ExVariantIntList = { kind: "intcons"; arg: [number, ExVariantIntList] } | { kind: "intnil" };
export function analyzeExVariantIntList<__bindoj_ret>(__bindoj_fns: {
  intcons: (__bindoj_v: { kind: "intcons"; arg: [number, ExVariantIntList] }) => __bindoj_ret;
  intnil: (__bindoj_v: { kind: "intnil" }) => __bindoj_ret;
}): (__bindoj_x: ExVariantIntList) => __bindoj_ret {
  return (__bindoj_x: ExVariantIntList) => {
    if (__bindoj_x.kind === "intcons") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "intnil") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeExVariantIntList - unrecognized: " + __bindoj_x);
    }
  };
}

export type ExVariantIntListObjtuple =
  | { kind: "intcons"; _0: number; _1: ExVariantIntListObjtuple }
  | { kind: "intnil" };
export function analyzeExVariantIntListObjtuple<__bindoj_ret>(__bindoj_fns: {
  intcons: (__bindoj_v: { kind: "intcons"; _0: number; _1: ExVariantIntListObjtuple }) => __bindoj_ret;
  intnil: (__bindoj_v: { kind: "intnil" }) => __bindoj_ret;
}): (__bindoj_x: ExVariantIntListObjtuple) => __bindoj_ret {
  return (__bindoj_x: ExVariantIntListObjtuple) => {
    if (__bindoj_x.kind === "intcons") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "intnil") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeExVariantIntListObjtuple - unrecognized: " + __bindoj_x);
    }
  };
}

export type ExVariantFoo = { kind: "foo0" } | { kind: "foo1"; arg: number } | { kind: "foo2"; arg: [number, number] };
export function analyzeExVariantFoo<__bindoj_ret>(__bindoj_fns: {
  foo0: (__bindoj_v: { kind: "foo0" }) => __bindoj_ret;
  foo1: (__bindoj_v: { kind: "foo1"; arg: number }) => __bindoj_ret;
  foo2: (__bindoj_v: { kind: "foo2"; arg: [number, number] }) => __bindoj_ret;
}): (__bindoj_x: ExVariantFoo) => __bindoj_ret {
  return (__bindoj_x: ExVariantFoo) => {
    if (__bindoj_x.kind === "foo0") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "foo1") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "foo2") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeExVariantFoo - unrecognized: " + __bindoj_x);
    }
  };
}

export type ExVariantCustomizedUnion = { tag: "case1'"; value: number } | { tag: "case2'"; "x'": number; "y'": number };
export function analyzeExVariantCustomizedUnion<__bindoj_ret>(__bindoj_fns: {
  "case1'": (__bindoj_v: { tag: "case1'"; value: number }) => __bindoj_ret;
  "case2'": (__bindoj_v: { tag: "case2'"; "x'": number; "y'": number }) => __bindoj_ret;
}): (__bindoj_x: ExVariantCustomizedUnion) => __bindoj_ret {
  return (__bindoj_x: ExVariantCustomizedUnion) => {
    if (__bindoj_x.tag === "case1'") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "case2'") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeExVariantCustomizedUnion - unrecognized: " + __bindoj_x);
    }
  };
}
