import { ExRecordTeacher } from "./ex_record_gen";
export type ExVariantPerson =
  | { kind: "anonymous" }
  | { kind: "student"; name: string; studentId: number }
  | { kind: "teacher"; department: string; facultyId: number; name: string }
  | { kind: "with-id"; value: number };
export function analyzeExVariantPerson<__bindoj_ret>(__bindoj_fns: {
  anonymous: (__bindoj_v: { kind: "anonymous" }) => __bindoj_ret;
  student: (__bindoj_v: { kind: "student"; name: string; studentId: number }) => __bindoj_ret;
  teacher: (__bindoj_v: { kind: "teacher"; department: string; facultyId: number; name: string }) => __bindoj_ret;
  "with-id": (__bindoj_v: { kind: "with-id"; value: number }) => __bindoj_ret;
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
  | { kind: "with-id"; value: number };
export function analyzeExVariantPersonReused<__bindoj_ret>(__bindoj_fns: {
  anonymous: (__bindoj_v: { kind: "anonymous" }) => __bindoj_ret;
  student: (__bindoj_v: { kind: "student"; name: string; studentId: number }) => __bindoj_ret;
  teacher: (__bindoj_v: { kind: "teacher" } & ExRecordTeacher) => __bindoj_ret;
  "with-id": (__bindoj_v: { kind: "with-id"; value: number }) => __bindoj_ret;
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

export type ExVariantIntList = { kind: "intcons"; value: [number, ExVariantIntList] } | { kind: "intnil" };
export function analyzeExVariantIntList<__bindoj_ret>(__bindoj_fns: {
  intcons: (__bindoj_v: { kind: "intcons"; value: [number, ExVariantIntList] }) => __bindoj_ret;
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

export type ExVariantFoo =
  | { kind: "foo0" }
  | { kind: "foo1"; value: number }
  | { kind: "foo2"; value: [number, number] }
  | { kind: "foo3"; field1: number; field2: number };
export function analyzeExVariantFoo<__bindoj_ret>(__bindoj_fns: {
  foo0: (__bindoj_v: { kind: "foo0" }) => __bindoj_ret;
  foo1: (__bindoj_v: { kind: "foo1"; value: number }) => __bindoj_ret;
  foo2: (__bindoj_v: { kind: "foo2"; value: [number, number] }) => __bindoj_ret;
  foo3: (__bindoj_v: { kind: "foo3"; field1: number; field2: number }) => __bindoj_ret;
}): (__bindoj_x: ExVariantFoo) => __bindoj_ret {
  return (__bindoj_x: ExVariantFoo) => {
    if (__bindoj_x.kind === "foo0") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "foo1") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "foo2") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "foo3") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeExVariantFoo - unrecognized: " + __bindoj_x);
    }
  };
}

export type ExVariantCustomizedUnion =
  | { tag: "case-inline-record'"; "x'": number; "y'": number }
  | { tag: "case-tuple-like-arg'"; arg: number }
  | { tag: "case-tuple-like-exactly'"; Argument: number }
  | { tag: "case-tuple-like-kind-name'"; "case-tuple-like-kind-name'": number }
  | { tag: "case-tuple-like-kind-name-no-mangling"; Case_tuple_like_kind_name_no_mangling: number }
  | {
      tag: "case-tuple-like-kind-name-no-mangling-with-ctor-name";
      "case-tuple-like-kind-name-no-mangling-with-ctor-name": number;
    };
export function analyzeExVariantCustomizedUnion<__bindoj_ret>(__bindoj_fns: {
  "case-inline-record'": (__bindoj_v: { tag: "case-inline-record'"; "x'": number; "y'": number }) => __bindoj_ret;
  "case-tuple-like-arg'": (__bindoj_v: { tag: "case-tuple-like-arg'"; arg: number }) => __bindoj_ret;
  "case-tuple-like-exactly'": (__bindoj_v: { tag: "case-tuple-like-exactly'"; Argument: number }) => __bindoj_ret;
  "case-tuple-like-kind-name'": (__bindoj_v: {
    tag: "case-tuple-like-kind-name'";
    "case-tuple-like-kind-name'": number;
  }) => __bindoj_ret;
  "case-tuple-like-kind-name-no-mangling": (__bindoj_v: {
    tag: "case-tuple-like-kind-name-no-mangling";
    Case_tuple_like_kind_name_no_mangling: number;
  }) => __bindoj_ret;
  "case-tuple-like-kind-name-no-mangling-with-ctor-name": (__bindoj_v: {
    tag: "case-tuple-like-kind-name-no-mangling-with-ctor-name";
    "case-tuple-like-kind-name-no-mangling-with-ctor-name": number;
  }) => __bindoj_ret;
}): (__bindoj_x: ExVariantCustomizedUnion) => __bindoj_ret {
  return (__bindoj_x: ExVariantCustomizedUnion) => {
    if (__bindoj_x.tag === "case-inline-record'") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "case-tuple-like-arg'") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "case-tuple-like-exactly'") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "case-tuple-like-kind-name'") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "case-tuple-like-kind-name-no-mangling") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "case-tuple-like-kind-name-no-mangling-with-ctor-name") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeExVariantCustomizedUnion - unrecognized: " + __bindoj_x);
    }
  };
}
