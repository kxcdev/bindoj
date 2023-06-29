export type MyInt = number;

export type MyTuple = { _0: number; _1: string };

export type Student = { admissionYear: number; name: string };

export type Person =
  | { kind: "anonymous" }
  | { kind: "student"; name: string; studentId: number }
  | { kind: "teacher"; department: string; facultyId: number; name: string }
  | { kind: "with-id"; arg: number };
export function analyzePerson<__bindoj_ret>(__bindoj_fns: {
  anonymous: (__bindoj_v: { kind: "anonymous" }) => __bindoj_ret;
  student: (__bindoj_v: { kind: "student"; name: string; studentId: number }) => __bindoj_ret;
  teacher: (__bindoj_v: { kind: "teacher"; department: string; facultyId: number; name: string }) => __bindoj_ret;
  "with-id": (__bindoj_v: { kind: "with-id"; arg: number }) => __bindoj_ret;
}): (__bindoj_x: Person) => __bindoj_ret {
  return (__bindoj_x: Person) => {
    if (__bindoj_x.kind === "anonymous") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "student") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "teacher") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "with-id") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzePerson - unrecognized: " + __bindoj_x);
    }
  };
}
