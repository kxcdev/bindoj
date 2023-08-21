export type person_inherited_mangling =
  | { kind: "Anonymous" }
  | { kind: "student"; caseValue: "Case_at0" | "case-at1"; name: string; student_id: number }
  | { kind: "Teacher"; department: string; facultyId: number; name: string }
  | { kind: "With_id"; arg: number };
export function analyze_person_inherited_mangling<__bindoj_ret>(__bindoj_fns: {
  Anonymous: (__bindoj_v: { kind: "Anonymous" }) => __bindoj_ret;
  student: (__bindoj_v: {
    kind: "student";
    caseValue: "Case_at0" | "case-at1";
    name: string;
    student_id: number;
  }) => __bindoj_ret;
  Teacher: (__bindoj_v: { kind: "Teacher"; department: string; facultyId: number; name: string }) => __bindoj_ret;
  With_id: (__bindoj_v: { kind: "With_id"; arg: number }) => __bindoj_ret;
}): (__bindoj_x: person_inherited_mangling) => __bindoj_ret {
  return (__bindoj_x: person_inherited_mangling) => {
    if (__bindoj_x.kind === "Anonymous") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "student") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "Teacher") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "With_id") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyze_person_inherited_mangling - unrecognized: " + __bindoj_x);
    }
  };
}
