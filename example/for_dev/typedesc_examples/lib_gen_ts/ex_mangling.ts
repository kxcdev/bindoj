export type ex_mangling_student_inherited = {
  admission_year: number;
  caseValue: "Case-at0" | "case_at1";
  name: string;
};

export type ex_mangling_person_no_mangling =
  | { kind: "Anonymous" }
  | { kind: "Student"; name: string; student_id: number }
  | { kind: "Teacher"; department: string; faculty_id: number; name: string }
  | { kind: "With_id"; value: number };
export function analyze_ex_mangling_person_no_mangling<__bindoj_ret>(__bindoj_fns: {
  Anonymous: (__bindoj_v: { kind: "Anonymous" }) => __bindoj_ret;
  Student: (__bindoj_v: { kind: "Student"; name: string; student_id: number }) => __bindoj_ret;
  Teacher: (__bindoj_v: { kind: "Teacher"; department: string; faculty_id: number; name: string }) => __bindoj_ret;
  With_id: (__bindoj_v: { kind: "With_id"; value: number }) => __bindoj_ret;
}): (__bindoj_x: ex_mangling_person_no_mangling) => __bindoj_ret {
  return (__bindoj_x: ex_mangling_person_no_mangling) => {
    if (__bindoj_x.kind === "Anonymous") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "Student") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "Teacher") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "With_id") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyze_ex_mangling_person_no_mangling - unrecognized: " + __bindoj_x);
    }
  };
}

export type ex_mangling_person_inherited =
  | { kind: "Anonymous" }
  | { kind: "student"; caseValue: "Case_at0" | "case-at1"; name: string; student_id: number }
  | { kind: "Teacher"; department: string; facultyId: number; name: string }
  | { kind: "With_id"; value: number };
export function analyze_ex_mangling_person_inherited<__bindoj_ret>(__bindoj_fns: {
  Anonymous: (__bindoj_v: { kind: "Anonymous" }) => __bindoj_ret;
  student: (__bindoj_v: {
    kind: "student";
    caseValue: "Case_at0" | "case-at1";
    name: string;
    student_id: number;
  }) => __bindoj_ret;
  Teacher: (__bindoj_v: { kind: "Teacher"; department: string; facultyId: number; name: string }) => __bindoj_ret;
  With_id: (__bindoj_v: { kind: "With_id"; value: number }) => __bindoj_ret;
}): (__bindoj_x: ex_mangling_person_inherited) => __bindoj_ret {
  return (__bindoj_x: ex_mangling_person_inherited) => {
    if (__bindoj_x.kind === "Anonymous") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "student") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "Teacher") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "With_id") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyze_ex_mangling_person_inherited - unrecognized: " + __bindoj_x);
    }
  };
}

export type ExManglingEnum = "Case_at0" | "case-at1" | "Case-at2" | "Case-third";
