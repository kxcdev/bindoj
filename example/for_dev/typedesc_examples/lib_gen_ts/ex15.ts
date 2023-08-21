type IntList = { kind: "intcons"; arg: [number, IntList] } | { kind: "intnil" };
type Student = { admissionYear: number; name: string };
export type NestedVariant =
  | { tag: "int-list1"; value: IntList }
  | ({ tag: "int-list2" } & IntList)
  | { tag: "student1"; student: Student }
  | ({ tag: "student2" } & Student)
  | { tag: "student3"; value: Student }
  | ({ tag: "student4" } & Student);
export function analyzeNestedVariant<__bindoj_ret>(__bindoj_fns: {
  "int-list1": (__bindoj_v: { tag: "int-list1"; value: IntList }) => __bindoj_ret;
  "int-list2": (__bindoj_v: { tag: "int-list2" } & IntList) => __bindoj_ret;
  student1: (__bindoj_v: { tag: "student1"; student: Student }) => __bindoj_ret;
  student2: (__bindoj_v: { tag: "student2" } & Student) => __bindoj_ret;
  student3: (__bindoj_v: { tag: "student3"; value: Student }) => __bindoj_ret;
  student4: (__bindoj_v: { tag: "student4" } & Student) => __bindoj_ret;
}): (__bindoj_x: NestedVariant) => __bindoj_ret {
  return (__bindoj_x: NestedVariant) => {
    if (__bindoj_x.tag === "int-list1") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "int-list2") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "student1") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "student2") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "student3") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else if (__bindoj_x.tag === "student4") {
      return __bindoj_fns[__bindoj_x.tag](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeNestedVariant - unrecognized: " + __bindoj_x);
    }
  };
}
