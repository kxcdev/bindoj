import { ExVariantIntList } from "./ex_variant_gen";
import { ExRecordStudent } from "./ex_record_gen";
import { ex_mangling_person_inherited } from "./ex_mangling_gen";
import { ExAliasUnit } from "./ex_alias_gen";
export type ExNestedPoint2 = { x: number; y: number };

export type ExNestedRecord = {
  person: ex_mangling_person_inherited;
  point2: ExNestedPoint2;
  unit: ExAliasUnit;
} & ex_mangling_person_inherited &
  ExNestedPoint2;

export type ExNestedVariant =
  | { tag: "int-list1"; arg: ExVariantIntList }
  | ({ tag: "int-list2" } & ExVariantIntList)
  | { tag: "student1"; student: ExRecordStudent }
  | ({ tag: "student2" } & ExRecordStudent)
  | { tag: "student3"; arg: ExRecordStudent }
  | ({ tag: "student4" } & ExRecordStudent);
export function analyzeExNestedVariant<__bindoj_ret>(__bindoj_fns: {
  "int-list1": (__bindoj_v: { tag: "int-list1"; arg: ExVariantIntList }) => __bindoj_ret;
  "int-list2": (__bindoj_v: { tag: "int-list2" } & ExVariantIntList) => __bindoj_ret;
  student1: (__bindoj_v: { tag: "student1"; student: ExRecordStudent }) => __bindoj_ret;
  student2: (__bindoj_v: { tag: "student2" } & ExRecordStudent) => __bindoj_ret;
  student3: (__bindoj_v: { tag: "student3"; arg: ExRecordStudent }) => __bindoj_ret;
  student4: (__bindoj_v: { tag: "student4" } & ExRecordStudent) => __bindoj_ret;
}): (__bindoj_x: ExNestedVariant) => __bindoj_ret {
  return (__bindoj_x: ExNestedVariant) => {
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
      throw new TypeError("panic @analyzeExNestedVariant - unrecognized: " + __bindoj_x);
    }
  };
}
