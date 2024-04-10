import { ExNestedRecord, ExNestedVariant } from "./ex_nested_gen";
type ExAliasUnit = 1;
type ex_mangling_person_inherited =
  | { kind: "Anonymous" }
  | { kind: "student"; caseValue: "Case_at0" | "case-at1"; name: string; student_id: number }
  | { kind: "Teacher"; department: string; facultyId: number; name: string }
  | { kind: "With_id"; value: number };
type ExNestedPoint2 = { x: number; y: number };
type ExOptionalVariant =
  | { tag: "inline-record"; intOpt?: number; objtuple: { _0?: number; _1?: number }; xOpt?: number; yOpt?: number }
  | ({ tag: "inline-record-spreading"; intOpt?: number } & ExOptionalXyOpt)
  | ({ tag: "reused-inline-record" } & ExOptionalXyOpt)
  | { tag: "tuple-like"; arg?: number }
  | { tag: "tuple-like-alias"; arg?: number }
  | { tag: "tuple-like-obj"; _0?: number; _1?: number }
  | ({ tag: "tuple-like-spreading" } & ExOptionalXyOpt);
type ExOptionalXyOpt = { xOpt?: number; yOpt?: number };
type ExRecordStudent = { admissionYear: number; name: string };
type ExVariantIntList = { kind: "intcons"; value: [number, ExVariantIntList] } | { kind: "intnil" };
export type ExNestedMultiplyRecord = { nestedRecord: ExNestedRecord } & ExNestedRecord;

export type ExNestedMultiplyVariant =
  | ({ label: "nested-record"; nestedRecord: ExNestedRecord } & ExNestedRecord)
  | ({ label: "nested-variant"; nestedVariant: ExNestedVariant } & ExNestedVariant);
export function analyzeExNestedMultiplyVariant<__bindoj_ret>(__bindoj_fns: {
  "nested-record": (
    __bindoj_v: { label: "nested-record"; nestedRecord: ExNestedRecord } & ExNestedRecord
  ) => __bindoj_ret;
  "nested-variant": (
    __bindoj_v: { label: "nested-variant"; nestedVariant: ExNestedVariant } & ExNestedVariant
  ) => __bindoj_ret;
}): (__bindoj_x: ExNestedMultiplyVariant) => __bindoj_ret {
  return (__bindoj_x: ExNestedMultiplyVariant) => {
    if (__bindoj_x.label === "nested-record") {
      return __bindoj_fns[__bindoj_x.label](__bindoj_x);
    } else if (__bindoj_x.label === "nested-variant") {
      return __bindoj_fns[__bindoj_x.label](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeExNestedMultiplyVariant - unrecognized: " + __bindoj_x);
    }
  };
}
