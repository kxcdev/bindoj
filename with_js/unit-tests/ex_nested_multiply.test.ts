import * as ex from "../compile-tests/ex_nested_multiply_gen";
import * as samples_tmp from "../compile-tests/ex_nested_multiply_examples.json";
import * as schema_tmp from "../compile-tests/ex_nested_multiply_schema.json";
import * as schema_variant from "../compile-tests/ex_variant_schema.json";
import { Schema } from "jsonschema";
import { validationTest } from "./helper";

const allSchema: Schema[] = <Schema[]>(<unknown>schema_tmp);
const refs: Schema[] = <Schema[]>(<unknown>schema_variant);

describe("ex_nested_multiply", (): void => {
  describe("ExNestedMultiplyRecord", () => {
    // also typecheck the generate JSON
    const samples: ex.ExNestedMultiplyRecord[] = <ex.ExNestedMultiplyRecord[]>samples_tmp.ExNestedMultiplyRecord;

    test("it compiles", (): void => {
      expect(samples).toStrictEqual([
        {
          nestedRecord: {
            unit: 1,
            point2: { x: 1, y: 2 },
            x: 1,
            y: 2,
            person: { kind: "Anonymous" },
            kind: "Anonymous",
          },
          unit: 1,
          point2: { x: 1, y: 2 },
          x: 1,
          y: 2,
          person: { kind: "Anonymous" },
          kind: "Anonymous",
        },
        {
          nestedRecord: {
            unit: 1,
            point2: { x: 1, y: 2 },
            x: 1,
            y: 2,
            person: { kind: "With_id", value: 1619 },
            kind: "With_id",
            value: 1619,
          },
          unit: 1,
          point2: { x: 1, y: 2 },
          x: 1,
          y: 2,
          person: { kind: "With_id", value: 1619 },
          kind: "With_id",
          value: 1619,
        },
        {
          nestedRecord: {
            unit: 1,
            point2: { x: 1, y: 2 },
            x: 1,
            y: 2,
            person: { kind: "student", student_id: 451, name: "Ray Bradbury", caseValue: "Case_at0" },
            kind: "student",
            student_id: 451,
            name: "Ray Bradbury",
            caseValue: "Case_at0",
          },
          unit: 1,
          point2: { x: 1, y: 2 },
          x: 1,
          y: 2,
          person: { kind: "student", student_id: 451, name: "Ray Bradbury", caseValue: "Case_at0" },
          kind: "student",
          student_id: 451,
          name: "Ray Bradbury",
          caseValue: "Case_at0",
        },
        {
          nestedRecord: {
            unit: 1,
            point2: { x: 1, y: 2 },
            x: 1,
            y: 2,
            person: { kind: "Teacher", facultyId: 2001, name: "Arthur C. Clark", department: "Space" },
            kind: "Teacher",
            facultyId: 2001,
            name: "Arthur C. Clark",
            department: "Space",
          },
          unit: 1,
          point2: { x: 1, y: 2 },
          x: 1,
          y: 2,
          person: { kind: "Teacher", facultyId: 2001, name: "Arthur C. Clark", department: "Space" },
          kind: "Teacher",
          facultyId: 2001,
          name: "Arthur C. Clark",
          department: "Space",
        },
      ]);
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExNestedMultiplyRecord"));
  });

  describe("ExNestedMultiplyVariant", () => {
    // also typecheck the generate JSON
    const samples: ex.ExNestedMultiplyVariant[] = <ex.ExNestedMultiplyVariant[]>samples_tmp.ExNestedMultiplyVariant;

    test("it compiles", () => {
      expect(samples).toStrictEqual([
        {
          label: "nested-record",
          nestedRecord: {
            unit: 1,
            point2: { x: 1, y: 2 },
            x: 1,
            y: 2,
            person: { kind: "Anonymous" },
            kind: "Anonymous",
          },
          unit: 1,
          point2: { x: 1, y: 2 },
          x: 1,
          y: 2,
          person: { kind: "Anonymous" },
          kind: "Anonymous",
        },
        {
          label: "nested-record",
          nestedRecord: {
            unit: 1,
            point2: { x: 1, y: 2 },
            x: 1,
            y: 2,
            person: { kind: "With_id", value: 1619 },
            kind: "With_id",
            value: 1619,
          },
          unit: 1,
          point2: { x: 1, y: 2 },
          x: 1,
          y: 2,
          person: { kind: "With_id", value: 1619 },
          kind: "With_id",
          value: 1619,
        },
        {
          label: "nested-record",
          nestedRecord: {
            unit: 1,
            point2: { x: 1, y: 2 },
            x: 1,
            y: 2,
            person: { kind: "student", student_id: 451, name: "Ray Bradbury", caseValue: "Case_at0" },
            kind: "student",
            student_id: 451,
            name: "Ray Bradbury",
            caseValue: "Case_at0",
          },
          unit: 1,
          point2: { x: 1, y: 2 },
          x: 1,
          y: 2,
          person: { kind: "student", student_id: 451, name: "Ray Bradbury", caseValue: "Case_at0" },
          kind: "student",
          student_id: 451,
          name: "Ray Bradbury",
          caseValue: "Case_at0",
        },
        {
          label: "nested-record",
          nestedRecord: {
            unit: 1,
            point2: { x: 1, y: 2 },
            x: 1,
            y: 2,
            person: { kind: "Teacher", facultyId: 2001, name: "Arthur C. Clark", department: "Space" },
            kind: "Teacher",
            facultyId: 2001,
            name: "Arthur C. Clark",
            department: "Space",
          },
          unit: 1,
          point2: { x: 1, y: 2 },
          x: 1,
          y: 2,
          person: { kind: "Teacher", facultyId: 2001, name: "Arthur C. Clark", department: "Space" },
          kind: "Teacher",
          facultyId: 2001,
          name: "Arthur C. Clark",
          department: "Space",
        },
        {
          label: "nested-variant",
          nestedVariant: { tag: "student1", student: { admissionYear: 1984, name: "William Gibson" } },
          tag: "student1",
          student: { admissionYear: 1984, name: "William Gibson" },
        },
        {
          label: "nested-variant",
          nestedVariant: { tag: "student2", admissionYear: 1984, name: "William Gibson" },
          tag: "student2",
          admissionYear: 1984,
          name: "William Gibson",
        },
        {
          label: "nested-variant",
          nestedVariant: { tag: "student3", arg: { admissionYear: 1984, name: "William Gibson" } },
          tag: "student3",
          arg: { admissionYear: 1984, name: "William Gibson" },
        },
        {
          label: "nested-variant",
          nestedVariant: { tag: "student4", admissionYear: 1984, name: "William Gibson" },
          tag: "student4",
          admissionYear: 1984,
          name: "William Gibson",
        },
        {
          label: "nested-variant",
          nestedVariant: {
            tag: "int-list1",
            arg: { kind: "intcons", value: [1, { kind: "intcons", value: [2, { kind: "intnil" }] }] },
          },
          tag: "int-list1",
          arg: { kind: "intcons", value: [1, { kind: "intcons", value: [2, { kind: "intnil" }] }] },
        },
        {
          label: "nested-variant",
          nestedVariant: {
            tag: "int-list2",
            kind: "intcons",
            value: [1, { kind: "intcons", value: [2, { kind: "intnil" }] }],
          },
          tag: "int-list2",
          kind: "intcons",
          value: [1, { kind: "intcons", value: [2, { kind: "intnil" }] }],
        },
      ]);
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExNestedMultiplyVariant", refs));
  });
});
