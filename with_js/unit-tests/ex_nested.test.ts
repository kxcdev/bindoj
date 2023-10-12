import * as ex from "../compile-tests/ex_nested_gen";
import * as samples_tmp from "../compile-tests/ex_nested_examples.json";
import * as schema_tmp from "../compile-tests/ex_nested_schema.json";
import * as schema_variant from "../compile-tests/ex_variant_schema.json";
import { Schema } from "jsonschema";
import { validationTest } from "./helper";

const allSchema: Schema[] = <Schema[]>(<unknown>schema_tmp);
const refs: Schema[] = <Schema[]>(<unknown>schema_variant);

describe("ex_nested", (): void => {
  describe("ExNestedPoint2", () => {
    // also typecheck the generate JSON
    const samples: ex.ExNestedPoint2[] = <ex.ExNestedPoint2[]>samples_tmp.ExNestedPoint2;

    test("it compiles", (): void => {
      expect(samples[0].x).toBe(1);
      expect(samples[0].y).toBe(2);
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExNestedPoint2"));
  });

  describe("ExNestedRecord", () => {
    // also typecheck the generate JSON
    const samples: ex.ExNestedRecord[] = <ex.ExNestedRecord[]>samples_tmp.ExNestedRecord;

    test("it compiles", (): void => {
      expect(samples).toStrictEqual([
        { unit: 1, point2: { x: 1, y: 2 }, x: 1, y: 2, person: { kind: "Anonymous" }, kind: "Anonymous" },
        {
          unit: 1,
          point2: { x: 1, y: 2 },
          x: 1,
          y: 2,
          person: { kind: "With_id", value: 1619 },
          kind: "With_id",
          value: 1619,
        },
        {
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

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExNestedRecord"));
  });

  describe("ExNestedVariant", () => {
    // also typecheck the generate JSON
    const samples: ex.ExNestedVariant[] = <ex.ExNestedVariant[]>samples_tmp.ExNestedVariant;

    test("it compiles", () => {
      expect(samples).toStrictEqual([
        { tag: "student1", student: { admissionYear: 1984, name: "William Gibson" } },
        { tag: "student2", admissionYear: 1984, name: "William Gibson" },
        { tag: "student3", arg: { admissionYear: 1984, name: "William Gibson" } },
        { tag: "student4", admissionYear: 1984, name: "William Gibson" },
        { tag: "int-list1", arg: { kind: "intcons", value: [1, { kind: "intcons", value: [2, { kind: "intnil" }] }] } },
        { tag: "int-list2", kind: "intcons", value: [1, { kind: "intcons", value: [2, { kind: "intnil" }] }] },
      ]);
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExNestedVariant", refs));
  });
});
