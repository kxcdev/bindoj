import * as ex from "../compile-tests/ex_variant_gen";
import * as samples_tmp from "../compile-tests/ex_variant_examples.json";
import * as schema_tmp from "../compile-tests/ex_variant_schema.json";
import { Schema } from "jsonschema";
import { validationTest } from "./helper";

const allSchema: Schema[] = <Schema[]>(<unknown>schema_tmp);

type AnalyzeResult = string | number | [string, number] | [string, number, string];

describe("ex_variant", (): void => {
  test("it compiles", (): void => {
    return;
  });

  describe("ExVariantPerson", () => {
    // also typecheck the generate JSON
    const samples: ex.ExVariantPerson[] = <ex.ExVariantPerson[]>samples_tmp.ExVariantPerson;

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExVariantPerson"));

    test("the case analyzer works", (): void => {
      const analyzer = ex.analyzeExVariantPerson<AnalyzeResult>({
        anonymous: (_) => "Anonymous",
        "with-id": (v) => v.arg,
        student: (v) => [v.name, v.studentId],
        teacher: (v) => [v.name, v.facultyId, v.department],
      });
      expect(analyzer(samples[0])).toBe("Anonymous");
      expect(analyzer(samples[1])).toBe(1619);
      expect(analyzer(samples[2])).toStrictEqual(["Ray Bradbury", 451]);
      expect(analyzer(samples[3])).toStrictEqual(["Arthur C. Clark", 2001, "Space"]);
    });
  });

  describe("ExVariantPersonReused", () => {
    // also typecheck the generate JSON
    const samples: ex.ExVariantPersonReused[] = <ex.ExVariantPersonReused[]>samples_tmp.ExVariantPersonReused;

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExVariantPersonReused"));

    test("the case analyzer works", (): void => {
      const analyzer = ex.analyzeExVariantPersonReused<AnalyzeResult>({
        anonymous: (_) => "Anonymous",
        "with-id": (v) => v.arg,
        student: (v) => [v.name, v.studentId],
        teacher: (v) => [v.name, v.facultyId, v.department],
      });
      expect(analyzer(samples[0])).toBe("Anonymous");
      expect(analyzer(samples[1])).toBe(1619);
      expect(analyzer(samples[2])).toStrictEqual(["Ray Bradbury", 451]);
      expect(analyzer(samples[3])).toStrictEqual(["Arthur C. Clark", 2001, "Space"]);
    });
  });

  describe("ExVariantIntList", () => {
    // also typecheck the generate JSON
    const samples: ex.ExVariantIntList[] = <ex.ExVariantIntList[]>samples_tmp.ExVariantIntList;

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExVariantIntList"));

    test("the case analyzer works", (): void => {
      function analyzer(x: ex.ExVariantIntList): number {
        return ex.analyzeExVariantIntList({
          intcons: (v) => v.arg[0] + analyzer(v.arg[1]),
          intnil: (_) => 0,
        })(x);
      }

      expect(analyzer(samples[0])).toBe(0);
      expect(analyzer(samples[1])).toBe(3);
      expect(analyzer(samples[2])).toBe(10);
    });
  });

  describe("ExVariantIntListObjtuple", () => {
    // also typecheck the generate JSON
    const samples: ex.ExVariantIntListObjtuple[] = <ex.ExVariantIntListObjtuple[]>samples_tmp.ExVariantIntListObjtuple;

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExVariantIntListObjtuple"));

    test("the case analyzer works", (): void => {
      function analyzer(x: ex.ExVariantIntListObjtuple): number {
        return ex.analyzeExVariantIntListObjtuple({
          intcons: (v) => v._0 + analyzer(v._1),
          intnil: (_) => 0,
        })(x);
      }

      expect(analyzer(samples[0])).toBe(0);
      expect(analyzer(samples[1])).toBe(3);
      expect(analyzer(samples[2])).toBe(10);
    });
  });

  describe("ExVariantFoo", () => {
    // also typecheck the generate JSON
    const samples: ex.ExVariantFoo[] = <ex.ExVariantFoo[]>samples_tmp.ExVariantFoo;

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExVariantFoo"));

    test("the case analyzer works", (): void => {
      function analyzer(x: ex.ExVariantFoo) {
        return ex.analyzeExVariantFoo<number>({
          foo0: (_) => 0,
          foo1: (v) => v.arg,
          foo2: (v) => v.arg.reduce((x, y) => x + y, 0),
        })(x);
      }

      expect(analyzer(samples[0])).toBe(0);
      expect(analyzer(samples[1])).toBe(1);
      expect(analyzer(samples[2])).toBe(3);
    });
  });

  describe("ExVariantCustomizedUnion", () => {
    // also typecheck the generate JSON
    const samples: ex.ExVariantCustomizedUnion[] = <ex.ExVariantCustomizedUnion[]>samples_tmp.ExVariantCustomizedUnion;

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExVariantCustomizedUnion"));

    test("the case analyzer works", (): void => {
      function analyzer(x: ex.ExVariantCustomizedUnion) {
        return ex.analyzeExVariantCustomizedUnion<number>({
          "case1'": (v) => v.value,
          "case2'": (v) => v["x'"] + v["y'"],
        })(x);
      }

      expect(analyzer(samples[0])).toBe(42);
      expect(analyzer(samples[1])).toBe(6);
    });
  });
});
