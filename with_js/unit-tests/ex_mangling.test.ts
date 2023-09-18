import * as ex from "../compile-tests/ex_mangling_gen";
import * as samples_tmp from "../compile-tests/ex_mangling_examples.json";
import * as schema_tmp from "../compile-tests/ex_mangling_schema.json";
import { Schema } from "jsonschema";
import { validationTest } from "./helper";

const allSchema: Schema[] = <Schema[]>(<unknown>schema_tmp);

type AnalyzeResult = string | number | [string, number] | [string, number, string];

describe("ex_mangling", (): void => {
  describe("ex_mangling_student_inherited", () => {
    // also typecheck the generate JSON
    const samples: ex.ex_mangling_student_inherited[] = <ex.ex_mangling_student_inherited[]>(
      samples_tmp.ex_mangling_student_inherited
    );

    test("it compiles", (): void => {
      expect(samples[0].admission_year).toBe(1984);
      expect(samples[0].name).toBe("William Gibson");
      expect(samples[0].caseValue).toBe("Case-at0");

      expect(samples[1].admission_year).toBe(2001);
      expect(samples[1].name).toBe("Arthur C. Clark");
      expect(samples[1].caseValue).toBe("case_at1");
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ex_mangling_student_inherited"));
  });

  describe("ex_mangling_person_no_mangling", () => {
    // also typecheck the generate JSON
    const samples: ex.ex_mangling_person_no_mangling[] = <ex.ex_mangling_person_no_mangling[]>(
      samples_tmp.ex_mangling_person_no_mangling
    );

    test("schema validates all the examples", validationTest(allSchema, samples, "#ex_mangling_person_no_mangling"));

    test("the case analyzer works", (): void => {
      const analyzer = ex.analyze_ex_mangling_person_no_mangling<AnalyzeResult>({
        Anonymous: (_) => "Anonymous",
        With_id: (v) => v.arg,
        Student: (v) => [v.name, v.student_id],
        Teacher: (v) => [v.name, v.faculty_id, v.department],
      });
      expect(analyzer(samples[0])).toBe("Anonymous");
      expect(analyzer(samples[1])).toBe(1619);
      expect(analyzer(samples[2])).toStrictEqual(["Ray Bradbury", 451]);
      expect(analyzer(samples[3])).toStrictEqual(["Arthur C. Clark", 2001, "Space"]);
    });
  });

  describe("ex_mangling_person_inherited", () => {
    // also typecheck the generate JSON
    const samples: ex.ex_mangling_person_inherited[] = <ex.ex_mangling_person_inherited[]>(
      samples_tmp.ex_mangling_person_inherited
    );

    test("schema validates all the examples", validationTest(allSchema, samples, "#ex_mangling_person_inherited"));

    test("the case analyzer works", (): void => {
      const analyzer = ex.analyze_ex_mangling_person_inherited<AnalyzeResult>({
        Anonymous: (_) => "Anonymous",
        With_id: (v) => v.arg,
        student: (v) => [v.name, v.student_id, v.caseValue],
        Teacher: (v) => [v.name, v.facultyId, v.department],
      });
      expect(analyzer(samples[0])).toBe("Anonymous");
      expect(analyzer(samples[1])).toBe(1619);
      expect(analyzer(samples[2])).toStrictEqual(["Ray Bradbury", 451, "Case_at0"]);
      expect(analyzer(samples[3])).toStrictEqual(["Ray Bradbury", 451, "case-at1"]);
      expect(analyzer(samples[4])).toStrictEqual(["Arthur C. Clark", 2001, "Space"]);
    });
  });

  describe("ExManglingEnum", () => {
    // also typecheck the generate JSON
    const samples: ex.ExManglingEnum[] = <ex.ExManglingEnum[]>samples_tmp.ExManglingEnum;

    test("it compiles", (): void => {
      expect(samples[0]).toBe("Case_at0");
      expect(samples[1]).toBe("case-at1");
      expect(samples[2]).toBe("Case-at2");
      expect(samples[3]).toBe("Case-third");
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExManglingEnum"));
  });
});
