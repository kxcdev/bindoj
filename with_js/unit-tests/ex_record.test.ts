import * as ex from "../compile-tests/ex_record_gen";
import * as samples_tmp from "../compile-tests/ex_record_examples.json";
import * as schema_tmp from "../compile-tests/ex_record_schema.json";
import { Schema } from "jsonschema";
import { validationTest } from "./helper";

const allSchema: Schema[] = <Schema[]>(<unknown>schema_tmp);

describe("ex_record", (): void => {
  describe("ExRecordStudent", () => {
    // also typecheck the generate JSON
    const samples: ex.ExRecordStudent[] = <ex.ExRecordStudent[]>samples_tmp.ExRecordStudent;

    test("it compiles", (): void => {
      expect(samples[0].admissionYear).toBe(1984);
      expect(samples[0].name).toBe("William Gibson");
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExRecordStudent"));
  });

  describe("ExRecordTeacher", () => {
    // also typecheck the generate JSON
    const samples: ex.ExRecordTeacher[] = <ex.ExRecordTeacher[]>samples_tmp.ExRecordTeacher;

    test("it compiles", () => {
      expect(samples[0].facultyId).toBe(2001);
      expect(samples[0].name).toBe("Arthur C. Clark");
      expect(samples[0].department).toBe("Space");
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExRecordTeacher"));
  });
});
