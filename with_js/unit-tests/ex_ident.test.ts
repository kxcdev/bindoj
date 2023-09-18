import * as ex from "../compile-tests/ex_ident_gen";
import * as samples_tmp from "../compile-tests/ex_ident_examples.json";
import * as schema_tmp from "../compile-tests/ex_ident_schema.json";
import * as schema_record from "../compile-tests/ex_record_schema.json";
import { Schema } from "jsonschema";
import { validationTest } from "./helper";

const allSchema: Schema[] = <Schema[]>(<unknown>schema_tmp);
const refs: Schema[] = <Schema[]>(<unknown>schema_record);

describe("ex_ident", (): void => {
  describe("ExIdentStudentPair", () => {
    // also typecheck the generate JSON
    const samples: ex.ExIdentStudentPair[] = <ex.ExIdentStudentPair[]>samples_tmp.ExIdentStudentPair;

    test("it compiles", (): void => {
      const student = { admissionYear: 1984, name: "William Gibson" };
      expect(samples[0]).toStrictEqual({ student1: student, student2: student });
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExIdentStudentPair", refs));
  });
});
