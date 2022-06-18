import * as ex02 from "../compile-tests/ex02_gen"
import * as samples_tmp from "../compile-tests/ex02_examples.json"
import * as schema_tmp from "../compile-tests/ex02_schema.json"
import {Schema, Validator} from "jsonschema"
var validator = new Validator();

// also typecheck the generate JSON
const samples : ex02.person[] = <ex02.person[]>samples_tmp;
const schema : Schema = <Schema><unknown>schema_tmp;

type AnalyzeResult = string | number | [string, number] | [string, number, string]

describe('ex02', (): void => {
  test('it compiles', (): void => {
    return;
  });

  test('schema validates all the examples', (): void => {
    samples.forEach(sample => {
      const result = validator.validate(sample, schema);
      expect(result.valid);
      expect(result.errors).toStrictEqual([]);
    });
    return;
  });

  test('the case analyzer works', (): void => {
    let analyzer =
      ex02.analyze_person<AnalyzeResult>({
        Anonymous: _ => "anonymous",
        With_id:   v => v.arg,
        Student:   v => [v.name, v.student_id],
        Teacher:   v => [v.name, v.faculty_id, v.department]
      });
    expect(analyzer(samples[0])).toBe("anonymous")
    expect(analyzer(samples[1])).toBe(1619)
    expect(analyzer(samples[2])).toStrictEqual(["Ray Bradbury", 451])
    expect(analyzer(samples[3])).toStrictEqual(["Arthur C. Clark", 2001, "Space"])
  });
})
