import * as ex01 from "../compile-tests/ex01_gen"
import * as samples_tmp from "../compile-tests/ex01_examples.json"
import * as schema_tmp from "../compile-tests/ex01_schema.json"
import {Schema, Validator} from "jsonschema"
var validator = new Validator();

// also typecheck the generate JSON
const samples : ex01.student[] = <ex01.student[]>samples_tmp;
const schema : Schema = <Schema><unknown>schema_tmp;

describe('ex01', (): void => {
  test('it compiles', (): void => {
    expect(samples[0].admission_year).toBe(1984);
    expect(samples[0].name).toBe("William Gibson");
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
})

