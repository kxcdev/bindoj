import * as ex01 from "../compile-tests/ex01_gen"
import * as tmp from "../compile-tests/ex01_examples.json"
import * as schema from "../compile-tests/ex01_schema.json"
import {Validator} from "jsonschema"
var validator = new Validator();

// also typecheck the generate JSON
const samples : ex01.student[] = <ex01.student[]>tmp;

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

