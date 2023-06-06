import * as ex03 from "../compile-tests/ex03_gen"
import * as samples_tmp from "../compile-tests/ex03_examples.json"
import * as schema_tmp from "../compile-tests/ex03_schema.json"
import {Schema,Validator} from "jsonschema"
var validator = new Validator();

// also typecheck the generate JSON
const samples : ex03.IntList[] = <ex03.IntList[]>samples_tmp;
const schema : Schema = <Schema><unknown>schema_tmp;

describe('ex03', (): void => {
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
    function analyzer(x: ex03.IntList) : number {
      return ex03.analyzeIntList({
        intcons: v => v.arg[0] + analyzer(v.arg[1]),
        intnil:  _ => 0
      })(x);
    };

    expect(analyzer(samples[0])).toBe(0)
    expect(analyzer(samples[1])).toBe(3)
    expect(analyzer(samples[2])).toBe(10)
  });
})

