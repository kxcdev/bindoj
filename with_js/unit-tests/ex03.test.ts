import * as ex03 from "../compile-tests/ex03_gen"
import * as tmp from "../compile-tests/ex03_examples.json"
import * as schema from "../compile-tests/ex03_schema.json"
import {Validator} from "jsonschema"
var validator = new Validator();

// also typecheck the generate JSON
const samples : ex03.int_list[] = <ex03.int_list[]>tmp;

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
    function analyzer(x: ex03.int_list) : number {
      return ex03.analyze_int_list({
        IntCons: v => v.arg[0] + analyzer(v.arg[1]),
        IntNil:  _ => 0
      })(x);
    };

    expect(analyzer(samples[0])).toBe(0)
    expect(analyzer(samples[1])).toBe(3)
    expect(analyzer(samples[2])).toBe(10)
  });
})

