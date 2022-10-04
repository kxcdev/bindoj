import * as ex03 from "../compile-tests/ex03_objtuple_gen"
import * as samples_tmp from "../compile-tests/ex03_objtuple_examples.json"
import * as schema_tmp from "../compile-tests/ex03_objtuple_schema.json"
import {Schema,Validator} from "jsonschema"
var validator = new Validator();

// also typecheck the generate JSON
const samples : ex03.int_list[] = <ex03.int_list[]>samples_tmp;
const schema : Schema = <Schema><unknown>schema_tmp;

describe('ex03_objtuple', (): void => {
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
        IntCons: v => v._0 + analyzer(v._1),
        IntNil:  _ => 0
      })(x);
    };

    expect(analyzer(samples[0])).toBe(0)
    expect(analyzer(samples[1])).toBe(3)
    expect(analyzer(samples[2])).toBe(10)
  });
})

