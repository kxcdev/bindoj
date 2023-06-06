import * as ex04 from "../compile-tests/ex04_gen"
import * as samples_tmp from "../compile-tests/ex04_examples.json"
import * as schema_tmp from "../compile-tests/ex04_schema.json"
import {Schema,Validator} from "jsonschema"
var validator = new Validator();

// also typecheck the generate JSON
const samples : ex04.Foo[] = <ex04.Foo[]>samples_tmp;
const schema : Schema = <Schema><unknown>schema_tmp;

describe('ex04', (): void => {
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
    function analyzer(x: ex04.Foo) {;
      return ex04.analyzeFoo<number>({
        foo0: _ => 0,
        foo1: v => v.arg,
        foo2: v => v.arg.reduce((x,y) => x+y, 0),
      })(x);
    };

    expect(analyzer(samples[0])).toBe(0)
    expect(analyzer(samples[1])).toBe(1)
    expect(analyzer(samples[2])).toBe(3)
  })
})

