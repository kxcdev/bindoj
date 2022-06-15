import * as ex04 from "../compile-tests/ex04_gen"
import * as tmp from "../compile-tests/ex04_examples.json"
import * as schema from "../compile-tests/ex04_schema.json"
import {Validator} from "jsonschema"
var validator = new Validator();

// also typecheck the generate JSON
const samples : ex04.foo[] = <ex04.foo[]>tmp;

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
    function analyzer(x: ex04.foo) {;
      return ex04.analyze_foo<number>({
        Foo0: _ => 0,
        Foo1: v => v.arg,
        Foo2: v => v.arg.reduce((x,y) => x+y, 0),
      })(x);
    };

    expect(analyzer(samples[0])).toBe(0)
    expect(analyzer(samples[1])).toBe(1)
    expect(analyzer(samples[2])).toBe(3)
  })
})

