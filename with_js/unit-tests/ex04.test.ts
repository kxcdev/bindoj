import * as ex04 from "../compile-tests/ex04_gen"
import * as tmp from "../compile-tests/ex04_examples.json"

// also typecheck the generate JSON
const samples : ex04.foo[] = <ex04.foo[]>tmp;

describe('ex04', (): void => {
  test('it compiles', (): void => {
    return;
  })

  test('the case analyzer works', (): void => {
    function analyzer(x: ex04.foo) {;
      return ex04.analyze_foo<number|string>({
        Bar: v => v.arg,
        Baz: v => v.arg
      })(x);
    };

    expect(analyzer(samples[0])).toBe(42)
    expect(analyzer(samples[1])).toBe("Hello")
  })
})

