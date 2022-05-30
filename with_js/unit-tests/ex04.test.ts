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

