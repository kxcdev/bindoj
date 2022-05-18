import * as ex03 from "../compile-tests/ex03_ts_gen"
import * as tmp from "../compile-tests/ex03_examples.json"

// also typecheck the generate JSON
const samples : ex03.int_list[] = <ex03.int_list[]>tmp;

describe('ex03', (): void => {
  test('it compiles', (): void => {
    return;
  })

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
  })
})

