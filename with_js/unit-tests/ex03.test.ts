import * as ex03 from "../compile-tests/ex03_ts_gen"

function cons (value: number, rest: ex03.int_list) : ex03.int_list {
  return { kind: "IntCons", arg: [value, rest] };
}

const nil : ex03.int_list = { kind: "IntNil" }

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

    expect(analyzer(nil)).toBe(0)
    expect(analyzer(cons(1, cons(2, nil)))).toBe(3)
    expect(analyzer(cons(1, cons(2, cons(3, cons(4, nil)))))).toBe(10)
  })
})

