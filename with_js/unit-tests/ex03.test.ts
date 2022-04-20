import * as ex03 from "../../src/lib_gen_ts/unit_test/ex03_ts_gen"

function cons (arg: ex03.intlist) : ex03.intlist {
  return { kind: "CONS", arg: arg };
}

const nil : ex03.intlist = { kind: "NIL" }

describe('ex03', (): void => {
  test('it compiles', (): void => {
    return;
  })

  test('the case analyzer works', (): void => {
    function analyzer(x: ex03.intlist) : number {
      return ex03.analyze_intlist({
        CONS: v => 1 + analyzer(v.arg),
        NIL:  _ => 0
      })(x);
    };

    expect(analyzer(nil)).toBe(0)
    expect(analyzer(cons(cons(nil)))).toBe(2)
    expect(analyzer(cons(cons(cons(cons(nil)))))).toBe(4)
  })
})

