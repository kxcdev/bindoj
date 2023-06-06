import * as ex07 from "../compile-tests/ex07_gen"
import * as tmp from "../compile-tests/ex07_examples.json"

// also typecheck the generate JSON
const samples : ex07.CustomizedUnion[] = <ex07.CustomizedUnion[]>tmp;

describe('ex07', (): void => {
  test('it compiles', (): void => {
    return;
  })

  test('the case analyzer works', (): void => {
    function analyzer(x: ex07.CustomizedUnion) {;
      return ex07.analyzeCustomizedUnion<number>({
        "case1'": v => v.value,
        "case2'": v => v["x'"] + v["y'"],
      })(x);
    };

    expect(analyzer(samples[0])).toBe(42)
    expect(analyzer(samples[1])).toBe(6)
  })
})

