import * as ex07 from "../compile-tests/ex07_gen"
import * as tmp from "../compile-tests/ex07_examples.json"

// also typecheck the generate JSON
const samples : ex07.customized_union[] = <ex07.customized_union[]>tmp;

describe('ex07', (): void => {
  test('it compiles', (): void => {
    return;
  })

  test('the case analyzer works', (): void => {
    function analyzer(x: ex07.customized_union) {;
      return ex07.analyze_customized_union<number>({
        Case1_: v => v.value,
        Case2_: v => v.x_ + v.y_,
      })(x);
    };

    expect(analyzer(samples[0])).toBe(42)
    expect(analyzer(samples[1])).toBe(6)
  })
})

