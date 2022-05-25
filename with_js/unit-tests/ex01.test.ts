import * as ex01 from "../compile-tests/ex01_gen"
import * as tmp from "../compile-tests/ex01_examples.json"

// also typecheck the generate JSON
const samples : ex01.student[] = <ex01.student[]>tmp;

describe('ex01', (): void => {
  test('it compiles', (): void => {
    expect(samples[0].admission_year).toBe(1984);
    expect(samples[0].name).toBe("William Gibson");
    return;
  })
})
