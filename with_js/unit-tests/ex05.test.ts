import * as ex05 from "../compile-tests/ex05_gen"
import * as tmp from "../compile-tests/ex05_examples.json"

// also typecheck the generate JSON
// const samples : ex05.complex_types[] = <ex05.complex_types[]>tmp;
// `resolveJsonModule` generates a TypeScript type from JSON which is not compatible with ex05_gen

describe('ex05', (): void => {
  test('it compiles', (): void => {
    return;
  })
})

