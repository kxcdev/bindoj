import * as ex06 from "../compile-tests/ex06_gen"
import * as tmp from "../compile-tests/ex06_examples.json"

// also typecheck the generate JSON
const samples : ex06.various_prim_types[] = <ex06.various_prim_types[]>tmp;

describe('ex06', (): void => {
  test('it compiles', (): void => {
    return;
  })
})

