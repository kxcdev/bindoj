import * as ex08 from "../compile-tests/ex08_gen"
import * as tmp from "../compile-tests/ex08_examples.json"

// also typecheck the generate JSON
const samples : ex08.named_json[] = <ex08.named_json[]>tmp;

describe('ex08', (): void => {
  test('it compiles', (): void => {
    return;
  })
})
