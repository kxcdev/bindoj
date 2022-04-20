import * as ex01 from "../../src/lib_gen_ts/unit_test/ex01_ts_gen"

const sample01 : ex01.student = {
  admission_year: 2022,
  name: "Yamada"
}

describe('ex01', (): void => {
  test('it compiles', (): void => {
    console.log(sample01);
    return;
  })
})
