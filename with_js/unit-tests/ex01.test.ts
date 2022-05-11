import * as ex01 from "../compile-tests/ex01_ts_gen"

const sample01 : ex01.student = {
  admission_year: 2022,
  name: "Yamada"
}

describe('ex01', (): void => {
  test('it compiles', (): void => {
    expect(sample01.admission_year).toBe(2022);
    expect(sample01.name).toBe("Yamada");
    return;
  })
})
