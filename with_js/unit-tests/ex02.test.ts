import * as ex02 from "../../src/lib_gen_ts/unit_test/ex02_ts_gen"

const sample01 : ex02.person = { kind: "Anonymous" };
const sample02 : ex02.person = { kind: "With_id", arg: 4 }
const sample03 : ex02.person = { kind: "Student", student_id: 2, name: "Yamada" };
const sample04 : ex02.person = { kind: "Teacher", faculty_id: 42, name: "Foo", department: "Department of Silly Walks" };

type AnalyzeResult = string | number | [string, number] | [string, number, string]

describe('ex02', (): void => {
  test('it compiles', (): void => {
    return;
  })

  test('the case analyzer works', (): void => {
    let analyzer =
      ex02.analyze_person<AnalyzeResult>({
        Anonymous: _ => "anonymous",
        With_id:   v => v.arg,
        Student:   v => [v.name, v.student_id],
        Teacher:   v => [v.name, v.faculty_id, v.department]
      });
    expect(analyzer(sample01)).toBe("anonymous")
    expect(analyzer(sample02)).toBe(4)
    expect(analyzer(sample03)).toStrictEqual(["Yamada", 2])
    expect(analyzer(sample04)).toStrictEqual(["Foo", 42, "Department of Silly Walks"])
  })
})
