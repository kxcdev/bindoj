import { MockServer, createMockClient } from "./lib";
import { Sample_apidir_01_no_manglingInvpInfo as invpInfo, Sample_apidir_01_no_manglingClientIntf as clientIntf } from "../sample_apidir_01_no_mangling";
import { ex_mangling_person_no_mangling } from "../../compile-tests/ex_mangling_gen";

const b = require("../sample_apidir_01_no_mangling_server.js").bindoj_jsoo_bridge;

const mockServer: MockServer = b.server_mock;

const mockClient: clientIntf = createMockClient(invpInfo)(mockServer);

describe("apidir-typescript-tests-sample01-no-mangling", () => {
  afterAll(() => {
    b.coverage_helper.write_coverage_data();
    b.coverage_helper.reset_counters();
  });

  test("get-any-student", async () => {
    const { body: student, status_code } = await mockClient["get-any-student"]();
    expect(status_code).toBe(200);
    expect(student.name).toBe("William Gibson");
    expect(student.admissionYear).toBe(1984);
  });

  test("get-student-from-person", async () => {
    const person: ex_mangling_person_no_mangling = { kind: "Student", student_id: 1997, name: "Bruce Sterling" };
    const { body: student, status_code } = await mockClient["get-student-from-person"](person);

    expect(status_code).toBe(200);
    expect(student.name).toBe(person.name);
    expect(student.admissionYear).toBe(person.student_id);
  });
});
