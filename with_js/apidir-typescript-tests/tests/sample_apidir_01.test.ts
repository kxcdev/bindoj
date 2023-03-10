import { MockServer, createMockClient } from "./lib";
import { Sample_apidir_01InvpInfo as invpInfo, Sample_apidir_01ClientIntf as clientIntf } from "../sample_apidir_01";
import { person } from "../../compile-tests/ex02_gen";

const b = require("../sample_apidir_01_server.js").bindoj_jsoo_bridge;

const mockServer: MockServer = b.server_mock;

const mockClient: clientIntf = createMockClient(invpInfo)(mockServer);

describe("apidir-typescript-tests-sample01", () => {
  test("get-any-student", async () => {
    const { body: student, status_code } = await mockClient["get-any-student"]();
    expect(status_code).toBe(200);
    expect(student.name).toBe("William Gibson");
    expect(student.admission_year).toBe(1984);
  });

  test("get-student-from-person", async () => {
    const person: person = { kind: "Student", student_id: 1997, name: "Bruce Sterling" };
    const { body: student, status_code } = await mockClient["get-student-from-person"](person);

    expect(status_code).toBe(200);
    expect(student.name).toBe(person.name);
    expect(student.admission_year).toBe(person.student_id);
  });
});
