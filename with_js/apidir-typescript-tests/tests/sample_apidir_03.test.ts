import { MockServer, createMockClient } from "./lib";
import { Sample_apidir_03InvpInfo as invpInfo, Sample_apidir_03ClientIntf as clientIntf } from "../sample_apidir_03";
import { ExVariantPerson } from "../../compile-tests/ex_variant_gen";

const b = require("../sample_apidir_03_server.js").bindoj_jsoo_bridge;

const mockServer: MockServer = b.server_mock;

const mockClient: clientIntf = createMockClient(invpInfo)(mockServer);

describe("apidir-typescript-tests-sample03", () => {
  afterAll(() => {
    b.coverage_helper.write_coverage_data();
    b.coverage_helper.reset_counters();
  });

  describe("id-of-person", () => {
    test("Anonymous", async () => {
      const person: ExVariantPerson = { kind: "anonymous" };
      const { body, status_code } = await mockClient["id-of-person"](person);
      expect(status_code).toBe(403);
      expect(body).toBe("Cannot get the ID of an anonymous person.");
    });

    test("With_id", async () => {
      const person: ExVariantPerson = { kind: "with-id", arg: 1619 };
      const { body, status_code } = await mockClient["id-of-person"](person);
      expect(status_code).toBe(200);
      expect(body).toBe(1619);
    });

    test("Student", async () => {
      const person: ExVariantPerson = { kind: "student", name: "Ray Bradbury", studentId: 451 };
      const { body, status_code } = await mockClient["id-of-person"](person);
      expect(status_code).toBe(200);
      expect(body).toBe(451);
    });

    test("Teacher", async () => {
      const person: ExVariantPerson = { kind: "teacher", name: "Arthur C. Clark", department: "Space", facultyId: 2001 };
      const { body, status_code } = await mockClient["id-of-person"](person);
      expect(status_code).toBe(404);
      expect(body).toBe("This teacher does not have a personal ID.");
    });
  });
});
