import { MockServer, createMockClient } from "./lib";
import { Sample_apidir_03InvpInfo as invpInfo, Sample_apidir_03ClientIntf as clientIntf } from "../sample_apidir_03";
import { person } from "../../compile-tests/ex02_gen";

const b = require("../sample_apidir_03_server.js").bindoj_jsoo_bridge;

const mockServer: MockServer = b.server_mock;

const mockClient: clientIntf = createMockClient(invpInfo)(mockServer);

describe("apidir-typescript-tests-sample03", () => {
  describe("id-of-person", () => {
    test("Anonymous", async () => {
      const person: person = { kind: "Anonymous" };
      const { body, status_code } = await mockClient["id-of-person"](person);
      expect(status_code).toBe(403);
      expect(body).toBe("Cannot get the ID of an anonymous person.");
    });

    test("With_id", async () => {
      const person: person = { kind: "With_id", arg: 1619 };
      const { body, status_code } = await mockClient["id-of-person"](person);
      expect(status_code).toBe(200);
      expect(body).toBe(1619);
    });

    test("Student", async () => {
      const person: person = { kind: "Student", name: "Ray Bradbury", student_id: 451 };
      const { body, status_code } = await mockClient["id-of-person"](person);
      expect(status_code).toBe(200);
      expect(body).toBe(451);
    });

    test("Teacher", async () => {
      const person: person = { kind: "Teacher", name: "Arthur C. Clark", department: "Space", faculty_id: 2001 };
      const { body, status_code } = await mockClient["id-of-person"](person);
      expect(status_code).toBe(404);
      expect(body).toBe("This teacher does not have a personal ID.");
    });
  });
});
