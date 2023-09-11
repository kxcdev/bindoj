import { MockServer, createMockClient } from "./lib";
import { Sample_apidir_04InvpInfo as invpInfo, Sample_apidir_04ClientIntf as clientIntf } from "../sample_apidir_04";
import { NamedJson } from "../../compile-tests/ex08_gen";

const b = require("../sample_apidir_04_server.js").bindoj_jsoo_bridge;

const mockServer: MockServer = b.server_mock;

const mockClient: clientIntf = createMockClient(invpInfo)(mockServer);

describe("apidir-typescript-tests-sample04", () => {
  afterAll(() => {
    b.coverage_helper.write_coverage_data();
    b.coverage_helper.reset_counters();
  });

  const sample_values: NamedJson[] = [
    { name: "position", json: { x: 2, y: -5} },
    { name: "greeting", json: "hello?" },
    { name: "tup_pos", json: [ "x", 2, "y", -5 ] },
    { name: "flag", json: true },
    { name: "null_val", json: null }
  ];
  describe("name-of-named_json", () => {
    sample_values.forEach((named_json) => {
      test(named_json.name, async () => {
        const { body, status_code } = await mockClient["name-of-named_json"](named_json);
        expect(status_code).toBe(200);
        expect(body).toBe(named_json.name);
      });
    });
  });

  describe("json-of-named_json", () => {
    sample_values.forEach((named_json) => {
      test(named_json.name, async () => {
        const { body, status_code } = await mockClient["json-of-named_json"](named_json);
        expect(status_code).toBe(200);
        expect(body).toStrictEqual(named_json.json);
      });
    });
  });
});
