import { MockServer, createMockClient } from "./lib";
import { Sample_apidir_06InvpInfo as invpInfo, Sample_apidir_06ClientIntf as clientIntf } from "../sample_apidir_06";
import { xy_opt } from "../../compile-tests/ex10_gen";

const b = require("../sample_apidir_06_server.js").bindoj_jsoo_bridge;

const mockServer: MockServer = b.server_mock;

const mockClient: clientIntf = createMockClient(invpInfo)(mockServer);

describe("apidir-typescript-tests-sample06", () => {
  const sample_values: xy_opt[] = [
    { x_opt: null, y_opt: null },
    { x_opt: null, y_opt: 42 },
    { x_opt: -25, y_opt: null },
    { x_opt: 512, y_opt: -119 },
  ];

  describe("get-x", () => {
    sample_values.forEach((samle, i) => {
      test(`test ${i}`, async () => {
        const { body, status_code } = await mockClient["get-x"](samle);
        expect(status_code).toBe(200);
        expect(body).toBe(samle.x_opt);
      });
    });
  });

  describe("get-y", () => {
    sample_values.forEach((samle, i) => {
      test(`test ${i}`, async () => {
        const { body, status_code } = await mockClient["get-y"](samle);
        expect(status_code).toBe(200);
        expect(body).toBe(samle.y_opt);
      });
    });
  });
});
