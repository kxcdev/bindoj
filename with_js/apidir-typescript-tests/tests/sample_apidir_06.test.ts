import { MockServer, createMockClient } from "./lib";
import { Sample_apidir_06InvpInfo as invpInfo, Sample_apidir_06ClientIntf as clientIntf } from "../sample_apidir_06";
import { XyOpt } from "../../compile-tests/ex10_gen";

const b = require("../sample_apidir_06_server.js").bindoj_jsoo_bridge;

const mockServer: MockServer = b.server_mock;

const mockClient: clientIntf = createMockClient(invpInfo)(mockServer);

describe("apidir-typescript-tests-sample06", () => {
  afterAll(() => {
    b.coverage_helper.write_coverage_data();
    b.coverage_helper.reset_counters();
  });

  const sample_values: XyOpt[] = [
    { },
    { yOpt: 42 },
    { xOpt: -25 },
    { xOpt: 512, yOpt: -119 },
  ];

  describe("get-x", () => {
    sample_values.forEach((samle, i) => {
      test(`test ${i}`, async () => {
        const { body, status_code } = await mockClient["get-x"](samle);
        expect(status_code).toBe(200);
        expect(body).toBe(samle.xOpt ?? null);
      });
    });
  });

  describe("get-y", () => {
    sample_values.forEach((samle, i) => {
      test(`test ${i}`, async () => {
        const { body, status_code } = await mockClient["get-y"](samle);
        expect(status_code).toBe(200);
        expect(body).toBe(samle.yOpt ?? null);
      });
    });
  });
});
