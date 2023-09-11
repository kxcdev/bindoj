import { MockServer, createMockClient } from "./lib";
import { Sample_apidir_02InvpInfo as invpInfo, Sample_apidir_02ClientIntf as clientIntf } from "../sample_apidir_02";
import { IntListObjtuple } from "../../compile-tests/ex03_objtuple_gen";

const b = require("../sample_apidir_02_server.js").bindoj_jsoo_bridge;

const mockServer: MockServer = b.server_mock;

const mockClient: clientIntf = createMockClient(invpInfo)(mockServer);

const createIntList = (nums: number[]): IntListObjtuple => {
  if (nums.length === 0) {
    return { kind: "intnil" };
  } else {
    return { kind: "intcons", _0: nums[0], _1: createIntList(nums.slice(1)) } as unknown as IntListObjtuple;
  }
};

describe("apidir-typescript-tests-sample02", () => {
  afterAll(() => {
    b.coverage_helper.write_coverage_data();
    b.coverage_helper.reset_counters();
  });

  test("get-any-int-list", async () => {
    const { body, status_code } = await mockClient["get-any-int-list"]();
    expect(status_code).toBe(200);
    expect(body).toStrictEqual<IntListObjtuple>(createIntList([1, 2, 3, 4]));
  });

  const sample_values = [
    [],
    [1],
    [1, 2, 3],
    [-1, 0, 1]
  ];

  describe("inc-int-list", () => {
    sample_values.forEach((numbers) => {
      test(`[${numbers.map((x) => x.toString()).join(", ")}]`, async () => {
        const { body, status_code } = await mockClient["inc-int-list"](createIntList(numbers));
        expect(status_code).toBe(200);
        expect(body).toStrictEqual(createIntList(numbers.map((x) => x + 1)));
      });
    });
  });

  describe("sum-of-int-list", () => {
    sample_values.forEach((numbers) => {
      test(`[${numbers.map((x) => x.toString()).join(", ")}]`, async () => {
        const { body, status_code } = await mockClient["sum-of-int-list"](createIntList(numbers));
        expect(status_code).toBe(200);
        expect(body).toBe(numbers.reduce((s, x) => s + x, 0));
      });
    })
  })
});
