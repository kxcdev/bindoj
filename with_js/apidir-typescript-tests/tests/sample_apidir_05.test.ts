import { MockServer, createMockClient } from "./lib";
import { Sample_apidir_05InvpInfo as invpInfo, Sample_apidir_05ClientIntf as clientIntf } from "../sample_apidir_05";
import { complex_types } from "../../compile-tests/ex05_gen";

const b = require("../sample_apidir_05_server.js").bindoj_jsoo_bridge;

const mockServer: MockServer = b.server_mock;

const mockClient: clientIntf = createMockClient(invpInfo)(mockServer);

describe("apidir-typescript-tests-sample05", () => {
  describe("int-of-string", () => {
    [
      { value: "", result: null },
      { value: "0", result: 0 },
      { value: "2020", result: 2020 },
      { value: "a", result: null },
      { value: "11.2", result: null },
    ].forEach((x) => {
      test(x.value, async () => {
        const { body, status_code } = await mockClient["int-of-string"](x.value);
        expect(status_code).toBe(200);
        expect(body).toBe(x.result);
      });
    });
  });

  describe("option-of-string", () => {
    const sample_values: complex_types[] = [
      {
        option: 42,
        list: [1, 2, 3, 4],
        map: { foo: 4, bar: 2 },
        nested: [null, [0, 1, 2], [1, 2]],
        objtuple: { _0: 1, _1: 2 },
        tuple: [3, 4],
      },
      { option: null,
        list: [],
        map: {},
        nested: [null, [0, 1, 2], [1, 2]],
        objtuple: { _0: 1, _1: 2 },
        tuple: [3, 4] },
    ];
    sample_values.forEach((x, i) => {
      test(`test ${i}`, async () => {
        const { body, status_code } = await mockClient["option-of-complex"](x);
        expect(status_code).toBe(200);
        expect(body).toBe(x.option);
      });
    });
  });
});
