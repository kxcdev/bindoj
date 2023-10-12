import * as ex from "../compile-tests/ex_optional_gen";
import * as samples_tmp from "../compile-tests/ex_optional_examples.json";
import * as schema_tmp from "../compile-tests/ex_optional_schema.json";
import { Schema } from "jsonschema";
import { validationTest } from "./helper";

const allSchema: Schema[] = <Schema[]>(<unknown>schema_tmp);

describe("ex_optional", (): void => {
  describe("ExOptionalXyOpt", () => {
    // also typecheck the generate JSON
    const samples: ex.ExOptionalXyOpt[] = <ex.ExOptionalXyOpt[]>samples_tmp.ExOptionalXyOpt;

    test("it compiles", (): void => {
      expect(samples[0]).toStrictEqual({});
      expect(samples[1]).toStrictEqual({ yOpt: 42 });
      expect(samples[2]).toStrictEqual({ xOpt: -25 });
      expect(samples[3]).toStrictEqual({ xOpt: 512, yOpt: -119 });
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExOptionalXyOpt"));
  });

  describe("ExOptionalVariant", () => {
    // also typecheck the generate JSON
    const samples: ex.ExOptionalVariant[] = <ex.ExOptionalVariant[]>samples_tmp.ExOptionalVariant;

    test("it compiles", () => {
      expect(samples).toStrictEqual([
        { tag: "tuple-like" },
        { tag: "tuple-like", arg: 42 },
        { tag: "tuple-like-alias" },
        { tag: "tuple-like-alias", arg: 42 },
        { tag: "tuple-like-obj" },
        { tag: "tuple-like-obj", _0: 42 },
        { tag: "tuple-like-obj", _1: 128 },
        { tag: "tuple-like-obj", _0: 256, _1: 23 },
        { tag: "tuple-like-spreading" },
        { tag: "tuple-like-spreading", yOpt: 42 },
        { tag: "tuple-like-spreading", xOpt: -25 },
        { tag: "tuple-like-spreading", xOpt: 512, yOpt: -119 },
        { tag: "inline-record", objtuple: {} },
        { tag: "inline-record", objtuple: { _0: 42 } },
        { tag: "inline-record", objtuple: { _1: 128 } },
        { tag: "inline-record", objtuple: { _0: 256, _1: 23 } },
        { tag: "inline-record", yOpt: 42, objtuple: {} },
        { tag: "inline-record", yOpt: 42, objtuple: { _0: 42 } },
        { tag: "inline-record", yOpt: 42, objtuple: { _1: 128 } },
        { tag: "inline-record", yOpt: 42, objtuple: { _0: 256, _1: 23 } },
        { tag: "inline-record", xOpt: -25, objtuple: {} },
        { tag: "inline-record", xOpt: -25, objtuple: { _0: 42 } },
        { tag: "inline-record", xOpt: -25, objtuple: { _1: 128 } },
        { tag: "inline-record", xOpt: -25, objtuple: { _0: 256, _1: 23 } },
        { tag: "inline-record", xOpt: 512, yOpt: -119, objtuple: {} },
        { tag: "inline-record", xOpt: 512, yOpt: -119, objtuple: { _0: 42 } },
        { tag: "inline-record", xOpt: 512, yOpt: -119, objtuple: { _1: 128 } },
        { tag: "inline-record", xOpt: 512, yOpt: -119, objtuple: { _0: 256, _1: 23 } },
        { tag: "inline-record", intOpt: 42, objtuple: {} },
        { tag: "inline-record", intOpt: 42, objtuple: { _0: 42 } },
        { tag: "inline-record", intOpt: 42, objtuple: { _1: 128 } },
        { tag: "inline-record", intOpt: 42, objtuple: { _0: 256, _1: 23 } },
        { tag: "inline-record", intOpt: 42, yOpt: 42, objtuple: {} },
        { tag: "inline-record", intOpt: 42, yOpt: 42, objtuple: { _0: 42 } },
        { tag: "inline-record", intOpt: 42, yOpt: 42, objtuple: { _1: 128 } },
        { tag: "inline-record", intOpt: 42, yOpt: 42, objtuple: { _0: 256, _1: 23 } },
        { tag: "inline-record", intOpt: 42, xOpt: -25, objtuple: {} },
        { tag: "inline-record", intOpt: 42, xOpt: -25, objtuple: { _0: 42 } },
        { tag: "inline-record", intOpt: 42, xOpt: -25, objtuple: { _1: 128 } },
        { tag: "inline-record", intOpt: 42, xOpt: -25, objtuple: { _0: 256, _1: 23 } },
        { tag: "inline-record", intOpt: 42, xOpt: 512, yOpt: -119, objtuple: {} },
        { tag: "inline-record", intOpt: 42, xOpt: 512, yOpt: -119, objtuple: { _0: 42 } },
        { tag: "inline-record", intOpt: 42, xOpt: 512, yOpt: -119, objtuple: { _1: 128 } },
        { tag: "inline-record", intOpt: 42, xOpt: 512, yOpt: -119, objtuple: { _0: 256, _1: 23 } },
        { tag: "inline-record-spreading" },
        { tag: "inline-record-spreading", yOpt: 42 },
        { tag: "inline-record-spreading", xOpt: -25 },
        { tag: "inline-record-spreading", xOpt: 512, yOpt: -119 },
        { tag: "inline-record-spreading", intOpt: 42 },
        { tag: "inline-record-spreading", intOpt: 42, yOpt: 42 },
        { tag: "inline-record-spreading", intOpt: 42, xOpt: -25 },
        { tag: "inline-record-spreading", intOpt: 42, xOpt: 512, yOpt: -119 },
        { tag: "reused-inline-record" },
        { tag: "reused-inline-record", yOpt: 42 },
        { tag: "reused-inline-record", xOpt: -25 },
        { tag: "reused-inline-record", xOpt: 512, yOpt: -119 },
      ]);
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExOptionalVariant"));
  });
});
