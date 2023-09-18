import * as ex from "../compile-tests/ex_coretype_gen";
import * as samples_tmp from "../compile-tests/ex_coretype_examples.json";
import * as schema_tmp from "../compile-tests/ex_coretype_schema.json";
import { Schema } from "jsonschema";
import { validationTest } from "./helper";

const allSchema: Schema[] = <Schema[]>(<unknown>schema_tmp);

describe("ex_coretype", (): void => {
  describe("ExCoretypeVariousPrimTypes", () => {
    // also typecheck the generate JSON
    const samples: ex.ExCoretypeVariousPrimTypes[] = <ex.ExCoretypeVariousPrimTypes[]>(
      samples_tmp.ExCoretypeVariousPrimTypes
    );

    test("it compiles", (): void => {
      expect(samples[0].unit).toBe(1);
      expect(samples[0].bool).toBe(true);
      expect(samples[0].int).toBe(42);
      expect(Math.abs(samples[0].float - 4.2)).toBeLessThanOrEqual(0);
      expect(samples[0].string).toBe("foo");
      expect(samples[0].uchar).toBe("a");
      expect(String.fromCharCode(samples[0].byte)).toBe("b");
      expect(samples[0].bytes).toBe("SGVsbG8sIHdvcmxkIQ==");
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExCoretypeVariousPrimTypes"));
  });

  describe("ExCoretypeWithInt53p", () => {
    // also typecheck the generate JSON
    const samples: ex.ExCoretypeWithInt53p[] = <ex.ExCoretypeWithInt53p[]>samples_tmp.ExCoretypeWithInt53p;

    test("it compiles", (): void => {
      expect(samples[0]).toStrictEqual({ value: 0 });
      expect(samples[1]).toStrictEqual({ value: 1 });
      expect(samples[2]).toStrictEqual({ value: -1 });
      expect(samples[3]).toStrictEqual({ value: 102 });
      expect(samples[4]).toStrictEqual({ value: 1_099_511_627_776 });
      expect(samples[5]).toStrictEqual({ value: -2_199_023_255_552 });
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExCoretypeWithInt53p"));
  });

  describe("ExCoretypeVariousComplexTypes", () => {
    // also typecheck the generate JSON
    const samples: ex.ExCoretypeVariousComplexTypes[] = <ex.ExCoretypeVariousComplexTypes[]>(
      samples_tmp.ExCoretypeVariousComplexTypes
    );

    test("it compiles", (): void => {
      expect(samples[0]).toStrictEqual({
        option: 42,
        list: [1, 2, 3, 4],
        map: { foo: 4, bar: 2 },
      });
      expect(samples[1]).toStrictEqual({ list: [], map: {} });
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExCoretypeVariousComplexTypes"));
  });

  describe("ExCoretypeVariousTupleTypes", () => {
    // also typecheck the generate JSON
    const samples: ex.ExCoretypeVariousTupleTypes[] = <ex.ExCoretypeVariousTupleTypes[]>(
      samples_tmp.ExCoretypeVariousTupleTypes
    );

    test("it compiles", (): void => {
      expect(samples[0]).toStrictEqual({
        tuple: [4, 2],
        objtuple: { _0: 4, _1: 2 },
        nested: [42, [4, 2], [4, 2]],
      });
      expect(samples[1]).toStrictEqual({
        tuple: [0, 0],
        objtuple: { _0: 0, _1: 0 },
        nested: [null, [], [0, 0]],
      });
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExCoretypeVariousTupleTypes"));
  });

  describe("ExCoretypeNamedJson", () => {
    // also typecheck the generate JSON
    const samples: ex.ExCoretypeNamedJson[] = <ex.ExCoretypeNamedJson[]>samples_tmp.ExCoretypeNamedJson;

    test("it compiles", (): void => {
      expect(samples).toStrictEqual([
        {
          name: "position",
          json: { x: 2, y: -5 },
        },
        {
          name: "greeting",
          json: "hello?",
        },
        {
          name: "tup_pos",
          json: ["x", 2, "y", -5],
        },
        {
          name: "flag",
          json: true,
        },
        {
          name: "null_val",
          json: null,
        },
      ]);
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExCoretypeNamedJson"));
  });
});
