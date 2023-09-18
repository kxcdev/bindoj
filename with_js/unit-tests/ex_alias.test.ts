import * as ex from "../compile-tests/ex_alias_gen";
import * as samples_tmp from "../compile-tests/ex_alias_examples.json";
import * as schema_tmp from "../compile-tests/ex_alias_schema.json";
import { Schema } from "jsonschema";
import { validationTest } from "./helper";

const allSchema: Schema[] = <Schema[]>(<unknown>schema_tmp);

describe("ex_alias", (): void => {
  describe("ExAliasUnit", () => {
    // also typecheck the generate JSON
    const samples: ex.ExAliasUnit[] = <ex.ExAliasUnit[]>samples_tmp.ExAliasUnit;

    test("it compiles", () => {
      expect(samples[0]).toBe(1);
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExAliasUnit"));
  });

  describe("ExAliasIntOpt", () => {
    // also typecheck the generate JSON
    const samples: ex.ExAliasIntOpt[] = <ex.ExAliasIntOpt[]>samples_tmp.ExAliasIntOpt;

    test("it compiles", () => {
      expect(samples[0]).toBeNull();
      expect(samples[1]).toBe(42);
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExAliasIntOpt"));
  });

  describe("ExAliasObjtuple", () => {
    // also typecheck the generate JSON
    const samples: ex.ExAliasObjtuple[] = <ex.ExAliasObjtuple[]>samples_tmp.ExAliasObjtuple;

    test("it compiles", () => {
      expect(Math.abs(samples[0]._0 - 12.3)).toBeLessThanOrEqual(0);
      expect(samples[0]._1).toBe("test");
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExAliasObjtuple"));
  });
});
