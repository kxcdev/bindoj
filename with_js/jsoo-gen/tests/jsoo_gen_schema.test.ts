import { readFile } from "fs/promises";

const b = require("../jsoo_gen_schema.js").jsoo_gen_schema;

const generator = b.generator;

describe("jsoo_gen_schema", () => {
  const names: string[] = generator.module_names;
  for (const name of names) {
    test(name, async () => {
      const generated_str: string = generator.generate(name);
      const expected_str: string = await readFile(`compile-tests/${name}_schema.json`, "utf-8");

      const generated_json = JSON.parse(generated_str);
      const expected_json = JSON.parse(expected_str);

      expect(generated_str).toStrictEqual(expected_str);

      expect(generated_json).toStrictEqual(expected_json);
    });
  }
});
