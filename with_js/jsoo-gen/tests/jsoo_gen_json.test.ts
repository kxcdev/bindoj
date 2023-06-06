import { readFile } from "fs/promises";

const b = require("../jsoo_gen_json.js").jsoo_gen_json;

const generator = b.generator;

describe("jsoo_gen_json", () => {
  const names: string[] = generator.module_names;
  for (const name of names) {
    test(name, async () => {
      const generated_str: string = generator.generate(name);
      const expected_str: string = await readFile(`compile-tests/${name}_examples.json`, "utf-8");

      const generated_json = JSON.parse(generated_str);
      const expected_json = JSON.parse(expected_str);

      // Comparison by string fails due to a difference in the implementation of int53p in kxclib (current_impl_flavor).
      // expect(generated_str).toStrictEqual(expected_str);

      expect(generated_json).toStrictEqual(expected_json);
    });
  }
});
