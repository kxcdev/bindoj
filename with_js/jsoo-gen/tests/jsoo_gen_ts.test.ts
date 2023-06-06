import { readFile } from 'fs/promises';

const b = require("../jsoo_gen_ts.js").jsoo_gen_ts;

const generator = b.generator;

describe("jsoo_gen_ts", () => {
  const names: string[] = generator.module_names;
  for (const name of names) {
    test(name, async () => {
      const generated: string = generator.generate(name);

      const expected: string = await readFile(`compile-tests/${name}_gen.ts`, 'utf-8');

      expect(generated).toStrictEqual(expected);
    });
  }
});
