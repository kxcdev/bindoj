import { readFile } from "fs/promises";

const b = require("../jsoo_gen_ml.js").jsoo_gen_ml;

const generator = b.generator;

const getFileName = (name: string, gen_type_decl: boolean, kind: "structure" | "signature") => {
  const directoryName = gen_type_decl ? "output_with_decl" : "output";
  const extension = kind === "structure" ? "ml" : "mli";
  return `../src/lib_gen/unit_test/gen/${directoryName}/${name}_gen.${extension}`;
};

describe("jsoo_gen_ml", () => {
  afterAll(() => {
    b.coverage_helper.write_coverage_data();
    b.coverage_helper.reset_counters();
  });

  const names: string[] = generator.module_names;
  names
    .flatMap((name) => [
      { name: name, testName: name, gen_type_decl: false },
      { name: name + "_docstr", testName: name + " with docstr", gen_type_decl: false },
      { name: name, testName: name + " with decl", gen_type_decl: true },
    ])
    .forEach(({ name, testName, gen_type_decl }) => {
      test(testName, async () => {
        const generated: { structure: string; signature: string } = generator.generate(name, gen_type_decl);

        const expectedStructure: string = await readFile(getFileName(name, gen_type_decl, "structure"), "utf-8");
        const expectedSignature: string = await readFile(getFileName(name, gen_type_decl, "signature"), "utf-8");

        expect(generated.structure).toStrictEqual(expectedStructure);
        expect(generated.signature).toStrictEqual(expectedSignature);
      });
    });
});
