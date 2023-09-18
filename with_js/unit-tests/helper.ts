import { Schema, Validator } from "jsonschema";

// https://github.com/tdegrunt/jsonschema/issues/231#issuecomment-597391259
function rewriteNullable(schema: Schema): Schema {
  schema = JSON.parse(JSON.stringify(schema));

  if (schema.properties !== undefined) {
    schema.properties = Object.fromEntries(
      Object.entries(schema.properties).map(([key, s]) => {
        return [key, rewriteNullable(s)];
      })
    );
  }

  if (Array.isArray(schema.items)) {
    schema.items = schema.items?.map((s) => rewriteNullable(s));
  } else if (schema.items !== undefined) {
    schema.items = rewriteNullable(schema.items);
  }

  if ((schema as any).nullable !== true) return schema;

  if (Array.isArray(schema.type)) {
    schema.type.push("null");
  } else if (typeof schema.type === "string") {
    schema.type = [schema.type, "null"];
  }

  return schema;
}

export const validationTest =
  <T>(allSchema: Schema[], samples: T[], id: string, refs: Schema[] = []) =>
  () => {
    let schema = allSchema.find((x) => x.id === id);
    expect(schema).not.toBeUndefined();
    if (schema === undefined) throw "schema is undefined";

    const validator = new Validator();
    refs.forEach((s) => validator.addSchema(rewriteNullable(s), "/" + s.id));

    schema = rewriteNullable(schema);

    samples.forEach((sample) => {
      const result = validator.validate(sample, schema as Schema);
      expect(result.valid);
      expect(result.errors).toStrictEqual([]);
    });
  };
