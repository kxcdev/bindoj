import * as ex from "../compile-tests/ex_version_substring_gen";
import * as samples_tmp from "../compile-tests/ex_version_substring_examples.json";
import * as schema_tmp from "../compile-tests/ex_version_substring_schema.json";
import { Schema } from "jsonschema";
import { validationTest } from "./helper";

const allSchema: Schema[] = <Schema[]>(<unknown>schema_tmp);

describe("ex_version_substring", (): void => {
  describe("ExVersionSubstringRecordV3_2_1", () => {
    // also typecheck the generate JSON
    const samples: ex.ExVersionSubstringRecordV3_2_1[] = <ex.ExVersionSubstringRecordV3_2_1[]>(
      samples_tmp.ExVersionSubstringRecordV3_2_1
    );

    test("it compiles", (): void => {
      const commonProps = {
        versionInfoV2: -1,
        versionInfoV2_0: -2,
        versionInfoV2_0_1: -3,
        versionV3Info: 1,
        versionV3_0Info: 2,
        versionV3_0_1Info: 3,
        v4VersionInfo: 4,
        v4_0VersionInfo: 5,
        v4_0_1VersionInfo: 6,
        noPreservingV12Version: 0,
      };
      expect(samples).toStrictEqual([
        {
          v5_3VersionInfo: "Case-version-v1",
          ...commonProps,
        },
        {
          v5_3VersionInfo: "Case-v2_0-version",
          ...commonProps,
        },
        {
          v5_3VersionInfo: "v3_0_1-case-version",
          ...commonProps,
        },
      ]);
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExVersionSubstringRecordV3_2_1"));
  });

  describe("ExVersionSubstringVariantV1_0", () => {
    // also typecheck the generate JSON
    const samples: ex.ExVersionSubstringVariantV1_0[] = <ex.ExVersionSubstringVariantV1_0[]>(
      samples_tmp.ExVersionSubstringVariantV1_0
    );

    test("it compiles", (): void => {
      expect(samples).toStrictEqual([
        { kind: "version-info-v1_0", versionInfoV1: 1, versionInfoV1_0: 2, versionInfoV1_0_1: 3 },
        { kind: "version-v1_0-info", versionV1Info: 1, versionV1_0Info: 2, versionV1_0_1Info: 3 },
        { kind: "v1_0-version-info", v1VersionInfo: 1, v1_0VersionInfo: 2, v1_0_1VersionInfo: 3 },
        { kind: "no-preserving-version-substring-v1-0", versionInfoV1: 1, versionInfoV10: 2, versionInfoV101: 3 },
      ]);
    });

    test("schema validates all the examples", validationTest(allSchema, samples, "#ExVersionSubstringVariantV1_0"));
  });
});
