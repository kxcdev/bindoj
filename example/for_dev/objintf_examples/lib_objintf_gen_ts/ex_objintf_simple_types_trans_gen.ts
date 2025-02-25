/* eslint-disable @typescript-eslint/no-namespace */
import { objintf as bindoj } from "../../public-packages/runtime";
import {
  ExVersionSubstringVariantV1_0,
  ExVersionSubstringRecordV3_2_1,
} from "../../compile-tests/ex_version_substring_gen";
import {
  ExVariantPersonReused,
  ExVariantPerson,
  ExVariantIntListObjtuple,
  ExVariantIntList,
  ExVariantFoo,
  ExVariantCustomizedUnion,
} from "../../compile-tests/ex_variant_gen";
import { ExRecordTeacher, ExRecordStudent } from "../../compile-tests/ex_record_gen";
import { ExOptionalXyOpt, ExOptionalVariant } from "../../compile-tests/ex_optional_gen";
import { ExNestedVariant, ExNestedRecord, ExNestedPoint2 } from "../../compile-tests/ex_nested_gen";
import { ExNestedMultiplyVariant, ExNestedMultiplyRecord } from "../../compile-tests/ex_nested_multiply_gen";
import {
  ex_mangling_student_inherited,
  ex_mangling_person_no_mangling,
  ex_mangling_person_inherited,
  ExManglingEnum,
} from "../../compile-tests/ex_mangling_gen";
import { ExIdentStudentPair } from "../../compile-tests/ex_ident_gen";
import {
  ExCoretypeWithInt53p,
  ExCoretypeVariousTupleTypes,
  ExCoretypeVariousPrimTypes,
  ExCoretypeVariousComplexTypes,
  ExCoretypeNamedJson,
} from "../../compile-tests/ex_coretype_gen";
import { ExAliasUnit, ExAliasObjtuple, ExAliasIntOpt } from "../../compile-tests/ex_alias_gen";
export namespace GeneratedBridge {
  export type PeerFullBridgeReference = bindoj.PeerFullBridgeReference<ConcreteBridge>;
  export namespace Interfaces {}
  export type ConcreteBridge = {
    readonly peerObjects: {
      readonly ctyUnit: null;
      readonly ctyUnitOption: 1 | null | undefined;
      readonly ctyUnitList: 1[];
      readonly ctyUnitMap: Record<string, 1>;
      readonly ctyUnitTupleWithUnit: [1, 1];
      readonly ctyUnitTupleWithBool: [1, boolean];
      readonly ctyUnitTupleWithInt: [1, number];
      readonly ctyUnitTupleWithInt53p: [1, number];
      readonly ctyUnitTupleWithFloat: [1, number];
      readonly ctyUnitTupleWithString: [1, string];
      readonly ctyUnitTupleWithUchar: [1, string];
      readonly ctyUnitTupleWithByte: [1, number];
      readonly ctyUnitTupleWithBytes: [1, string];
      readonly ctyBool: boolean;
      readonly ctyBoolOption: boolean | null | undefined;
      readonly ctyBoolList: boolean[];
      readonly ctyBoolMap: Record<string, boolean>;
      readonly ctyBoolTupleWithUnit: [boolean, 1];
      readonly ctyBoolTupleWithBool: [boolean, boolean];
      readonly ctyBoolTupleWithInt: [boolean, number];
      readonly ctyBoolTupleWithInt53p: [boolean, number];
      readonly ctyBoolTupleWithFloat: [boolean, number];
      readonly ctyBoolTupleWithString: [boolean, string];
      readonly ctyBoolTupleWithUchar: [boolean, string];
      readonly ctyBoolTupleWithByte: [boolean, number];
      readonly ctyBoolTupleWithBytes: [boolean, string];
      readonly ctyInt: number;
      readonly ctyIntOption: number | null | undefined;
      readonly ctyIntList: number[];
      readonly ctyIntMap: Record<string, number>;
      readonly ctyIntTupleWithUnit: [number, 1];
      readonly ctyIntTupleWithBool: [number, boolean];
      readonly ctyIntTupleWithInt: [number, number];
      readonly ctyIntTupleWithInt53p: [number, number];
      readonly ctyIntTupleWithFloat: [number, number];
      readonly ctyIntTupleWithString: [number, string];
      readonly ctyIntTupleWithUchar: [number, string];
      readonly ctyIntTupleWithByte: [number, number];
      readonly ctyIntTupleWithBytes: [number, string];
      readonly ctyInt53p: number;
      readonly ctyInt53pOption: number | null | undefined;
      readonly ctyInt53pList: number[];
      readonly ctyInt53pMap: Record<string, number>;
      readonly ctyInt53pTupleWithUnit: [number, 1];
      readonly ctyInt53pTupleWithBool: [number, boolean];
      readonly ctyInt53pTupleWithInt: [number, number];
      readonly ctyInt53pTupleWithInt53p: [number, number];
      readonly ctyInt53pTupleWithFloat: [number, number];
      readonly ctyInt53pTupleWithString: [number, string];
      readonly ctyInt53pTupleWithUchar: [number, string];
      readonly ctyInt53pTupleWithByte: [number, number];
      readonly ctyInt53pTupleWithBytes: [number, string];
      readonly ctyFloat: number;
      readonly ctyFloatOption: number | null | undefined;
      readonly ctyFloatList: number[];
      readonly ctyFloatMap: Record<string, number>;
      readonly ctyFloatTupleWithUnit: [number, 1];
      readonly ctyFloatTupleWithBool: [number, boolean];
      readonly ctyFloatTupleWithInt: [number, number];
      readonly ctyFloatTupleWithInt53p: [number, number];
      readonly ctyFloatTupleWithFloat: [number, number];
      readonly ctyFloatTupleWithString: [number, string];
      readonly ctyFloatTupleWithUchar: [number, string];
      readonly ctyFloatTupleWithByte: [number, number];
      readonly ctyFloatTupleWithBytes: [number, string];
      readonly ctyString: string;
      readonly ctyStringOption: string | null | undefined;
      readonly ctyStringList: string[];
      readonly ctyStringMap: Record<string, string>;
      readonly ctyStringTupleWithUnit: [string, 1];
      readonly ctyStringTupleWithBool: [string, boolean];
      readonly ctyStringTupleWithInt: [string, number];
      readonly ctyStringTupleWithInt53p: [string, number];
      readonly ctyStringTupleWithFloat: [string, number];
      readonly ctyStringTupleWithString: [string, string];
      readonly ctyStringTupleWithUchar: [string, string];
      readonly ctyStringTupleWithByte: [string, number];
      readonly ctyStringTupleWithBytes: [string, string];
      readonly ctyUchar: string;
      readonly ctyUcharOption: string | null | undefined;
      readonly ctyUcharList: string[];
      readonly ctyUcharMap: Record<string, string>;
      readonly ctyUcharTupleWithUnit: [string, 1];
      readonly ctyUcharTupleWithBool: [string, boolean];
      readonly ctyUcharTupleWithInt: [string, number];
      readonly ctyUcharTupleWithInt53p: [string, number];
      readonly ctyUcharTupleWithFloat: [string, number];
      readonly ctyUcharTupleWithString: [string, string];
      readonly ctyUcharTupleWithUchar: [string, string];
      readonly ctyUcharTupleWithByte: [string, number];
      readonly ctyUcharTupleWithBytes: [string, string];
      readonly ctyByte: number;
      readonly ctyByteOption: number | null | undefined;
      readonly ctyByteList: number[];
      readonly ctyByteMap: Record<string, number>;
      readonly ctyByteTupleWithUnit: [number, 1];
      readonly ctyByteTupleWithBool: [number, boolean];
      readonly ctyByteTupleWithInt: [number, number];
      readonly ctyByteTupleWithInt53p: [number, number];
      readonly ctyByteTupleWithFloat: [number, number];
      readonly ctyByteTupleWithString: [number, string];
      readonly ctyByteTupleWithUchar: [number, string];
      readonly ctyByteTupleWithByte: [number, number];
      readonly ctyByteTupleWithBytes: [number, string];
      readonly ctyBytes: bindoj.bytes;
      readonly ctyBytesOption: string | null | undefined;
      readonly ctyBytesList: string[];
      readonly ctyBytesMap: Record<string, string>;
      readonly ctyBytesTupleWithUnit: [string, 1];
      readonly ctyBytesTupleWithBool: [string, boolean];
      readonly ctyBytesTupleWithInt: [string, number];
      readonly ctyBytesTupleWithInt53p: [string, number];
      readonly ctyBytesTupleWithFloat: [string, number];
      readonly ctyBytesTupleWithString: [string, string];
      readonly ctyBytesTupleWithUchar: [string, string];
      readonly ctyBytesTupleWithByte: [string, number];
      readonly ctyBytesTupleWithBytes: [string, string];
      readonly ctyStringEnum: "a" | "b" | "c";
      readonly tdExCoretypeVariousPrimTypes: ExCoretypeVariousPrimTypes;
      readonly tdExCoretypeWithInt53p: ExCoretypeWithInt53p;
      readonly tdExCoretypeVariousComplexTypes: ExCoretypeVariousComplexTypes;
      readonly tdExCoretypeVariousTupleTypes: ExCoretypeVariousTupleTypes;
      readonly tdExCoretypeNamedJson: ExCoretypeNamedJson;
      readonly tdExAliasUnit: null;
      readonly tdExAliasIntOpt: ExAliasIntOpt;
      readonly tdExAliasObjtuple: ExAliasObjtuple;
      readonly tdExRecordStudent: ExRecordStudent;
      readonly tdExRecordTeacher: ExRecordTeacher;
      readonly tdExVariantPerson: ExVariantPerson;
      readonly tdExVariantPersonReused: ExVariantPersonReused;
      readonly tdExVariantIntList: ExVariantIntList;
      readonly tdExVariantIntListObjtuple: ExVariantIntListObjtuple;
      readonly tdExVariantFoo: ExVariantFoo;
      readonly tdExVariantCustomizedUnion: ExVariantCustomizedUnion;
      readonly tdExManglingStudentInherited: ex_mangling_student_inherited;
      readonly tdExManglingPersonNoMangling: ex_mangling_person_no_mangling;
      readonly tdExManglingPersonInherited: ex_mangling_person_inherited;
      readonly tdExManglingEnum: ExManglingEnum;
      readonly tdExOptionalXyOpt: ExOptionalXyOpt;
      readonly tdExOptionalVariant: ExOptionalVariant;
      readonly tdExIdentStudentPair: ExIdentStudentPair;
      readonly tdExNestedPoint2: ExNestedPoint2;
      readonly tdExNestedRecord: ExNestedRecord;
      readonly tdExNestedVariant: ExNestedVariant;
      readonly tdExNestedMultiplyRecord: ExNestedMultiplyRecord;
      readonly tdExNestedMultiplyVariant: ExNestedMultiplyVariant;
      readonly tdExVersionSubstringRecordV3_2_1: ExVersionSubstringRecordV3_2_1;
      readonly tdExVersionSubstringVariantV1_0: ExVersionSubstringVariantV1_0;
    };
  };
  export type FullBridge_peerSetupOnly = {
    readonly bridgeAsync: Promise<ConcreteBridge>;
    readonly getBridge: () => ConcreteBridge | null;
    readonly setup: (peerFullBridge: bindoj.PeerFullBridgeReference<ConcreteBridge>) => void;
  };
  export const FullBridge: FullBridge_peerSetupOnly = bindoj.Internal.createFullBridgePeerSetupOnlyImpl(() => {
    const bridge = {
      peerObjects: {
        ctyUnit: undefined as unknown as null,
        ctyUnitOption: undefined as unknown as 1 | null | undefined,
        ctyUnitList: undefined as unknown as 1[],
        ctyUnitMap: undefined as unknown as Record<string, 1>,
        ctyUnitTupleWithUnit: undefined as unknown as [1, 1],
        ctyUnitTupleWithBool: undefined as unknown as [1, boolean],
        ctyUnitTupleWithInt: undefined as unknown as [1, number],
        ctyUnitTupleWithInt53p: undefined as unknown as [1, number],
        ctyUnitTupleWithFloat: undefined as unknown as [1, number],
        ctyUnitTupleWithString: undefined as unknown as [1, string],
        ctyUnitTupleWithUchar: undefined as unknown as [1, string],
        ctyUnitTupleWithByte: undefined as unknown as [1, number],
        ctyUnitTupleWithBytes: undefined as unknown as [1, string],
        ctyBool: undefined as unknown as boolean,
        ctyBoolOption: undefined as unknown as boolean | null | undefined,
        ctyBoolList: undefined as unknown as boolean[],
        ctyBoolMap: undefined as unknown as Record<string, boolean>,
        ctyBoolTupleWithUnit: undefined as unknown as [boolean, 1],
        ctyBoolTupleWithBool: undefined as unknown as [boolean, boolean],
        ctyBoolTupleWithInt: undefined as unknown as [boolean, number],
        ctyBoolTupleWithInt53p: undefined as unknown as [boolean, number],
        ctyBoolTupleWithFloat: undefined as unknown as [boolean, number],
        ctyBoolTupleWithString: undefined as unknown as [boolean, string],
        ctyBoolTupleWithUchar: undefined as unknown as [boolean, string],
        ctyBoolTupleWithByte: undefined as unknown as [boolean, number],
        ctyBoolTupleWithBytes: undefined as unknown as [boolean, string],
        ctyInt: undefined as unknown as number,
        ctyIntOption: undefined as unknown as number | null | undefined,
        ctyIntList: undefined as unknown as number[],
        ctyIntMap: undefined as unknown as Record<string, number>,
        ctyIntTupleWithUnit: undefined as unknown as [number, 1],
        ctyIntTupleWithBool: undefined as unknown as [number, boolean],
        ctyIntTupleWithInt: undefined as unknown as [number, number],
        ctyIntTupleWithInt53p: undefined as unknown as [number, number],
        ctyIntTupleWithFloat: undefined as unknown as [number, number],
        ctyIntTupleWithString: undefined as unknown as [number, string],
        ctyIntTupleWithUchar: undefined as unknown as [number, string],
        ctyIntTupleWithByte: undefined as unknown as [number, number],
        ctyIntTupleWithBytes: undefined as unknown as [number, string],
        ctyInt53p: undefined as unknown as number,
        ctyInt53pOption: undefined as unknown as number | null | undefined,
        ctyInt53pList: undefined as unknown as number[],
        ctyInt53pMap: undefined as unknown as Record<string, number>,
        ctyInt53pTupleWithUnit: undefined as unknown as [number, 1],
        ctyInt53pTupleWithBool: undefined as unknown as [number, boolean],
        ctyInt53pTupleWithInt: undefined as unknown as [number, number],
        ctyInt53pTupleWithInt53p: undefined as unknown as [number, number],
        ctyInt53pTupleWithFloat: undefined as unknown as [number, number],
        ctyInt53pTupleWithString: undefined as unknown as [number, string],
        ctyInt53pTupleWithUchar: undefined as unknown as [number, string],
        ctyInt53pTupleWithByte: undefined as unknown as [number, number],
        ctyInt53pTupleWithBytes: undefined as unknown as [number, string],
        ctyFloat: undefined as unknown as number,
        ctyFloatOption: undefined as unknown as number | null | undefined,
        ctyFloatList: undefined as unknown as number[],
        ctyFloatMap: undefined as unknown as Record<string, number>,
        ctyFloatTupleWithUnit: undefined as unknown as [number, 1],
        ctyFloatTupleWithBool: undefined as unknown as [number, boolean],
        ctyFloatTupleWithInt: undefined as unknown as [number, number],
        ctyFloatTupleWithInt53p: undefined as unknown as [number, number],
        ctyFloatTupleWithFloat: undefined as unknown as [number, number],
        ctyFloatTupleWithString: undefined as unknown as [number, string],
        ctyFloatTupleWithUchar: undefined as unknown as [number, string],
        ctyFloatTupleWithByte: undefined as unknown as [number, number],
        ctyFloatTupleWithBytes: undefined as unknown as [number, string],
        ctyString: undefined as unknown as string,
        ctyStringOption: undefined as unknown as string | null | undefined,
        ctyStringList: undefined as unknown as string[],
        ctyStringMap: undefined as unknown as Record<string, string>,
        ctyStringTupleWithUnit: undefined as unknown as [string, 1],
        ctyStringTupleWithBool: undefined as unknown as [string, boolean],
        ctyStringTupleWithInt: undefined as unknown as [string, number],
        ctyStringTupleWithInt53p: undefined as unknown as [string, number],
        ctyStringTupleWithFloat: undefined as unknown as [string, number],
        ctyStringTupleWithString: undefined as unknown as [string, string],
        ctyStringTupleWithUchar: undefined as unknown as [string, string],
        ctyStringTupleWithByte: undefined as unknown as [string, number],
        ctyStringTupleWithBytes: undefined as unknown as [string, string],
        ctyUchar: undefined as unknown as string,
        ctyUcharOption: undefined as unknown as string | null | undefined,
        ctyUcharList: undefined as unknown as string[],
        ctyUcharMap: undefined as unknown as Record<string, string>,
        ctyUcharTupleWithUnit: undefined as unknown as [string, 1],
        ctyUcharTupleWithBool: undefined as unknown as [string, boolean],
        ctyUcharTupleWithInt: undefined as unknown as [string, number],
        ctyUcharTupleWithInt53p: undefined as unknown as [string, number],
        ctyUcharTupleWithFloat: undefined as unknown as [string, number],
        ctyUcharTupleWithString: undefined as unknown as [string, string],
        ctyUcharTupleWithUchar: undefined as unknown as [string, string],
        ctyUcharTupleWithByte: undefined as unknown as [string, number],
        ctyUcharTupleWithBytes: undefined as unknown as [string, string],
        ctyByte: undefined as unknown as number,
        ctyByteOption: undefined as unknown as number | null | undefined,
        ctyByteList: undefined as unknown as number[],
        ctyByteMap: undefined as unknown as Record<string, number>,
        ctyByteTupleWithUnit: undefined as unknown as [number, 1],
        ctyByteTupleWithBool: undefined as unknown as [number, boolean],
        ctyByteTupleWithInt: undefined as unknown as [number, number],
        ctyByteTupleWithInt53p: undefined as unknown as [number, number],
        ctyByteTupleWithFloat: undefined as unknown as [number, number],
        ctyByteTupleWithString: undefined as unknown as [number, string],
        ctyByteTupleWithUchar: undefined as unknown as [number, string],
        ctyByteTupleWithByte: undefined as unknown as [number, number],
        ctyByteTupleWithBytes: undefined as unknown as [number, string],
        ctyBytes: undefined as unknown as bindoj.bytes,
        ctyBytesOption: undefined as unknown as string | null | undefined,
        ctyBytesList: undefined as unknown as string[],
        ctyBytesMap: undefined as unknown as Record<string, string>,
        ctyBytesTupleWithUnit: undefined as unknown as [string, 1],
        ctyBytesTupleWithBool: undefined as unknown as [string, boolean],
        ctyBytesTupleWithInt: undefined as unknown as [string, number],
        ctyBytesTupleWithInt53p: undefined as unknown as [string, number],
        ctyBytesTupleWithFloat: undefined as unknown as [string, number],
        ctyBytesTupleWithString: undefined as unknown as [string, string],
        ctyBytesTupleWithUchar: undefined as unknown as [string, string],
        ctyBytesTupleWithByte: undefined as unknown as [string, number],
        ctyBytesTupleWithBytes: undefined as unknown as [string, string],
        ctyStringEnum: undefined as unknown as "a" | "b" | "c",
        tdExCoretypeVariousPrimTypes: undefined as unknown as ExCoretypeVariousPrimTypes,
        tdExCoretypeWithInt53p: undefined as unknown as ExCoretypeWithInt53p,
        tdExCoretypeVariousComplexTypes: undefined as unknown as ExCoretypeVariousComplexTypes,
        tdExCoretypeVariousTupleTypes: undefined as unknown as ExCoretypeVariousTupleTypes,
        tdExCoretypeNamedJson: undefined as unknown as ExCoretypeNamedJson,
        tdExAliasUnit: undefined as unknown as null,
        tdExAliasIntOpt: undefined as unknown as ExAliasIntOpt,
        tdExAliasObjtuple: undefined as unknown as ExAliasObjtuple,
        tdExRecordStudent: undefined as unknown as ExRecordStudent,
        tdExRecordTeacher: undefined as unknown as ExRecordTeacher,
        tdExVariantPerson: undefined as unknown as ExVariantPerson,
        tdExVariantPersonReused: undefined as unknown as ExVariantPersonReused,
        tdExVariantIntList: undefined as unknown as ExVariantIntList,
        tdExVariantIntListObjtuple: undefined as unknown as ExVariantIntListObjtuple,
        tdExVariantFoo: undefined as unknown as ExVariantFoo,
        tdExVariantCustomizedUnion: undefined as unknown as ExVariantCustomizedUnion,
        tdExManglingStudentInherited: undefined as unknown as ex_mangling_student_inherited,
        tdExManglingPersonNoMangling: undefined as unknown as ex_mangling_person_no_mangling,
        tdExManglingPersonInherited: undefined as unknown as ex_mangling_person_inherited,
        tdExManglingEnum: undefined as unknown as ExManglingEnum,
        tdExOptionalXyOpt: undefined as unknown as ExOptionalXyOpt,
        tdExOptionalVariant: undefined as unknown as ExOptionalVariant,
        tdExIdentStudentPair: undefined as unknown as ExIdentStudentPair,
        tdExNestedPoint2: undefined as unknown as ExNestedPoint2,
        tdExNestedRecord: undefined as unknown as ExNestedRecord,
        tdExNestedVariant: undefined as unknown as ExNestedVariant,
        tdExNestedMultiplyRecord: undefined as unknown as ExNestedMultiplyRecord,
        tdExNestedMultiplyVariant: undefined as unknown as ExNestedMultiplyVariant,
        tdExVersionSubstringRecordV3_2_1: undefined as unknown as ExVersionSubstringRecordV3_2_1,
        tdExVersionSubstringVariantV1_0: undefined as unknown as ExVersionSubstringVariantV1_0,
      },
    };
    return bridge as ConcreteBridge;
  });
}
