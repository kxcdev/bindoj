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
  export type EndemicSetupParams = {
    readonly endemicObjects: {
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
  export type ConcreteBridge = {};
  export type FullBrdge_endemicSetupOnly = (initParams: EndemicSetupParams) => {
    readonly bridge: ConcreteBridge;
    readonly setup: (peerFullBridge: bindoj.PeerFullBridgeReference<ConcreteBridge>) => void;
  };
  export const FullBridge: FullBrdge_endemicSetupOnly = bindoj.Internal.createFullBridgeEndemicSetupOnlyImpl(
    (initParams: EndemicSetupParams) => {
      const bridge = {
        endemicObjects: initParams.endemicObjects,
      };
      return bridge as ConcreteBridge;
    }
  );
}
