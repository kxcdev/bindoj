import { apidir as bindoj } from "../public-packages/runtime/index";
import { ExVariantPerson } from "../compile-tests/ex_variant_gen";
import { ExRecordStudent } from "../compile-tests/ex_record_gen";

export const Sample_apidir_01InvpInfo = {
  "get-any-student": {
    name: "get-any-student",
    method: "GET",
    urlpath: "/student/any-one",
    responseType: undefined as unknown as ExRecordStudent,
  },
  "get-student-from-person": {
    name: "get-student-from-person",
    method: "POST",
    urlpath: "/student/from-person",
    requestType: undefined as unknown as ExVariantPerson,
    responseType: undefined as unknown as ExRecordStudent,
  },
} as const;
export type Sample_apidir_01InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Sample_apidir_01InvpInfo>;
export type Sample_apidir_01ClientIntf = bindoj.ApiDirClientPromiseIntf<Sample_apidir_01InvpInfoMap>;
