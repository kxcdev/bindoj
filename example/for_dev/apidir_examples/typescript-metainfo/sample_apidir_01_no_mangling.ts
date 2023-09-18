import { apidir as bindoj } from "../public-packages/runtime/index";
import { ex_mangling_person_no_mangling } from "../compile-tests/ex_mangling_gen";
import { ExRecordStudent } from "../compile-tests/ex_record_gen";

export const Sample_apidir_01_no_manglingInvpInfo = {
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
    requestType: undefined as unknown as ex_mangling_person_no_mangling,
    responseType: undefined as unknown as ExRecordStudent,
  },
} as const;
export type Sample_apidir_01_no_manglingInvpInfoMap = bindoj.IsApiDirInfoMap<
  typeof Sample_apidir_01_no_manglingInvpInfo
>;
export type Sample_apidir_01_no_manglingClientIntf =
  bindoj.ApiDirClientPromiseIntf<Sample_apidir_01_no_manglingInvpInfoMap>;
