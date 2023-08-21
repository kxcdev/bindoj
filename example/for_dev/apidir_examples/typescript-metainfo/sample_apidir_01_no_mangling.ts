import { apidir as bindoj } from "../public-packages/runtime/index";
import { person_no_mangling } from "../compile-tests/ex02_no_mangling_gen";
import { Student } from "../compile-tests/ex01_gen";

export const Sample_apidir_01_no_manglingInvpInfo = {
  "get-any-student": {
    name: "get-any-student",
    method: "GET",
    urlpath: "/student/any-one",
    resp_type: undefined as unknown as Student,
  },
  "get-student-from-person": {
    name: "get-student-from-person",
    method: "POST",
    urlpath: "/student/from-person",
    req_type: undefined as unknown as person_no_mangling,
    resp_type: undefined as unknown as Student,
  },
} as const;
export type Sample_apidir_01_no_manglingInvpInfoMap = bindoj.IsApiDirInfoMap<
  typeof Sample_apidir_01_no_manglingInvpInfo
>;
export type Sample_apidir_01_no_manglingClientIntf =
  bindoj.ApiDirClientPromiseIntf<Sample_apidir_01_no_manglingInvpInfoMap>;
