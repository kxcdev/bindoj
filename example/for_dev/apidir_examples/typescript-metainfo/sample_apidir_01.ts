import { apidir as bindoj } from "../public-packages/runtime/index";
import { Person } from "../compile-tests/ex02_gen";
import { Student } from "../compile-tests/ex01_gen";

export const Sample_apidir_01InvpInfo = {
  "get-any-student": {
    name: "get-any-student",
    method: "GET",
    urlpath: "/student/any-one",
    responseType: undefined as unknown as Student,
  },
  "get-student-from-person": {
    name: "get-student-from-person",
    method: "POST",
    urlpath: "/student/from-person",
    requestType: undefined as unknown as Person,
    responseType: undefined as unknown as Student,
  },
} as const;
export type Sample_apidir_01InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Sample_apidir_01InvpInfo>;
export type Sample_apidir_01ClientIntf = bindoj.ApiDirClientPromiseIntf<Sample_apidir_01InvpInfoMap>;
