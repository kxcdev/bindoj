import { apidir as bindoj } from "../../../with_js/public-packages/runtime/index";
import { Person, Student } from "./ex01";

export const Ex01InvpInfo = {
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
    req_type: undefined as unknown as Person,
    resp_type: undefined as unknown as Student | string,
  },
} as const;
export type Ex01InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Ex01InvpInfo>;
export type Ex01ClientIntf = bindoj.ApiDirClientPromiseIntf<Ex01InvpInfoMap>;
