import { apidir as bindoj } from "../public-packages/runtime/index";
import { IntListObjtuple } from "../compile-tests/ex03_objtuple_gen";
type Int = number;
export const Sample_apidir_02InvpInfo = {
  "get-any-int-list": {
    name: "get-any-int-list",
    method: "GET",
    urlpath: "/int-list/any-one",
    resp_type: undefined as unknown as IntListObjtuple,
  },
  "inc-int-list": {
    name: "inc-int-list",
    method: "POST",
    urlpath: "/int-list/inc",
    req_type: undefined as unknown as IntListObjtuple,
    resp_type: undefined as unknown as IntListObjtuple,
  },
  "sum-of-int-list": {
    name: "sum-of-int-list",
    method: "POST",
    urlpath: "/int-list/sum",
    req_type: undefined as unknown as IntListObjtuple,
    resp_type: undefined as unknown as Int,
  },
} as const;
export type Sample_apidir_02InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Sample_apidir_02InvpInfo>;
export type Sample_apidir_02ClientIntf = bindoj.ApiDirClientPromiseIntf<Sample_apidir_02InvpInfoMap>;
