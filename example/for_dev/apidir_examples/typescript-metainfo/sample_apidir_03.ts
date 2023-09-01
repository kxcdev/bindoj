import { apidir as bindoj } from "../public-packages/runtime/index";
import { Person } from "../compile-tests/ex02_gen";
type Int = number;
export const Sample_apidir_03InvpInfo = {
  "id-of-person": {
    name: "id-of-person",
    method: "POST",
    urlpath: "/person/id",
    requestType: undefined as unknown as Person,
    responseType: undefined as unknown as Int | String,
  },
} as const;
export type Sample_apidir_03InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Sample_apidir_03InvpInfo>;
export type Sample_apidir_03ClientIntf = bindoj.ApiDirClientPromiseIntf<Sample_apidir_03InvpInfoMap>;
