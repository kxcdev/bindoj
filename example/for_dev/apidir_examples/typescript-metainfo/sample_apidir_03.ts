import { apidir as bindoj } from "../public-packages/runtime/index";
import { ExVariantPerson } from "../compile-tests/ex_variant_gen";
type Int = number;
type String = string;
export const Sample_apidir_03InvpInfo = {
  "id-of-person": {
    name: "id-of-person",
    method: "POST",
    urlpath: "/person/id",
    requestType: undefined as unknown as ExVariantPerson,
    responseType: undefined as unknown as Int | String,
  },
} as const;
export type Sample_apidir_03InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Sample_apidir_03InvpInfo>;
export type Sample_apidir_03ClientIntf = bindoj.ApiDirClientPromiseIntf<Sample_apidir_03InvpInfoMap>;
