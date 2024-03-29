import { apidir as bindoj } from "../public-packages/runtime/index";
import { ExVariantIntListObjtuple } from "../compile-tests/ex_variant_gen";
type Int = number;
export const Sample_apidir_02InvpInfo = {
  "get-any-int-list": {
    name: "get-any-int-list",
    method: "GET",
    urlpath: "/int-list/any-one",
    responseType: undefined as unknown as ExVariantIntListObjtuple,
  },
  "inc-int-list": {
    name: "inc-int-list",
    method: "POST",
    urlpath: "/int-list/inc",
    requestType: undefined as unknown as ExVariantIntListObjtuple,
    responseType: undefined as unknown as ExVariantIntListObjtuple,
  },
  "sum-of-int-list": {
    name: "sum-of-int-list",
    method: "POST",
    urlpath: "/int-list/sum",
    requestType: undefined as unknown as ExVariantIntListObjtuple,
    responseType: undefined as unknown as Int,
  },
} as const;
export type Sample_apidir_02InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Sample_apidir_02InvpInfo>;
export type Sample_apidir_02ClientIntf = bindoj.ApiDirClientPromiseIntf<Sample_apidir_02InvpInfoMap>;
