import { apidir as bindoj } from "../public-packages/runtime/index";
import { ComplexTypesNotuple } from "../compile-tests/ex05_notuple_gen";

export const Sample_apidir_05InvpInfo = {
  "int-of-string": {
    name: "int-of-string",
    method: "POST",
    urlpath: "/option/int-of-string",
    req_type: undefined as unknown as String,
    resp_type: undefined as unknown as number | null | undefined,
  },
  "option-of-complex": {
    name: "option-of-complex",
    method: "POST",
    urlpath: "/option/of-complex",
    req_type: undefined as unknown as ComplexTypesNotuple,
    resp_type: undefined as unknown as number | null | undefined,
  },
} as const;
export type Sample_apidir_05InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Sample_apidir_05InvpInfo>;
export type Sample_apidir_05ClientIntf = bindoj.ApiDirClientPromiseIntf<Sample_apidir_05InvpInfoMap>;
