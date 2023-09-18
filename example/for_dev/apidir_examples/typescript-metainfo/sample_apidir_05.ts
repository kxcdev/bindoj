import { apidir as bindoj } from "../public-packages/runtime/index";
import { ExCoretypeVariousComplexTypes } from "../compile-tests/ex_coretype_gen";
type String = string;
export const Sample_apidir_05InvpInfo = {
  "int-of-string": {
    name: "int-of-string",
    method: "POST",
    urlpath: "/option/int-of-string",
    requestType: undefined as unknown as String,
    responseType: undefined as unknown as number | null | undefined,
  },
  "option-of-complex": {
    name: "option-of-complex",
    method: "POST",
    urlpath: "/option/of-complex",
    requestType: undefined as unknown as ExCoretypeVariousComplexTypes,
    responseType: undefined as unknown as number | null | undefined,
  },
} as const;
export type Sample_apidir_05InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Sample_apidir_05InvpInfo>;
export type Sample_apidir_05ClientIntf = bindoj.ApiDirClientPromiseIntf<Sample_apidir_05InvpInfoMap>;
