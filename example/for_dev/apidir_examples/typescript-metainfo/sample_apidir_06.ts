import { apidir as bindoj } from "../public-packages/runtime/index";
import { ExOptionalXyOpt } from "../compile-tests/ex_optional_gen";

export const Sample_apidir_06InvpInfo = {
  "get-x": {
    name: "get-x",
    method: "POST",
    urlpath: "/xy-opt/x",
    requestType: undefined as unknown as ExOptionalXyOpt,
    responseType: undefined as unknown as number | null | undefined,
  },
  "get-y": {
    name: "get-y",
    method: "POST",
    urlpath: "/xy-opt/y",
    requestType: undefined as unknown as ExOptionalXyOpt,
    responseType: undefined as unknown as number | null | undefined,
  },
} as const;
export type Sample_apidir_06InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Sample_apidir_06InvpInfo>;
export type Sample_apidir_06ClientIntf = bindoj.ApiDirClientPromiseIntf<Sample_apidir_06InvpInfoMap>;
