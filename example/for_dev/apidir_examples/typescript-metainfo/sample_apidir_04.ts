import { apidir as bindoj } from "../public-packages/runtime/index";
import { ExCoretypeNamedJson } from "../compile-tests/ex_coretype_gen";
type String = string;
export const Sample_apidir_04InvpInfo = {
  "name-of-named_json": {
    name: "name-of-named_json",
    method: "POST",
    urlpath: "/named_json/name",
    requestType: undefined as unknown as ExCoretypeNamedJson,
    responseType: undefined as unknown as String,
  },
  "json-of-named_json": {
    name: "json-of-named_json",
    method: "POST",
    urlpath: "/named_json/json",
    requestType: undefined as unknown as ExCoretypeNamedJson,
    responseType: undefined as unknown as any,
  },
} as const;
export type Sample_apidir_04InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Sample_apidir_04InvpInfo>;
export type Sample_apidir_04ClientIntf = bindoj.ApiDirClientPromiseIntf<Sample_apidir_04InvpInfoMap>;
