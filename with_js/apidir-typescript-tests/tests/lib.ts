import { apidir as bindoj } from "../../public-packages/runtime/index";

export type MockServer = {
  handle_path_json_get: (path: string) => Promise<any>;
  handle_path_json_post: (path: string, reqBody: any) => Promise<any>;
};

export const createMockClient =
  <T extends bindoj.ApiDirInfoMap>(invpInfo: T) =>
  (mockServer: MockServer): bindoj.ApiDirClientPromiseIntf<bindoj.IsApiDirInfoMap<T>> => {
    const res: { [prop: string]: any } = {};
    for (const [k, v] of Object.entries(invpInfo)) {
      if (v.method === "GET") {
        res[k] = (): Promise<{body: typeof v.resp_type, status_code: number}> => {
          return mockServer.handle_path_json_get(v.urlpath);
        };
      } else {
        res[k] = (reqBody: typeof v): Promise<{body: typeof v.resp_type, status_code: number}> => {
          return mockServer.handle_path_json_post(v.urlpath, reqBody);
        };
      }
    }
    return res as bindoj.ApiDirClientPromiseIntf<bindoj.IsApiDirInfoMap<T>>;
  };
