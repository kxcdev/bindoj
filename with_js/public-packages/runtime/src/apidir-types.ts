export type ApiDirGetInvpInfo = {
  name: string;
  urlpath: string;
  resp_type: unknown;
  method: "GET";
};
export type ApiDirPostInvpInfo = {
  name: string;
  urlpath: string;
  req_type: unknown;
  resp_type: unknown;
  method: "POST";
};

export type ApiDirInfoMap = {
  [invp: string]: ApiDirGetInvpInfo | ApiDirPostInvpInfo;
};

export type IsApiDirInfoMap<Dir extends ApiDirInfoMap> = Dir;

export type ApiDirAllInvps<InfoMap> = keyof InfoMap;

export type ApiDirInvpRequestType<
  Dir extends ApiDirInfoMap,
  invp extends keyof Dir
> = Dir[invp] extends ApiDirPostInvpInfo ? Dir[invp]["req_type"] : undefined;

export type ApiDirInvpResponseType<Dir extends ApiDirInfoMap, invp extends keyof Dir> = Dir[invp]["resp_type"];

export type ApiDirInvpFunctionType<
  Dir extends ApiDirInfoMap,
  invp extends keyof Dir,
  Opt extends unknown[] = [],
  respty = Promise<{ body: ApiDirInvpResponseType<Dir, invp>; status_code: number }>
> = ApiDirInvpRequestType<Dir, invp> extends undefined
  ? (...extraArgs: Opt) => respty
  : (reqBody: ApiDirInvpRequestType<Dir, invp>, ...extraArgs: Opt) => respty;

export type ApiDirClientPromiseIntf<Dir extends ApiDirInfoMap, Opt extends unknown[] = []> = {
  [invp in keyof Dir]: ApiDirInvpFunctionType<Dir, invp, Opt>;
};
