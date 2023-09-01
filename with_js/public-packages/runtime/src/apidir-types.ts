export type ApiDirGetInvpInfo<Req> = {
  name: string;
  urlpath: string;
  responseType: Req;
  method: "GET";
};
export type ApiDirPostInvpInfo<Req, Resp> = {
  name: string;
  urlpath: string;
  requestType: Req;
  responseType: Resp;
  method: "POST";
};

export type ApiDirInfoMap = {
  [invp: string]: ApiDirGetInvpInfo<unknown> | ApiDirPostInvpInfo<unknown, unknown>;
};

export type IsApiDirInfoMap<Dir extends ApiDirInfoMap> = Dir;

export type ApiDirAllInvps<InfoMap> = keyof InfoMap;

export type ApiDirInvpRequestType<
  Dir extends ApiDirInfoMap,
  invp extends keyof Dir
> = Dir[invp] extends ApiDirPostInvpInfo<unknown, unknown> ? Dir[invp]["requestType"] : undefined;

export type ApiDirInvpResponseType<Dir extends ApiDirInfoMap, invp extends keyof Dir> = Dir[invp]["responseType"];

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
