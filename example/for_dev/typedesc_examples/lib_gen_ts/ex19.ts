export type PreservingVersionInfoV1_0 =
  | {
      kind: "no-preserving-version-substring-v1-0";
      versionInfoV1: number;
      versionInfoV10: number;
      versionInfoV101: number;
    }
  | { kind: "v1_0-version-info"; v1_0_1VersionInfo: number; v1_0VersionInfo: number; v1VersionInfo: number }
  | { kind: "version-info-v1_0"; versionInfoV1: number; versionInfoV1_0: number; versionInfoV1_0_1: number }
  | { kind: "version-v1_0-info"; versionV1_0_1Info: number; versionV1_0Info: number; versionV1Info: number };
export function analyzePreservingVersionInfoV1_0<__bindoj_ret>(__bindoj_fns: {
  "no-preserving-version-substring-v1-0": (__bindoj_v: {
    kind: "no-preserving-version-substring-v1-0";
    versionInfoV1: number;
    versionInfoV10: number;
    versionInfoV101: number;
  }) => __bindoj_ret;
  "v1_0-version-info": (__bindoj_v: {
    kind: "v1_0-version-info";
    v1_0_1VersionInfo: number;
    v1_0VersionInfo: number;
    v1VersionInfo: number;
  }) => __bindoj_ret;
  "version-info-v1_0": (__bindoj_v: {
    kind: "version-info-v1_0";
    versionInfoV1: number;
    versionInfoV1_0: number;
    versionInfoV1_0_1: number;
  }) => __bindoj_ret;
  "version-v1_0-info": (__bindoj_v: {
    kind: "version-v1_0-info";
    versionV1_0_1Info: number;
    versionV1_0Info: number;
    versionV1Info: number;
  }) => __bindoj_ret;
}): (__bindoj_x: PreservingVersionInfoV1_0) => __bindoj_ret {
  return (__bindoj_x: PreservingVersionInfoV1_0) => {
    if (__bindoj_x.kind === "no-preserving-version-substring-v1-0") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "v1_0-version-info") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "version-info-v1_0") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "version-v1_0-info") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzePreservingVersionInfoV1_0 - unrecognized: " + __bindoj_x);
    }
  };
}
