export type Foo = { kind: "foo0" } | { kind: "foo1"; arg: number } | { kind: "foo2"; arg: [number, number] };
export function analyzeFoo<__bindoj_ret>(__bindoj_fns: {
  foo0: (__bindoj_v: { kind: "foo0" }) => __bindoj_ret;
  foo1: (__bindoj_v: { kind: "foo1"; arg: number }) => __bindoj_ret;
  foo2: (__bindoj_v: { kind: "foo2"; arg: [number, number] }) => __bindoj_ret;
}): (__bindoj_x: Foo) => __bindoj_ret {
  return (__bindoj_x: Foo) => {
    if (__bindoj_x.kind === "foo0") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "foo1") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "foo2") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzeFoo - unrecognized: " + __bindoj_x);
    }
  };
}
