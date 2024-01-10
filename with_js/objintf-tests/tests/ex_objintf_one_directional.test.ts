import { objintf as bindoj } from "../../public-packages/runtime";
import { GeneratedBridge } from "../ts/ex_objintf_one_directional_trans_gen";

const usage: {
  full_bridge: GeneratedBridge.PeerFullBridgeReference;
  usage(): {
    hello_my_string: () => void,
    hello_sole_var: () => void,
    hello_my_string_unit_sole: () => void,
    hello_my_string_unit_obj: () => void,
    hello_my_string_unit_mod: () => void,
    hello_registry: () => void,
    hello_rec_obj: () => void,
    hello_non_json_values: () => void,
    hello_with_default_value: () => void,
  } | null;
} = require("./js/ex_objintf_one_directional_usage.js").usage_one_directional;

const makeUnitObj = (name: string, prefix: string, suffix: string) => ({
  name: () => name,
  unit01: (__arg0: string) => prefix.repeat(3) + "_" + __arg0 + "_" + suffix.repeat(1),
  unit02: (__arg0: string) => prefix.repeat(2) + "_" + __arg0 + "_" + suffix.repeat(2),
  unit03: (__arg0: string) => prefix.repeat(1) + "_" + __arg0 + "_" + suffix.repeat(3),
});

const buffer = new Array();

const clearBuffer = () => buffer.length = 0;

const initParams: GeneratedBridge.EndemicSetupParams = {
  initiallyRegisteredObjects: {
    string: [],
    hello: [
      [ { id: "foo" }, (s) => buffer.push("hello(foo): " + s)],
      [ { id: "bar" }, (s) => buffer.push("hello(bar): " + s)]
    ]
  },
  endemicObjects: (() => {
    const myRecObj = {
      name: () => "myRecObj",
      getSelf: () => bindoj.bridge(myRecObj),
    };
    const encoder = new TextEncoder();
    return {
      myString: "abc",
      myHello: (name: string) => buffer.push(`[myHello] ${name}`),
      mySoleVar: () => "mySoleVar",
      myUnitSole: (s) => s + "!",
      myUnitObj: makeUnitObj("myUnitObj", "p", "o"),
      myUnitMod: makeUnitObj("myUnitMod", "p", "m"),
      myRecObj,
      myNonJsonValues: {
        bytes: encoder.encode("Hello, world!"),
      },
      withDefaultValue: {
        getDefaultString(labeledArgs) {
          return labeledArgs!.str!;
        },
        getDefaultStudent(labeledArgs) {
          return labeledArgs!.student!.name;
        },
      }
    };
  })(),
};

const fullBridge = GeneratedBridge.FullBridge(initParams);

describe("ex_objintf_one_directinoal", () => {
  beforeAll(() => {
    expect(usage).not.toBeUndefined();
    expect(usage.usage()).toBeNull();
    fullBridge.setup(usage.full_bridge);
    expect(usage.usage()).not.toBeNull();
  });

  beforeEach(() => {
    clearBuffer();
  });

  function expectBuffer(expected: string[]) {
    expect(buffer.length).toStrictEqual(expected.length);
    buffer.forEach((item, i) => {
      expect(item).toBe(expected[i]);
    })
  }

  test("hello_my_string", () => {
    usage.usage()?.hello_my_string();
    expectBuffer([
      "[myHello] abc from OCaml",
    ]);
  });

  test("hello_sole_var", () => {
    usage.usage()?.hello_sole_var();
    expectBuffer([
      "[myHello] mySoleVar from OCaml",
    ]);
  });

  test("hello_my_string_unit_sole", () => {
    usage.usage()?.hello_my_string_unit_sole();
    expectBuffer([
      "[myHello] abc! from OCaml",
    ]);
  });

  test("hello_my_string_unit_obj", () => {
    usage.usage()?.hello_my_string_unit_obj();
    expectBuffer([
      "[myHello] myUnitObj from OCaml",
      "[myHello] ppp_myUnitObj_o from OCaml",
      "[myHello] pp_myUnitObj_oo from OCaml",
      "[myHello] p_myUnitObj_ooo from OCaml",
    ]);
  });

  test("hello_my_string_unit_mod", () => {
    usage.usage()?.hello_my_string_unit_mod();
    expectBuffer([
      "[myHello] myUnitMod from OCaml",
      "[myHello] ppp_myUnitMod_m from OCaml",
      "[myHello] pp_myUnitMod_mm from OCaml",
      "[myHello] p_myUnitMod_mmm from OCaml",
    ]);
  });

  test("hello_registry", async () => {
    const bridge = await fullBridge.bridgeAsync;
    bridge.endemicObjectRegistry.string
      .register({ id0: "1", id1: 1.2 }, "registered string");
    
    usage.usage()?.hello_registry();

    expectBuffer([
      "hello(foo): registered string from OCaml",
    ]);

  });

  test("hello_rec_obj", () => {
    usage.usage()?.hello_rec_obj();
    expectBuffer([
      "[myHello] myRecObj from OCaml",
    ]);
  });

  test("hello_rec_obj", () => {
    usage.usage()?.hello_non_json_values();
    expectBuffer([
      "[myHello] Hello, world! from OCaml",
    ]);
  });

  test("hello_with_default_value", () => {
    usage.usage()?.hello_with_default_value();
    expectBuffer([
      "[myHello] Hello from OCaml",
      "[myHello] William Gibson from OCaml",
    ]);
  });
})