import { objintf as bindoj } from "../../public-packages/runtime";
import { GeneratedBridge, UnitMod, UnitObj } from "../ts/ex_objintf_one_directional_gen";

const usage: {
  full_bridge: GeneratedBridge.PeerFullBridgeReference;
  register: () => void;
  buffer(): string[];
  clear_buffer: () => void;
} = require("./js/ex_objintf_one_directional_trans_usage.js").usage_one_directional_trans;

const fullBridge = GeneratedBridge.FullBridge;

function expectBuffer(expected: string[]) {
  const buffer = usage.buffer();
  expect(buffer.length).toStrictEqual(expected.length);
  buffer.forEach((item, i) => {
    expect(item).toBe(expected[i]);
  });
}

describe("ex_objintf_one_directinoal_trans", () => {
  beforeAll(() => {
    expect(usage).not.toBeUndefined();
    fullBridge.setup(usage.full_bridge);
  });

  beforeEach(() => {
    usage.clear_buffer();
  });

  const b = fullBridge.getBridge()!;

  const fromTs = (s: string) => s + " from TypeScript";
  const myHello = (name: string) => {
    (bindoj.access(b.peerObjects.myHello))(fromTs(name));
  };

  test("helloMyString", () => {
    const name = b.peerObjects.myString;
    myHello(name);
    expectBuffer([
      "[my_hello] ABC from TypeScript",
    ]);
  });

  test("helloSoleVar", () => {
    myHello(
      bindoj.access(b.peerObjects.mySoleVar)(),
    );
    expectBuffer([
      "[my_hello] my_sole_var from TypeScript",
    ]);
  });

  test("helloMyStringUnitSole", () => {
    const name = b.peerObjects.myString;
    myHello(
      bindoj.access(b.peerObjects.myUnitSole)(name),
    );
    expectBuffer([
      "[my_hello] ABC! from TypeScript",
    ]);
  });

  test("helloMyStringUnitObj", () => {
    const hello = (f: (_: UnitObj) => string) => {
      myHello(
        f(bindoj.access(b.peerObjects.myUnitObj)),
      );
    };
    hello((o) => o.name());
    hello((o) => o.unit01(o.name()));
    hello((o) => o.unit02(o.name()));
    hello((o) => o.unit03(o.name()));
    
    expectBuffer([
      "[my_hello] my_unit_obj from TypeScript",
      "[my_hello] ppp_my_unit_obj_o from TypeScript",
      "[my_hello] pp_my_unit_obj_oo from TypeScript",
      "[my_hello] p_my_unit_obj_ooo from TypeScript",
    ]);
  });

  test("helloMyStringUnitMod", () => {
    const hello = (f: (_: UnitMod) => string) => {
      myHello(
        f(bindoj.access(b.peerObjects.myUnitMod)),
      );
    };
    hello((o) => o.name());
    hello((o) => o.unit01(o.name()));
    hello((o) => o.unit02(o.name()));
    hello((o) => o.unit03(o.name()));
    
    expectBuffer([
      "[my_hello] my_unit_mod from TypeScript",
      "[my_hello] ppp_my_unit_mod_m from TypeScript",
      "[my_hello] pp_my_unit_mod_mm from TypeScript",
      "[my_hello] p_my_unit_mod_mmm from TypeScript",
    ]);
  });

  test("helloRegistry", () => {
    const getName = () => b.peerObjectRegistry.lookupString({ cdnId0: "1", cdnId1: 1.2});

    expect(getName()).toBeNull();
    
    usage.register();

    const name = getName();
    const hello = b.peerObjectRegistry.lookupHello({ cdnId: "foo" });
    

    expect(name).not.toBeNull();
    expect(hello).not.toBeNull();

    bindoj.access(hello)!(fromTs(name!));

    expectBuffer([
      "hello (foo): registered string from TypeScript",
    ]);
  });

  test("helloRecObj", () => {
    const name =
      bindoj.access(
        bindoj.access(b.peerObjects.myRecObj).getSelf()
      ).name();
    myHello(name);

    expectBuffer([
      "[my_hello] my_rec_obj from TypeScript",
    ]);
  });

  test("helloRecObj", () => {
    const v = b.peerObjects.myNonJsonValues;

    const decoder = new TextDecoder();
    myHello(decoder.decode(v.bytes));

    expectBuffer([
      "[my_hello] Hello, world! from TypeScript",
    ]);
  });

  test("withDefaultValue", () => {
    const w = b.peerObjects.withDefaultValue;
    
    myHello(w.getDefaultString());
    myHello(w.getDefaultStudent());

    expectBuffer([
      "[my_hello] Hello from TypeScript",
      "[my_hello] William Gibson from TypeScript",
    ]);
  })
});
