import { objintf as bindoj } from "../../public-packages/runtime";
import { ByteSource, GeneratedBridge } from "../ts/ex_objintf_system_io_gen";

const usage: {
  full_bridge: GeneratedBridge.PeerFullBridgeReference;
  usage(): {
    register_remote_logger: () => { output: () => string };
    clear_stdout: () => void;
    get_stdout_content: () => string;
    get_file_content: (path: string) => string;
  } | null;
} = require("./js/ex_objintf_system_io_trans_usage.js").usage_system_io_trans;


const initParams: GeneratedBridge.EndemicSetupParams = {
  initiallyRegisteredObjects: {
    logger: [],
  },
  endemicObjects: {
    myLogger: {
      info: (s) => console.log(s),
      error: (s, labeledArgs?) => {
        const exn = labeledArgs?.exn;
        if (exn === undefined) {
          console.log(s);
        } else {
          console.log(`[${exn}] ${s}`);
        }
      }
    }
  },
};

const fullBridge = GeneratedBridge.FullBridge(initParams);

const encoder = new TextEncoder();
const decoder = new TextDecoder();

describe("ex_objintf_system_io_trans", () => {
  beforeAll(() => {
    expect(usage).not.toBeUndefined();
    expect(usage.usage()).toBeNull();
    fullBridge.setup(usage.full_bridge);
    expect(usage.usage()).not.toBeNull();
  });

  test("example_remote_logging", async () => {
    const { output } = usage.usage()!.register_remote_logger();

    const logger = (await fullBridge.bridgeAsync).peerObjectRegistry.lookupLogger({ name: "remote", variant: "persistent" });

    expect(logger).not.toBeNull();

    const message = "hello from Usage1";

    logger!.info(message);

    expect(output()).toStrictEqual(message);
  });

  test("example_stdout", async () => {
    const stdout = (await fullBridge.bridgeAsync).peerObjects.systemIo.stdout();

    const message = "example_stdout";

    stdout.write(encoder.encode(message));

    expect(usage.usage()!.get_stdout_content()).toStrictEqual(message);
  });

  test("example_pass_endemic_object", async () => {
    usage.usage()!.clear_stdout();

    const stdout = (await fullBridge.bridgeAsync).peerObjects.systemIo.stdout();
    const message = "example_pass_endemic_object";
    const buf = encoder.encode(message);
    let index = 0;
    const src : ByteSource = {
      bytesLeft: () => Math.max(0, buf.length - index),
      nextBlock: (labeledArgs?: { max?: number }) => {
        const max = labeledArgs?.max;
        if (max == null) {
          return buf.slice(index);
        } else {
          const res = buf.slice(index, index + max);
          index += max;
          return res;
        }
      },
    };

    stdout.writeBulk(src);

    expect(usage.usage()!.get_stdout_content()).toStrictEqual(message);
  });

  test("example_file_write", async () => {
    const message = "example_file_write";
    const buf = encoder.encode(message);
    let index = 0;

    const output = (size: number) => {
      const res = buf.slice(index, index + size);
      index += size;
      return bindoj.bytesToBase64String(res);
    };

    bindoj.access((await fullBridge.bridgeAsync).peerObjects.systemIo)
      .openFileWo({ path: "text.txt" })
      .writeAsync(bindoj.bridge((labeledArgs?: { max?: number }) => {
        const m = labeledArgs?.max;
        const len = buf.length - index;
        if (len === 0) {
          return { kind: "eof" };
        } else if (m != null && m < len) {
          return { kind: "data", value: [output(m), "maybe-more"] };
        } else {
          return { kind: "data", value: [output(len), "eof" ]}
        }
      }));

    expect(usage.usage()!.get_file_content("text.txt")).toStrictEqual(message);
  });
});