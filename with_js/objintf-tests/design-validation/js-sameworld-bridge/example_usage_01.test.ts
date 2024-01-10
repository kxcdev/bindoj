import { objintf as bindoj } from "../../../public-packages/runtime";
import { ByteSource, ByteSource$PRIME$, Generated_bridge } from "./example_usage_01_generated";

const example_usage_01: {
  full_bridge: Generated_bridge.PeerFullBridgeReference;
  usage(): {
    example_remote_logging: () => void;
    example_stdout: () => void;
    example_pass_endemic_object: () => void;
    example_file_write: () => void;
  } | null;
} = require("./example_usage_01.js").example_usage_01;

type OutputChannel = Generated_bridge.Interfaces.OutputChannel & { buf: Uint8Array };

const createOutputChannel = (name: string): OutputChannel => {
  const buffer = {
    buf: new Uint8Array(),
    channelName: () => name,
    write: (src: Uint8Array) => {
      const newBuf = new Uint8Array(buffer.buf.length + src.length);
      newBuf.set(buffer.buf, 0);
      newBuf.set(src, buffer.buf.length);
      buffer.buf = newBuf;
    },
    writeBulk: (src: bindoj.peer<ByteSource>) => {
      let bytesLeft = bindoj.access(src).bytesLeft();
      while (bytesLeft > 0) {
        buffer.write(bindoj.access(src).nextBlock({ max: 1 }));
        bytesLeft = bindoj.access(src).bytesLeft();
      }
    },
    writeAsync: (src: bindoj.peer<ByteSource$PRIME$>) => {
      let state = bindoj.access(src)();
      while (state.kind !== "eof") {
        if (state.kind === "data") {
          const [b, s] = state.value;
          buffer.write(bindoj.base64StringToBytes(b));
          if (s === "eof") break;
        }
        state = bindoj.access(src)();
      }
    },
  };

  return buffer;
};

const mockStdout = createOutputChannel("stdout");
const mockStderr = createOutputChannel("stderr");
const mockFiles: Record<string, OutputChannel> = {};

const initParams: Generated_bridge.EndemicSetupParams = {
  initiallyRegisteredObjects: {
    logger: [],
  },
  endemicObjects: {
    systemIo: {
      stdout: mockStdout,
      stderr: mockStderr,
      openFileWo: ({ path }: { path: string }) => {
        let channel = mockFiles[path];
        if (channel === undefined) {
          channel = createOutputChannel(path);
          mockFiles[path] = channel;
        }

        return channel;
      },
      openFileRo: ({ path }: { path: string }) => {
        let channel = mockFiles[path];
        if (channel === undefined) {
          channel = createOutputChannel(path);
          mockFiles[path] = channel;
        }

        let index = 0;
        return {
          bytesLeft: () => channel.buf.length - index,
          nextBlock: (labeledArgs?: { max?: number }) => {
            const max = labeledArgs?.max;
            if (max == undefined) {
              return channel.buf;
            } else {
              const res = channel.buf.slice(index, max);
              index += max;
              return res;
            }
          },
        };
      },
    },
  },
};
const fullBridge = Generated_bridge.FullBridge(initParams);

describe("Objintf.example_usage_01", () => {
  beforeAll(() => {
    expect(example_usage_01).not.toBeUndefined();
    expect(example_usage_01.usage()).toBeNull();
    fullBridge.setup(example_usage_01.full_bridge);
    expect(example_usage_01.usage()).not.toBeNull();
  });

  test("example_remote_logging", async () => {
    let output = "";
    (await fullBridge.bridgeAsync).endemicObjectRegistry.logger.register(
      { name: "remote", variant: "persistent" },
      {
        info(message) {
          output = message;
        },
        error(message, labeledArgs?) {
          const exn = labeledArgs?.exn;
          if (exn === undefined) {
            output = message;
          } else {
            output = `[${exn}] ${message}`;
          }
        },
      }
    );
    example_usage_01.usage()?.example_remote_logging();
    expect(output).toStrictEqual("hello from Usage1");
  });

  test("example_stdout", () => {
    example_usage_01.usage()?.example_stdout();
    expect(new TextDecoder().decode(mockStdout.buf)).toStrictEqual("hello");
  });

  test("example_pass_endemic_object", () => {
    mockStdout.buf = new Uint8Array();
    example_usage_01.usage()?.example_pass_endemic_object();
    expect(new TextDecoder().decode(mockStdout.buf)).toStrictEqual("hello?");
  });

  test("example_file_write", () => {
    example_usage_01.usage()?.example_file_write();
    expect(new TextDecoder().decode(mockFiles["text.txt"].buf)).toStrictEqual("hello?");
  });
});
