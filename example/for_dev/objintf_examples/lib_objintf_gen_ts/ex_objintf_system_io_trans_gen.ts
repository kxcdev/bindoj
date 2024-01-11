/* eslint-disable @typescript-eslint/no-namespace */
import { objintf as bindoj } from "../../public-packages/runtime";
export type ByteSource$PRIME$ = (labeledArgs?: {
  readonly max?: number;
}) => { kind: "data"; readonly value: [string, "eof" | "maybe-more"] } | { kind: "eof" } | { kind: "wait" };
export type Logger = {
  readonly info: (__arg0: string) => void;
  readonly error: (__arg0: string, labeledArgs?: { readonly exn?: string }) => void;
};
export type ByteSource = {
  readonly bytesLeft: () => number;
  readonly nextBlock: (labeledArgs?: { readonly max?: number }) => bindoj.bytes;
};
export namespace GeneratedBridge {
  export type PeerFullBridgeReference = bindoj.PeerFullBridgeReference<ConcreteBridge>;
  export namespace Interfaces {
    export type OutputChannel = {
      readonly channelName: () => string;
      readonly write: (__arg0: bindoj.bytes) => void;
      readonly writeBulk: (__arg0: bindoj.peer<ByteSource>) => void;
      readonly writeAsync: (__arg0: bindoj.peer<ByteSource$PRIME$>) => void;
    };
    export type SystemIo = {
      readonly stdout: () => bindoj.endemic<Interfaces.OutputChannel>;
      readonly stderr: () => bindoj.endemic<Interfaces.OutputChannel>;
      readonly openFileWo: (labeledArgs: { readonly path: string }) => bindoj.endemic<Interfaces.OutputChannel>;
      readonly openFileRo: (labeledArgs: { readonly path: string }) => bindoj.endemic<ByteSource>;
    };
  }
  export type EndemicSetupParams = {
    readonly initiallyRegisteredObjects: {
      readonly logger: [{ readonly name: string; readonly variant: "persistent" | "transient" }, Logger][];
    };
    readonly endemicObjects: { readonly systemIo: Interfaces.SystemIo };
  };
  export type ConcreteBridge = {
    readonly peerObjectRegistry: {
      readonly lookupLogger: (coordinate: { readonly id: string }) => bindoj.peer<Logger> | null;
    };
    readonly peerObjects: { readonly myLogger: bindoj.peer<Logger> };
    readonly endemicObjectRegistry: {
      readonly logger: {
        readonly register: (
          coordinate: { readonly name: string; readonly variant: "persistent" | "transient" },
          obj: bindoj.endemic<Logger>
        ) => void;
        readonly deregister: (coordinate: {
          readonly name: string;
          readonly variant: "persistent" | "transient";
        }) => void;
      };
    };
  };
  export type FullBridge_dualSetup = (initParams: EndemicSetupParams) => {
    readonly bridgeAsync: Promise<ConcreteBridge>;
    readonly getBridge: () => ConcreteBridge | null;
    readonly setup: (peerFullBridge: bindoj.PeerFullBridgeReference<ConcreteBridge>) => void;
  };
  export const FullBridge: FullBridge_dualSetup = bindoj.Internal.createFullBridgeDualSetupImpl(
    (initParams: EndemicSetupParams) => {
      const bridge = {
        peerObjectRegistry: {
          lookupLogger: () => {
            throw "This bridge has not been set up.";
          },
        },
        peerObjects: {
          myLogger: undefined as unknown as bindoj.peer<Logger>,
        },
        endemicObjectRegistry: {
          logger: bindoj.Internal.initEndemicObjectRegistry(initParams.initiallyRegisteredObjects.logger),
        },
        endemicObjects: initParams.endemicObjects,
      };
      return bridge as ConcreteBridge;
    }
  );
}
