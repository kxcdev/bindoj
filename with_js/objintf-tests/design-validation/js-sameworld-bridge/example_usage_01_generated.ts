import { objintf as bindoj } from "../../../public-packages/runtime";

// note that we don't support bridge markers in TypeScript
// omitting most 'export' keywords for conciseness

// Bindoj built-ins datatypes
// type bytes = Uint8Array;

// Bindoj objintf bridge support library
// type __BINDOJ_OBJINTF_sameworld_caml_peer<intf> = intf /* .. */;
// type __BINDOJ_OBJINTF_sameworld_caml_endemic<intf> = intf /* .. */;

// type PeerFullBridgeReference = unknown;

// mock-up utils / alias


// generated for the specific objintf definition

/// interface types for "simple" bridgeables

export interface Logger {
  info(message: string): void;
  // design note: we transform sole return value of unit to void

  error(
    message: string,
    labeledArgs?: {
      // design notes: all labelled arguments should be typed in this style
      exn?: string;
    }
  ): void;
}

export interface ByteSource {
  bytesLeft(): number;
  nextBlock(
    labeledArgs?: {
      max?: number;
    } /* if the last (non-labeled) argument is of a unit type, we omit it */
  ): bindoj.bytes;
}

export type ByteSource$PRIME$ = (
  labeledArgs?: {
    max?: number;
  } /* if the last (non-labeled) argument is of a unit type, we omit it */
) =>
  | {
      kind: "data";
      value: [
        /* base64 encoded bytes */ string,
        "eof" | "maybe-more" // string-enum
      ];
    }
  | {
      kind: "wait";
    }
  | {
      kind: "eof";
    };

// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace Generated_bridge {
  export type PeerFullBridgeReference = bindoj.PeerFullBridgeReference<ConcreteBridge>;
  // eslint-disable-next-line @typescript-eslint/no-namespace
  export namespace Interfaces {
    /// interface types for "complex" bridgeables

    export interface OutputChannel {
      channelName(): string;
      write(contents: bindoj.bytes): void;
      writeBulk(src: bindoj.peer<ByteSource>): void;
      writeAsync(src: bindoj.peer<ByteSource$PRIME$>): void;
    }

    export interface SystemIo {
      readonly stdout: bindoj.endemic<OutputChannel>;
      readonly stderr: bindoj.endemic<OutputChannel>;
      openFileWo(labeledArgs: { path: string }): bindoj.endemic<OutputChannel>;
      openFileRo(labeledArgs: { path: string }): bindoj.endemic<ByteSource>;
    }
  } // END: namespace Interfaces

  export interface ConcreteBridge {
    // // access bridged peer object
    // access<intf>(peerObj: bindoj.peer<intf>): intf;

    // // wrap endemic object for bridging
    // bridge<T>(obj: T): bindoj.endemic<T>;
    // // bridge(objType: "Logger", obj: Logger): endemic<Logger>;
    // // bridge(objType: "ByteSource", obj: ByteSource): endemic<ByteSource>;
    // // bridge(objType: "ByteSource$PRIME$", obj: ByteSource$PRIME$): endemic<ByteSource$PRIME$>;

    readonly peerObjectRegistry: {
      lookupLogger(coordinate: { id: string }): bindoj.peer<Logger> | null;
    };

    readonly peerObjects: {
      readonly myLogger: bindoj.peer<Logger>;
    };

    readonly endemicObjectRegistry: {
      readonly logger: {
        register(
          coordinate: {
            readonly name: string;
            readonly variant: "persistent" | "transient";
          },
          obj: bindoj.endemic<Logger>
        ): void;
        deregister(coordinate: { readonly name: string; readonly variant: "persistent" | "transient" }): void;
      };
    };
  }

  type FullBridge_setupless = ConcreteBridge;
  type FullBridge_endemicSetupOnly = (initParams: EndemicSetupParams) => {
    readonly bridge: ConcreteBridge;
    readonly setup: (peerFullBridge: bindoj.PeerFullBridgeReference<ConcreteBridge>) => void;
  };
  type FullBridge_peerSetupOnly = {
    readonly bridgeAsync: Promise<ConcreteBridge>;
    readonly getBridge: () => ConcreteBridge | null;
    readonly setup: (peerFullBridge: bindoj.PeerFullBridgeReference<ConcreteBridge>) => void;
  };
  type FullBridge_DualSetup = (initParams: EndemicSetupParams) => {
    readonly bridgeAsync: Promise<ConcreteBridge>;
    readonly getBridge: () => ConcreteBridge | null;
    readonly setup: (peerFullBridge: bindoj.PeerFullBridgeReference<ConcreteBridge>) => void;
  };

  export type EndemicSetupParams = {
    // design note: endemic objects not wrapped here as it will be impossible

    readonly initiallyRegisteredObjects: {
      readonly logger: [
        {
          name: string;
          variant: "persistent" | "transient";
        },
        Logger
      ][];
    };
    readonly endemicObjects: {
      readonly systemIo: Interfaces.SystemIo;
    };
  };

  /// bridge internals

  export type __BINDOJ_OBJINTF_bridge_state = {
    readonly peer_bridge_object: bindoj.tbd;
    readonly registries: {
      readonly logger: Record<string, bindoj.endemic<Logger>>;
    };
  };

  export const FullBridge: FullBridge_DualSetup =
    bindoj.Internal.createFullBridgeDualSetupImpl((initParams) => {
      const state: __BINDOJ_OBJINTF_bridge_state = {
        // TODO
        peer_bridge_object: undefined,
        registries: {
          logger: Object.assign({}, ...Array.from(initParams.initiallyRegisteredObjects.logger, ([k, v]) => ({[k.name]: v}) )),
        },
      };
      const bridge = {
        peerObjectRegistry: {
          // setup by peer
          lookupLogger: () => {
            throw "This bridge has not been set up.";
          },
        },
        peerObjects: {
          // setup by peer
          myLogger: undefined as unknown as bindoj.peer<Logger>,
        },
        endemicObjectRegistry: {
          // will be overwritten by peer setup.
          logger: bindoj.Internal.initEndemicObjectRegistry(initParams.initiallyRegisteredObjects.logger),
        },
        endemicObjects: initParams.endemicObjects,
      };

      for (const [k, v] of initParams.initiallyRegisteredObjects.logger) {
        bridge.endemicObjectRegistry.logger.register(k, v);
      }

      return bridge as ConcreteBridge;
    });
} // END: namespace Generated_bridge
