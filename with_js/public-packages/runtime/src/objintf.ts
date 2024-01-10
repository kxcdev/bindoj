export type bytes = Uint8Array;

export function base64StringToBytes(base64data: string): bytes {
  const binaryString = atob(base64data);
  const bytes = new Uint8Array(binaryString.length);
  for (let i = 0; i < binaryString.length; i++) {
    bytes[i] = binaryString.charCodeAt(i);
  }
  return bytes;
}

export function bytesToBase64String(data: bytes): string {
  let str = "";
  for (let i = 0; i < data.length; i++) {
    str += String.fromCharCode(data[i]);
  }
  return btoa(str);
}

export type tbd = any;
export type peer<intf> = intf;
export type endemic<intf> = intf;

export function access<intf>(peerObj: peer<intf>): intf {
  return peerObj;
}

export function bridge<T>(obj: T): endemic<T> {
  return obj;
}

export type PeerFullBridgeReference<ConcreteBridge> = {
  readonly setupCalled: () => boolean;
  readonly setup: (fullBridge: { readonly instance: ConcreteBridge }) => void;
};

export class FullBridgeAlreadySetupError extends Error {
  static {
    this.prototype.name = "FullBridgeAlreadySetupError";
  }

  constructor() {
    super("This full bridge has already been set up.");
  }
}

export namespace Internal {
  function createSetup<ConcreteBridge>(
    fullBridge: {
      setupCalled: boolean;
      readonly instance: ConcreteBridge;
    },
    resolve?: (value: ConcreteBridge) => void
  ) {
    return (peerFullBridge: PeerFullBridgeReference<ConcreteBridge>) => {
      if (fullBridge.setupCalled) {
        throw new FullBridgeAlreadySetupError();
      }

      fullBridge.setupCalled = true;

      if (!peerFullBridge.setupCalled()) {
        peerFullBridge.setup(fullBridge);
      }

      if (resolve != null) {
        resolve(fullBridge.instance);
      }
    };
  }

  export function initEndemicObjectRegistry<Coordinate, T>(initiallyRegisteredObjects: [Coordinate, T][]) {
    const initialObjects :
      ({ kind: 'register'; coordinate: Coordinate; value: T; }
        | { kind: 'deregister'; coordinate: Coordinate })[] =
      initiallyRegisteredObjects.map(([ coordinate, value ]) => ({ kind: 'register', coordinate, value }));
    return {
      initialObjects,
      register: (coordinate: Coordinate, value: T) => {
        initialObjects.push({ kind: 'register', coordinate, value });
      },
      deregister: (coordinate: Coordinate) => {
        initialObjects.push({ kind: 'deregister', coordinate });
      },
    };
  }

  export function createFullBridgePeerSetupOnlyImpl<ConcreteBridge>(initConcreteBridge: () => ConcreteBridge) {
    let resolved = false;
    let resolve: (value: ConcreteBridge) => void;
    const bridgeAsync = new Promise<ConcreteBridge>((res) => {
      resolve = res;
      resolved = true;
    });

    const fullBridge = {
      setupCalled: false,
      instance: initConcreteBridge(),
      bridgeAsync,
      getBridge: () => resolved ? fullBridge.instance : null,
      setup: (peerFullBridge: PeerFullBridgeReference<ConcreteBridge>) => createSetup(fullBridge, resolve)(peerFullBridge),
    };

    return fullBridge;
  }

  export function createFullBridgeEndemicSetupOnlyImpl<Param, ConcreteBridge>(
    initConcreteBridge: (initParams: Param) => ConcreteBridge
  ) {
    return (initParams: Param) => {
      const instance = initConcreteBridge(initParams);
      const fullBridge = {
        setupCalled: false,
        instance,
        bridge: instance,
        setup: (peerFullBridge: PeerFullBridgeReference<ConcreteBridge>) => createSetup(fullBridge)(peerFullBridge),
      };

      return fullBridge;
    };
  }

  export function createFullBridgeDualSetupImpl<Param, ConcreteBridge>(
    initConcreteBridge: (initParams: Param) => ConcreteBridge
  ) {
    return (initParams: Param) => {
      let resolved = false;
      let resolve: (value: ConcreteBridge) => void;
      const bridgeAsync = new Promise<ConcreteBridge>((res) => {
        resolve = res;
        resolved = true;
      });
      const fullBridge = {
        setupCalled: false,
        instance: initConcreteBridge(initParams),
        bridgeAsync,
        getBridge: () => resolved ? fullBridge.instance : null,
        setup: (peerFullBridge: PeerFullBridgeReference<ConcreteBridge>) => createSetup(fullBridge, resolve)(peerFullBridge),
      };

      return fullBridge;
    };
  }
}
