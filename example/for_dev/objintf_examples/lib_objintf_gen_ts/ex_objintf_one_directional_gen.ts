/* eslint-disable @typescript-eslint/no-namespace */
import { objintf as bindoj } from "../../public-packages/runtime";
import { NonJsonValues } from "./utils";
export type ExRecordStudent = { readonly admissionYear: number; readonly name: string };
export type MyString = string;
export type Hello = (name: string) => void;
export type UnitSole = (__arg0: string) => string;
export type UnitObj = {
  readonly name: () => string;
  readonly unit01: (__arg0: string) => string;
  readonly unit02: (__arg0: string) => string;
  readonly unit03: (__arg0: string) => string;
};
export type UnitMod = {
  readonly name: () => string;
  readonly unit01: (__arg0: string) => string;
  readonly unit02: (__arg0: string) => string;
  readonly unit03: (__arg0: string) => string;
};
export type WithDefaultValue = {
  readonly getDefaultString: (labeledArgs?: { readonly str?: MyString }) => string;
  readonly getDefaultStudent: (labeledArgs?: { readonly student?: ExRecordStudent }) => string;
};
export namespace GeneratedBridge {
  export type PeerFullBridgeReference = bindoj.PeerFullBridgeReference<ConcreteBridge>;
  export namespace Interfaces {
    export type SoleVar = () => string;
    export type RecObj = { readonly name: () => string; readonly getSelf: () => bindoj.peer<RecObj> };
  }
  export type ConcreteBridge = {
    readonly peerObjectRegistry: {
      readonly lookupString: (coordinate: { readonly id0: string; readonly id1: number }) => string | null;
      readonly lookupHello: (coordinate: { readonly id: string }) => bindoj.peer<Hello> | null;
    };
    readonly peerObjects: {
      readonly myString: string;
      readonly myHello: bindoj.peer<Hello>;
      readonly mySoleVar: bindoj.peer<Interfaces.SoleVar>;
      readonly myUnitSole: bindoj.peer<UnitSole>;
      readonly myUnitObj: bindoj.peer<UnitObj>;
      readonly myUnitMod: bindoj.peer<UnitMod>;
      readonly myRecObj: bindoj.peer<Interfaces.RecObj>;
      readonly myNonJsonValues: NonJsonValues;
      readonly withDefaultValue: bindoj.peer<WithDefaultValue>;
    };
  };
  export type FullBridge_peerSetupOnly = {
    readonly bridgeAsync: Promise<ConcreteBridge>;
    readonly getBridge: () => ConcreteBridge | null;
    readonly setup: (peerFullBridge: bindoj.PeerFullBridgeReference<ConcreteBridge>) => void;
  };
  export const FullBridge: FullBridge_peerSetupOnly = bindoj.Internal.createFullBridgePeerSetupOnlyImpl(() => {
    const bridge = {
      peerObjectRegistry: {
        lookupString: () => {
          throw "This bridge has not been set up.";
        },
        lookupHello: () => {
          throw "This bridge has not been set up.";
        },
      },
      peerObjects: {
        myString: undefined as unknown as string,
        myHello: undefined as unknown as bindoj.peer<Hello>,
        mySoleVar: undefined as unknown as bindoj.peer<Interfaces.SoleVar>,
        myUnitSole: undefined as unknown as bindoj.peer<UnitSole>,
        myUnitObj: undefined as unknown as bindoj.peer<UnitObj>,
        myUnitMod: undefined as unknown as bindoj.peer<UnitMod>,
        myRecObj: undefined as unknown as bindoj.peer<Interfaces.RecObj>,
        myNonJsonValues: undefined as unknown as NonJsonValues,
        withDefaultValue: undefined as unknown as bindoj.peer<WithDefaultValue>,
      },
    };
    return bridge as ConcreteBridge;
  });
}
