/* eslint-disable @typescript-eslint/no-namespace */
import { objintf as bindoj } from "../../public-packages/runtime";
import { NonJsonValues } from "./utils";
export type MyString = string;
export type ExRecordStudent = { readonly admissionYear: number; readonly name: string };
export type Hello = (pargName: string) => void;
export type UnitSole = (__arg0: string) => string;
export type UnitObj = {
  readonly name: () => string;
  readonly unit01: (__arg0: string) => string;
  readonly unit02: (pargName: string) => string;
  readonly unit03: (__arg0: string) => string;
};
export type UnitMod = {
  readonly name: () => string;
  readonly unit01: (__arg0: string) => string;
  readonly unit02: (pargName: string) => string;
  readonly unit03: (__arg0: string) => string;
};
export type WithDefaultValue = {
  readonly getDefaultString: (labeledArgs?: { readonly largStr?: MyString }) => string;
  readonly getDefaultStudent: (labeledArgs?: { readonly largStudent?: ExRecordStudent }) => string;
};
export namespace GeneratedBridge {
  export type PeerFullBridgeReference = bindoj.PeerFullBridgeReference<ConcreteBridge>;
  export namespace Interfaces {
    export type SoleVar = () => string;
    export type RecObj = { readonly name: () => string; readonly getSelf: () => bindoj.endemic<RecObj> };
  }
  export type EndemicSetupParams = {
    readonly initiallyRegisteredObjects: {
      readonly string: [{ readonly cdnId0: string; readonly cdnId1: number }, string][];
      readonly hello: [{ readonly cdnId: string }, Hello][];
    };
    readonly endemicObjects: {
      readonly myString: string;
      readonly myHello: Hello;
      readonly mySoleVar: Interfaces.SoleVar;
      readonly myUnitSole: UnitSole;
      readonly myUnitObj: UnitObj;
      readonly myUnitMod: UnitMod;
      readonly myRecObj: Interfaces.RecObj;
      readonly myNonJsonValues: NonJsonValues;
      readonly withDefaultValue: WithDefaultValue;
    };
  };
  export type ConcreteBridge = {
    readonly endemicObjectRegistry: {
      readonly string: {
        readonly register: (coordinate: { readonly cdnId0: string; readonly cdnId1: number }, obj: string) => void;
        readonly deregister: (coordinate: { readonly cdnId0: string; readonly cdnId1: number }) => void;
      };
      readonly hello: {
        readonly register: (coordinate: { readonly cdnId: string }, obj: bindoj.endemic<Hello>) => void;
        readonly deregister: (coordinate: { readonly cdnId: string }) => void;
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
        endemicObjectRegistry: {
          string: bindoj.Internal.initEndemicObjectRegistry(initParams.initiallyRegisteredObjects.string),
          hello: bindoj.Internal.initEndemicObjectRegistry(initParams.initiallyRegisteredObjects.hello),
        },
        endemicObjects: initParams.endemicObjects,
      };
      return bridge as ConcreteBridge;
    }
  );
}
