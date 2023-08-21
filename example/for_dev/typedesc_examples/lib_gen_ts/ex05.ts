export type ComplexTypes = {
  list: number[];
  map: Record<string, number>;
  nested: [number | null | undefined, number[], [number, number]];
  objtuple: { _0: number; _1: number };
  option: number | null | undefined;
  tuple: [number, number];
};
