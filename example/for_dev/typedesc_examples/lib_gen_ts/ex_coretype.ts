type json_value = any;
export type ExCoretypeVariousPrimTypes = {
  bool: boolean;
  byte: number;
  bytes: string;
  float: number;
  int: number;
  string: string;
  uchar: string;
  unit: 1;
};

export type ExCoretypeWithInt53p = { value: number };

export type ExCoretypeVariousComplexTypes = { list: number[]; map: Record<string, number>; option?: number };

export type ExCoretypeVariousTupleTypes = {
  nested: [number | null | undefined, number[], [number, number]];
  objtuple: { _0: number; _1: number };
  tuple: [number, number];
};

export type ExCoretypeNamedJson = { json: json_value; name: string };
