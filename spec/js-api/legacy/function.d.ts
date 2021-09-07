import {PluginThis} from './plugin_this';

type _SyncFunction = (this: PluginThis, ...args: Value[]) => Value;

type _AsyncFunction = (
  this: PluginThis,
  ...args: [...Value[], (type: Value) => void]
) => void;

export type CustomFunction<sync = 'sync' | 'async'> =
  | _SyncFunction
  | (sync extends 'async' ? _AsyncFunction : never);

export type Value =
  | types.Null
  | types.Number
  | types.String
  | types.Boolean
  | types.HslColor
  | types.RgbColor
  | types.List
  | types.Map;

export namespace types {
  export class Null {
    static NULL: Null;
  }

  export class Number {
    constructor(value: number, unit?: string);
    getValue(): number;
    setValue(value: number): void;
    getUnit(): string;
    setUnit(unit: string): void;
  }

  export class String {
    constructor(value: string);
    getValue(): string;
    setValue(value: string): void;
  }

  export class Boolean<T extends boolean = boolean> {
    constructor(value: T);
    getValue(): T;
    static readonly TRUE: Boolean<true>;
    static readonly FALSE: Boolean<false>;
  }

  export class HslColor {
    constructor(h: number, s: number, l: number, a?: number);
    getH(): number;
    setH(value: number): void;
    getS(): number;
    setS(value: number): void;
    getL(): number;
    setL(value: number): void;
    getA(): number;
    setA(value: number): void;
  }

  export class RgbColor {
    constructor(r: number, g: number, b: number, a?: number);
    getR(): number;
    setR(value: number): void;
    getG(): number;
    setG(value: number): void;
    getB(): number;
    setB(value: number): void;
    getA(): number;
    setA(value: number): void;
  }

  export class List {
    constructor(length: number, commaSeparator?: boolean);
    getValue(index: number): Value | undefined;
    setValue(index: number, value: Value): void;
    getSeparator(): boolean;
    setSeparator(isComma: boolean): void;
    getLength(): number;
  }

  export class Map {
    constructor(length: number);
    getValue(index: number): Value;
    setValue(index: number, value: Value): void;
    getKey(index: number): Value;
    setKey(index: number, key: Value): void;
    getLength(): number;
  }
}
