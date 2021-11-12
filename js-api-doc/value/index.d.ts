import {List, ValueObject} from 'immutable';

import {SassBoolean} from './boolean';
import {SassColor} from './color';
import {SassFunction} from './function';
import {ListSeparator} from './list';
import {SassMap} from './map';
import {SassNumber} from './number';
import {SassString} from './string';

export {SassArgumentList} from './argument_list';
export {SassBoolean, sassTrue, sassFalse} from './boolean';
export {SassColor} from './color';
export {SassFunction} from './function';
export {SassList, ListSeparator} from './list';
export {SassMap} from './map';
export {SassNumber} from './number';
export {SassString} from './string';

export const sassNull: Value;

export abstract class Value implements ValueObject {
  protected constructor();

  get asList(): List<Value>;

  get hasBrackets(): boolean;

  get isTruthy(): boolean;

  get realNull(): null | Value;

  get separator(): ListSeparator;

  sassIndexToListIndex(sassIndex: Value, name?: string): number;

  get(index: number): Value | undefined;

  assertBoolean(name?: string): SassBoolean;

  assertColor(name?: string): SassColor;

  assertFunction(name?: string): SassFunction;

  assertMap(name?: string): SassMap;

  assertNumber(name?: string): SassNumber;

  assertString(name?: string): SassString;

  tryMap(): SassMap | null;

  equals(other: Value): boolean;

  hashCode(): number;

  toString(): string;
}
