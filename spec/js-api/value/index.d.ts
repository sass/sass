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

/** The JS API representation of the SassScript null singleton. */
export const sassNull: Value;

/**
 * The JS API representation of a Sass value.
 *
 * Sass values are immutable. Therefore, all subclasses of Value must have an
 * API that obeys immutability. Their APIs must not expose ways to modify
 * Sass values, including lists and maps. An API call that returns a new copy
 * of a Sass value must ensure that the copy preserves the metadata of the
 * original value (e.g. units).
 *
 * > To make the spec terser and easier to author, each subclass that extends
 * > `Value` has a virtual, private property named `internal` that refers to the
 * > Sass value it represents.
 */
export abstract class Value implements ValueObject {
  protected constructor();

  /**
   * Returns `this` as an array:
   *
   * - If `internal` is a Sass list, return an array of its contents.
   * - If `internal` is a Sass map, return an array of its keys and values as
   *   two-element `SassList`s.
   * - Otherwise, return an array containing `this`.
   */
  get asList(): List<Value>;

  /** Whether `internal` is a bracketed Sass list. */
  get hasBrackets(): boolean;

  /** Whether `this` is truthy. */
  get isTruthy(): boolean;

  /** Returns JS null if `internal` is Sass null. Otherwise, returns `this`. */
  get realNull(): null | Value;

  /**
   * Returns `internal`'s list separator:
   *
   * - If `internal` is a Sass list, return its separator.
   * - Otherwise, return `null`.
   */
  get separator(): ListSeparator;

  /**
   * Converts the Sass index `sassIndex` to a JS index into the array returned
   * by `asList`:
   *
   * - If `sassIndex` is not a unitless Sass number, throw an error.
   *
   * - Let `value` be the value of `sassIndex`. Let `index` be the result of
   *   `fuzzyAsInt(value)`. If `index === null`, throw an error.
   *
   * - If `index === 0`, or the absolute value of `index` is greater than
   *   `asList.length`, throw an error.
   *
   * - If `index > 0`, return `index - 1`.
   * - Otherwise, if `index < 0`, return `asList.length + index`.
   *
   *   > Sass indices start counting at 1, and may be negative in order to index
   *   > from the end of the list.
   *
   * > The `name` parameter may be used for error reporting.
   */
  sassIndexToListIndex(sassIndex: Value, name?: string): number;

  /**
   * - Return `this.asList.get(index)`.
   *
   * > Note that the `immutable` package uses zero-based indexing, with negative
   * > numbers indexing backwards from the end of the list. Non-integer indices
   * > are rounded down.
   */
  get(index: number): Value | undefined;

  /**
   * Asserts that `this` is a `SassBoolean`:
   *
   * - If `internal` is a Sass boolean, return `this`.
   * - Otherwise, throw an error.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertBoolean(name?: string): SassBoolean;

  /**
   * Asserts that `this` is a `SassColor`:
   *
   * - If `internal` is a Sass color, return `this`.
   * - Otherwise, throw an error.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertColor(name?: string): SassColor;

  /**
   * Asserts that `this` is a `SassFunction`:
   *
   * - If `internal` is a Sass function, return `this`.
   * - Otherwise, throw an error.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertFunction(name?: string): SassFunction;

  /**
   * Asserts that `this` is a `SassMap`:
   *
   * - If `internal` is a Sass map, return `this`.
   * - If `internal` is an empty Sass list, return a `SassMap` with `internal`
   *   set to an empty map.
   * - Otherwise, throw an error.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertMap(name?: string): SassMap;

  /**
   * Asserts that `this` is a `SassNumber`:
   *
   * - If `internal` is a Sass number, return `this`.
   * - Otherwise, throw an error.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertNumber(name?: string): SassNumber;

  /**
   * Asserts that `this` is a `SassString`:
   *
   * - If `internal` is a Sass string, return `this`.
   * - Otherwise, throw an error.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertString(name?: string): SassString;

  /**
   * Returns `this` interpreted as a map.
   *
   * - If `this` is a `SassMap`, return `this`.
   *
   * - Otherwise, if `internal` is an empty Sass list, return a `SassMap` with
   *   its `internal` set to an empty `OrderedMap`.
   *
   * - Otherwise, return `null`.
   */
  tryMap(): SassMap | null;

  /** Whether `this == other` in SassScript. */
  equals(other: Value): boolean;

  /**
   * Must be the same for `Value`s that are equal to each other according to the
   * `==` SassScript operator.
   */
  hashCode(): number;

  /**
   * Returns a serialized representation of `this`.
   *
   * > The specific format can vary from implementation to implementation and is
   * > not guaranteed to be valid Sass source code.
   */
  toString(): string;
}
