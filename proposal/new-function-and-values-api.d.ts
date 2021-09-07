/**
 * # New Function and Values API: Draft 3.1
 *
 * *([Issue](https://github.com/sass/sass/issues/2510),
 * [Changelog](new-function-and-values-api.changes.md))*
 *
 * ## Background
 *
 * > This section is non-normative.
 *
 * Sass's current JavaScript API was inherited from Node Sass, which developed
 * over time in an _ad hoc_ manner. The functions API in particular has a number
 * of notable issues:
 *
 * - Values are mutable. This goes against the grain of how Sass values work,
 *   and makes it difficult to implement the API efficiently in Dart Sass.
 *
 * - It's hard to verify argument types with assertions.
 *
 * - It's hard to follow Sass's conventions for dealing with values, such as:
 *   - Treating `false` and `null` as falsey and everything else as truthy.
 *   - Treating all values as lists, and treating maps as lists of pairs.
 *   - Treating empty lists and empty maps as the same value.
 *   - Using 1-based indexes for lists and strings, and negative indexes to
 *     count from the back.
 *   - Indexing the Unicode [code point]s of strings rather than UTF-16
 *     [code unit]s.
 *
 *     [code point]: https://unicode.org/glossary/#code_point
 *     [code unit]: https://unicode.org/glossary/#code_unit
 *
 * - Sass values are not represented properly:
 *   - First-class function values do not exist.
 *   - Argument list values may contain keyword arguments that do not exist.
 *   - Maps are represented as a list of pairs, without any way of accessing a
 *     value using its key other than iterating through the entire list.
 *   - Numbers represent their units as a single string in an undocumented
 *     format, making it very difficult to work with them if they have more than
 *     just a single numerator unit.
 *
 * - The `this` context includes a bunch of undocumented information without a
 *   clear use-case.
 *
 * ## Summary
 *
 * This proposal solves the problems with the old API. It is heavily based on
 * Dart Sass's [Dart `Value` API].
 *
 * [Dart `Value` API]: https://pub.dev/documentation/sass/latest/sass/Value-class.html
 *
 * ### Proper JS representation of Sass values
 *
 * The new API exposes the following values:
 * - Singletons:
 *   - `sassNull`
 *   - `sassTrue`
 *   - `sassFalse`
 * - Types:
 *   - `SassBoolean`
 * - Classes:
 *   - `SassColor`
 *   - `SassFunction`
 *   - `SassList`
 *   - `ArgumentList`
 *   - `SassMap`
 *   - `SassNumber`
 *   - `SassString`
 *
 * These values all inherit from the `Value` abstract class, which provides a
 * suite of `assert*()` functions to facilitate type checking.
 *
 * These values are tightly translated from Sass values to idiomatic JS. For
 * example:
 * - Since a Sass map allows objects as keys and preserves insert order,
 *   `SassMap` represents its contents as an `OrderedMap`.
 * - To facilitate unit conversion, `SassNumber` tracks its numerator and
 *   denominator units as string arrays, and exposes unit conversion methods.
 * - `SassColor` properly represents `rgb`, `hwb`, and `hsl` formats.
 *
 * ### Proper handling of Sass conventions
 *
 * Values properly encode the meaningful differences between Sass and JS
 * conventions. For example:
 * - All values...
 *   - expose an `isTruthy()` method
 *   - expose an `equals()` method that follows the behavior of the SassScript
 *     `==` operator
 *   - can be handled as lists, including maps
 *   - support 1-based indexes and negative indexes to count from the back
 * - Information about list delimiters and brackets are exposed.
 * - Strings index the Unicode codepoints of strings rather than UTF-16 code
 *   units.
 *
 * ### No special handling of `this`
 *
 * The `this` context is left untouched. It does not get bound or modified. This
 * avoids hidden, unexpected behavior.
 *
 * ### Immutability
 *
 * Values only expose their data through getters. All collections returned by
 * getters are immutable (e.g. `SassMap.contents`).
 */

/** API */

import {List, OrderedMap, ValueObject} from 'immutable';

import './new-js-api';

type CustomFunctionCallback<sync extends 'sync' | 'async'> = sync extends 'sync'
  ? (args: Value[]) => Value
  : (args: Value[]) => Value | Promise<Value>;

/**
 * This definition updates the one in the [New JavaScript API proposal].
 *
 * [New JavaScript API proposal]: ./new-js-api.d.ts
 */
declare module './new-js-api' {
  interface Options<sync extends 'sync' | 'async'> {
    /**
     * When the compiler encounters a global function call with a signature that
     * does not match that of a built-in function, but matches a key in this
     * map, it must call the associated `CustomFunctionCallback` and return its
     * result.
     *
     * The compiler must throw an error if the `CustomFunctionCallback` does not
     * return a `Value`.
     */
    functions?: Record<string, CustomFunctionCallback<sync>>;
  }
}

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
   * Returns `this`'s map contents, if it can be interpreted as a map.
   *
   * - If `internal` is a Sass map:
   *   - Let `result` be an empty `OrderedMap`.
   *   - Add each key and value from `internal`'s contents to `result`, in
   *     order.
   *   - Return `result`.
   *
   * - Otherwise, if `internal` is an empty Sass list, return an empty
   *   `OrderedMap`.
   *
   * - Otherwise, return `null`.
   */
  tryMap(): OrderedMap<Value, Value> | null;

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

/** The JS API representation of the SassScript null singleton. */
export const sassNull: Value;

/** The JS API representation of the SassScript true singleton. */
export const sassTrue: SassBoolean;

/** The JS API representation of the SassScript false singleton. */
export const sassFalse: SassBoolean;

/** The JS API representation of a Sass boolean. */
export interface SassBoolean extends Value {
  get value(): boolean;
}

/**
 * The JS API representation of a Sass color.
 *
 * `internal` refers to a Sass color.
 */
export class SassColor extends Value {
  /**
   * Creates an RGB color:
   *
   * - Let `sassRed` be a Sass number with a value of `red` `fuzzyRound`ed to
   *   the nearest integer.
   *
   * - Let `sassGreen` be a Sass number with a value of `green` `fuzzyRound`ed
   *   to the nearest integer
   *
   * - Let `sassBlue` be a Sass number with a value of `blue` `fuzzyRound`ed to
   *   the nearest integer.
   *
   * - If `alpha` was passed, let `sassAlpha` be a Sass number with a value of
   *   `alpha`.
   *
   * - Set `internal` to the result of running [`rgb()`] with the following
   *   inputs:
   *   - `$red` set to `sassRed`
   *   - `$green` set to `sassGreen`
   *   - `$blue` set to `sassBlue`
   *   - If `alpha` was passed, `$alpha` set to `sassAlpha`
   *
   *   [`rgb()`]: ../spec/functions.md#rgb-and-rgba
   *
   * - Return `this`.
   */
  static rgb(
    red: number,
    green: number,
    blue: number,
    alpha?: number
  ): SassColor;

  /**
   * Creates an HSL color:
   *
   * - Let `sassHue` be a Sass number with a value of `hue`.
   *
   * - Let `sassSaturation` be a Sass number with a value of `saturation`.
   *
   * - Let `sassLightness` be a Sass number with a value of `lightness`.
   *
   * - If `alpha` was passed, let `sassAlpha` be a Sass number with a value of
   *   `alpha`.
   *
   * - Set `internal` to the result of running [`hsl()`] with the following
   *   inputs:
   *   - `$hue` set to `sassHue`
   *   - `$saturation` set to `sassSaturation`
   *   - `$lightness` set to `sassLightness`
   *   - If `alpha` was passed, `$alpha` set to `sassAlpha`
   *
   *   [`hsl()`]: ../spec/functions.md#hsl-and-hsla
   *
   * - Return `this`.
   */
  static hsl(
    hue: number,
    saturation: number,
    lightness: number,
    alpha?: number
  ): SassColor;

  /**
   * Creates an HWB color:
   *
   * - Let `sassHue` be a Sass number with a value of `hue`.
   *
   * - Let `sassWhiteness` be a Sass number with a value of `whiteness`.
   *
   * - Let `sassBlackness` be a Sass number with a value of `blackness`.
   *
   * - If `alpha` was passed, let `sassAlpha` be a Sass number with a value of
   *   `alpha`.
   *
   * - Set `internal` to the result of running [`hwb()`] with the following
   *   inputs:
   *   - `$hue` set to `sassHue`
   *   - `$whiteness` set to `sassWhiteness`
   *   - `$blackness` set to `sassBlackness`
   *   - If `alpha` was passed, `$alpha` set to `sassAlpha`
   *
   *   [`hwb()`]: ../spec/color.md#hwb
   *
   * - Return `this`.
   */
  static hwb(
    hue: number,
    whiteness: number,
    blackness: number,
    alpha?: number
  ): SassColor;

  /** `internal`'s red channel. */
  get red(): number;

  /** `internal`'s green channel. */
  get green(): number;

  /** `internal`'s blue channel. */
  get blue(): number;

  /**
   * Returns the value of the result of [`hue(internal)`][hue].
   *
   * [hue]: ../spec/built-in-modules/color.md#hue
   */
  get hue(): number;

  /**
   * Returns the value of the result of [`saturation(internal)`][saturation].
   *
   * [saturation]: ../spec/built-in-modules/color.md#saturation
   */
  get saturation(): number;

  /**
   * Returns the value of the result of [`lightness(internal)`][lightness].
   *
   * [lightness]: ../spec/built-in-modules/color.md#lightness
   */
  get lightness(): number;

  /**
   * Returns the value of the result of [`whiteness(internal)`][whiteness].
   *
   * [whiteness]: ../spec/built-in-modules/color.md#whiteness
   */
  get whiteness(): number;

  /**
   * Returns the value of the result of [`blackness(internal)`][blackness].
   *
   * [blackness]: ../spec/built-in-modules/color.md#blackness
   */
  get blackness(): number;

  /**
   * Returns the value of the result of [`alpha(internal)`][alpha].
   *
   * [alpha]: ../spec/built-in-modules/color.md#alpha
   */
  get alpha(): number;

  /**
   * Returns a new copy of `this` with one or more changes made to the RGB
   * channels:
   *
   * - Let `oldColor` be `this`.
   *
   * - If `red` was passed, let `newRed = red`.
   * - Otherwise, let `newRed = oldColor.red`.
   *
   * - If `green` was passed, let `newGreen = green`.
   * - Otherwise, let `newGreen = oldColor.green`.
   *
   * - If `blue` was passed, let `newBlue = blue`.
   * - Otherwise, let `newBlue = oldColor.blue`.
   *
   * - If `alpha` was passed, let `newAlpha = alpha`.
   * - Otherwise, let `newAlpha = oldColor.alpha`.
   *
   * - Return the result of
   *   `SassColor.rgb(newRed, newGreen, newBlue, newAlpha)`.
   */
  changeRgb(options: {
    red?: number;
    green?: number;
    blue?: number;
    alpha?: number;
  }): SassColor;

  /**
   * Returns a new copy of `this` with one or more changes made to the HSL
   * values:
   *
   * - Let `oldColor` be `this`.
   *
   * - If `hue` was passed, let `newHue = hue`.
   * - Otherwise, let `newHue = oldColor.hue`.
   *
   * - If `saturation` was passed, let `newSaturation = saturation`.
   * - Otherwise, let `newSaturation = oldColor.saturation`.
   *
   * - If `lightness` was passed, let `newLightness = lightness`.
   * - Otherwise, let `newLightness = oldColor.lightness`.
   *
   * - If `alpha` was passed, let `newAlpha = alpha`.
   * - Otherwise, let `newAlpha = oldColor.alpha`.
   *
   * - Return the result of
   *   `SassColor.hsl(newHue, newSaturation, newLightness, newAlpha)`.
   */
  changeHsl(options: {
    hue?: number;
    saturation?: number;
    lightness?: number;
    alpha?: number;
  }): SassColor;

  /**
   * Returns a new copy of `this` with one or more changes made to the HWB
   * values:
   *
   * - Let `oldColor` be `this`.
   *
   * - If `hue` was passed, let `newHue = hue`.
   * - Otherwise, let `newHue = oldColor.hue`.
   *
   * - If `whiteness` was passed, let `newWhiteness = whiteness`.
   * - Otherwise, let `newWhiteness = oldColor.whiteness`.
   *
   * - If `blackness` was passed, let `newBlackness = blackness`.
   * - Otherwise, let `newBlackness = oldColor.blackness`.
   *
   * - If `alpha` was passed, let `newAlpha = alpha`.
   * - Otherwise, let `newAlpha = oldColor.alpha`.
   *
   * - Return the result of
   *   `SassColor.hwb(newHue, newWhiteness, newBlackness, newAlpha)`.
   */
  changeHwb(options: {
    hue?: number;
    whiteness?: number;
    blackness?: number;
    alpha?: number;
  }): SassColor;

  /**
   * Returns a new copy of `this` with `internal`'s alpha channel set to
   * `alpha`.
   */
  changeAlpha(alpha: number): SassColor;
}

/**
 * The JS API representation of a Sass function.
 *
 * `internal` refers to a Sass function.
 */
export class SassFunction extends Value {
  /**
   * Creates a Sass function:
   *
   * - Set `internal` to a Sass function with signature set to `signature` that,
   *   upon execution, runs `callback` and returns the result.
   * - Return `this`.
   */
  constructor(
    /**
     * Must be a valid Sass function signature that could appear after the
     * `@function` directive in a Sass stylesheet, such as
     * `mix($color1, $color2, $weight: 50%)`.
     */
    signature: string,
    callback: CustomFunctionCallback<'sync'>
  );
}

/**
 * The JS API representation of a Sass list separator.
 *
 * > `null` represents the undecided separator type.
 */
export type ListSeparator = ',' | '/' | ' ' | null;

/**
 * The JS API representation of a Sass list.
 *
 * `internal` refers to a Sass list.
 */
export class SassList extends Value {
  /**
   * Creates a Sass list:
   *
   * - Set `internal` to a Sass list with contents set to `contents`, separator
   *   set to `options.separator`, and brackets set to `options.brackets`.
   * - Return `this`.
   */
  constructor(
    contents: Value[] | List<Value>,
    options?: {
      /** @default ',' */
      separator?: ListSeparator;
      /** @default false */
      brackets?: boolean;
    }
  );

  /**
   * Creates an empty Sass list:
   *
   * - Set `internal` to an empty Sass list with separator set to
   *   `options.separator` and brackets set to `options.brackets`.
   * - Return `this`.
   */
  static empty(options?: {
    /** @default null */
    separator?: ListSeparator;
    /** @default false */
    brackets?: boolean;
  }): SassList;

  /** `internal`'s list separator. */
  get separator(): ListSeparator;
}

/**
 * The JS API representation of a Sass argument list.
 *
 * `internal` refers to a Sass argument list.
 */
export class ArgumentList extends SassList {
  /**
   * Creates a Sass argument list:
   *
   * - Set `internal` to a Sass argument list with contents set to `contents`,
   *   keywords set to `keywords`, and list separator set to `separator`.
   * - Return `this`.
   */
  constructor(
    contents: Value[] | List<Value>,
    keywords: Record<string, Value> | OrderedMap<string, Value>,
    /** @default ',' */
    separator?: ListSeparator
  );

  /** `internal`'s keywords. */
  get keywords(): OrderedMap<string, Value>;
}

/**
 * The JS API representation of a Sass map.
 *
 * `internal` refers to a Sass map.
 */
export class SassMap extends Value {
  /**
   * Creates a Sass map:
   *
   * - Set `internal` to a Sass map with contents set to `contents`.
   * - Return `this`.
   */
  constructor(contents: OrderedMap<Value, Value>);

  /**
   * Creates an empty Sass map:
   *
   * - Set `internal` to an empty Sass map.
   * - Return `this`.
   */
  static empty(): SassMap;

  /**
   * Returns a map containing `internal`'s contents:
   *
   * - Let `result` be an empty `OrderedMap`.
   * - Add each key and value from `internal`'s contents to `result`, in order.
   * - Return `result`.
   */
  get contents(): OrderedMap<Value, Value>;

  /** Returns `this.contents`. */
  tryMap(): OrderedMap<Value, Value>;
}

/**
 * The JS API representation of a Sass number.
 *
 * `internal` refers to a Sass number.
 */
export class SassNumber extends Value {
  /**
   * Creates a Sass number:
   *
   * - Set `internal` to a Sass number with value set to `value` and a single
   *   numerator unit equal to `unit` (if passed).
   * - Return `this`.
   */
  constructor(value: number, unit?: string);

  /**
   * Creates a Sass number:
   *
   * - Set `internal` to a Sass number with value set to `value`, numerator
   *   units set to `options.numeratorUnits` (if passed), and denominator units
   *   set to `options.denominatorUnits` (if passed).
   * - Set `internal` to the result of `simplify`ing `internal`.
   * - Return `this`.
   */
  static withUnits(
    value: number,
    options?: {
      numeratorUnits?: string[] | List<string>;
      denominatorUnits?: string[] | List<string>;
    }
  ): SassNumber;

  /** `internal`'s value. */
  get value(): number;

  /** Whether `internal`'s value `fuzzyEquals` an integer. */
  get isInt(): boolean;

  /**
   * Returns `internal`'s value as an integer:
   *
   * - If `internal`'s value `fuzzyEquals` an integer, return that integer.
   * - Otherwise, return `null`.
   */
  get asInt(): number | null;

  /** `internal`'s numerator units. */
  get numeratorUnits(): List<string>;

  /** `internal`'s denominator units. */
  get denominatorUnits(): List<string>;

  /** Whether `internal` has numerator or denominator units. */
  get hasUnits(): boolean;

  /**
   * Asserts that `internal`'s value is an integer:
   *
   * - If `internal`'s value `fuzzyEquals` an integer, return that integer.
   * - Otherwise, throw an error.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertInt(name?: string): number;

  /**
   * Asserts that `internal`'s value is within the specified range:
   *
   * - If `internal`'s value is `fuzzyGreaterThan` `min` and `fuzzyLessThan`
   *   `max`, return it.
   * - Otherwise, if `internal`'s value `fuzzyEquals` `min`, return `min`.
   * - Otherwise, if `internal`'s value `fuzzyEquals` `max`, return `max`.
   * - Otherwise, throw an error.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertInRange(min: number, max: number, name?: string): number;

  /**
   * Asserts that `internal` is unitless:
   *
   * - If `internal` has any numerator or denominator units, throw an error.
   * - Otherwise, return `this`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertNoUnits(name?: string): SassNumber;

  /**
   * Asserts the type of `internal`'s unit:
   *
   * - If `internal` has any denominator units, or if `unit` is not `internal`'s
   *   only numerator unit, throw an error.
   * - Otherwise, return `this`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertUnit(unit: string, name?: string): SassNumber;

  /**
   * Whether `internal` has the specified unit:
   *
   * - If `internal` has any denominator units, return false.
   * - Otherwise, return whether `unit` is `internal`'s only numerator unit.
   */
  hasUnit(unit: string): boolean;

  /**
   * Whether `internal`'s numerator and denominator units are all [compatible]
   * with `unit`.
   *
   * [compatible]: ../spec/types/number.md#compatible-units
   */
  compatibleWithUnit(unit: string): boolean;

  /**
   * Creates a new copy of `this` with its units converted to those represented
   * by `newNumerators` and `newDenominators`:
   *
   * - Let `converter` be the result of
   *
   *   ```
   *   withUnits(0, {
   *     numeratorUnits: newNumerators,
   *     denominatorUnits: newDenominators,
   *   });
   *   ```
   *
   * - If `converter` is not [compatible] with `internal`, throw an error.
   *
   * - Set `converter` to the result of `simplify`ing `converter`.
   *
   * - Return a new `SassNumber` with `internal` set to the result of the
   *   SassScript expression `converter + internal`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  convert(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): SassNumber;

  /**
   * Creates a new copy of `this` with its units converted to the units of
   * `other`:
   *
   * - Let `newNumerators` be the numerator units of `other`.
   * - Let `newDenominators` be the denominator units of `other`.
   * - Return the result of `convert(newNumerators, newDenominators)`.
   *
   * > The `name` and `otherName` parameters may be used for error reporting.
   */
  convertToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): SassNumber;

  /**
   * Return the value of the result of `convert(newNumerators,
   * newDenominators)`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  convertValue(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): number;

  /**
   * Returns the value of the result of `convertToMatch(other)`.
   *
   * > The `name` and `otherName` parameters may be used for error reporting.
   */
  convertValueToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): number;

  /**
   * Creates a new copy of `this` with its units converted to those represented
   * by `newNumerators` and `newDenominators`:
   *
   * - Support converting to/from unitless:
   *
   *   - If `internal` is unitless:
   *
   *     - If `newNumerators` and `newDenominators` are both empty, return
   *       `this`.
   *
   *     - Otherwise, for the duration of this procedure, let `internal` behave
   *       as if its numerator units were equal to `newNumerators` and its
   *       denominator units were equal to `newDenominators`.
   *
   *   - Otherwise, if `newNumerators` and `newDenominators` are both empty, set
   *     `newNumerators` to `internal`'s numerator units and `newDenominators`
   *     to `internal`'s denominator units.
   *
   * - Return the result of `convert(newNumerators, newDenominators)`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  coerce(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): SassNumber;

  /**
   * Creates a new copy of `this` with its units converted to the units of
   * `other`:
   *
   * - Let `newNumerators` be the numerator units of `other`.
   * - Let `newDenominators` be the denominator units of `other`.
   * - Return the result of `coerce(newNumerators, newDenominators)`.
   *
   * > The `name` and `otherName` parameters may be used for error reporting.
   */
  coerceToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): SassNumber;

  /**
   * Return the value of the result of `coerce(newNumerators, newDenominators)`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  coerceValue(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): number;

  /**
   * Returns the value of the result of `coerceToMatch(other)`.
   *
   * > The `name` and `otherName` parameters may be used for error reporting.
   */
  coerceValueToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): number;
}

/**
 * The JS API representation of a Sass string.
 *
 * `internal` refers to a Sass string.
 */
export class SassString extends Value {
  /**
   * Creates a Sass string:
   *
   * - Set `internal` to a Sass string with contents set to `text` and quoted
   *   value set to `options.quotes`.
   * - Return `this`.
   */
  constructor(
    text: string,
    options?: {
      /** @default true */
      quotes: boolean;
    }
  );

  /**
   * Creates an empty Sass string:
   *
   * - Set `internal` to an empty Sass string with quoted value set to
   *   `options.quotes`.
   * - Return `this`.
   */
  static empty(options?: {
    /** @default true */
    quotes: boolean;
  }): SassString;

  /** The contents of `internal` serialized as UTF-16 code units. */
  get text(): string;

  /** Whether `internal` has quotes. */
  get hasQuotes(): boolean;

  /** The number of Unicode code points in `text`. */
  get sassLength(): number;

  /**
   * Converts the Sass index `sassIndex` to a JS index into `text`:
   *
   * - If `sassIndex` is not a unitless Sass number, throw an error.
   *
   * - Let `value` be the value of `sassIndex`. Let `index` be the result of
   *   `fuzzyAsInt(value)`. If `index === null`, throw an error.
   *
   * - If `index === 0`, or the absolute value of `index` is greater than
   *   the length of `sassLength`, throw an error.
   *
   * - If `index > 0`, let `normalizedIndex = index - 1`.
   * - Otherwise, if `index < 0`, let `normalizedIndex = sassLength + index`.
   *
   * - Let `jsIndex` be a JS index. Set `jsIndex` to the first code
   *   unit of the Unicode code point that `normalizedIndex` points to.
   *
   *   > Sass indices count Unicode code points, whereas JS indices count
   *   > UTF-16 code units.
   *
   * - Return `jsIndex`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  sassIndexToStringIndex(sassIndex: Value, name?: string): number;
}
