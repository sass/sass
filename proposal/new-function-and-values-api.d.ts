/**
 * # New Function and Values API: Draft 1
 *
 * *([Issue](https://github.com/sass/sass/issues/2510))*
 *
 * ## Background
 *
 * > This section is non-normative.
 *
 * ## Summary
 *
 * > This section is non-normative.
 */

import {OrderedMap} from 'immutable';

/** ## API */

/**
 * The JS API representation of a Sass value.
 *
 * > To make the spec terser and easier to author, each subclass that extends
 * > `Value` has a virtual, private property named `internal` that refers to the
 * > Sass value it represents.
 */
export abstract class Value {
  /**
   * Returns `this` as an array:
   *
   * - If `internal` is a Sass list, return an array of its contents.
   * - If `internal` is a Sass map, return an array of its keys and values as
   *   two-element `SassList`s.
   * - Otherwise, return an array containing `this`.
   */
  get asList(): Value[];

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
   */
  sassIndexToListIndex(sassIndex: Value): number;

  /**
   * Asserts that `this` is a `SassBoolean`.
   *
   * - If `internal` is a Sass boolean, return `this`.
   * - Otherwise, throw an error.
   */
  assertBoolean(): SassBoolean;

  /**
   * Asserts that `this` is a `SassColor`.
   *
   * - If `internal` is a Sass color, return `this`.
   * - Otherwise, throw an error.
   */
  assertColor(): SassColor;

  /**
   * Asserts that `this` is a `SassFunction`.
   *
   * - If `internal` is a Sass function, return `this`.
   * - Otherwise, throw an error.
   */
  assertFunction(): SassFunction;

  /**
   * Asserts that `this` is a `SassMap`.
   *
   * - If `internal` is a Sass map, return `this`.
   * - Otherwise, throw an error.
   */
  assertMap(): SassMap;

  /**
   * Returns `this` as a `SassMap`.
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
  asMap(): OrderedMap<Value, Value> | null;

  /**
   * Asserts that `this` is a `SassNumber`.
   *
   * - If `internal` is a Sass number, return `this`.
   * - Otherwise, throw an error.
   */
  assertNumber(): SassNumber;

  /**
   * Asserts that `this` is a `SassString`.
   *
   * - If `internal` is a Sass string, return `this`.
   * - Otherwise, throw an error.
   */
  assertString(): SassString;

  /** Whether `this == other` in SassScript. */
  equals(other: Value): boolean;

  /**
   * Must be the same for `Value`s that are equal to each other according to the
   * `==` SassScript operator.
   */
  hashCode(): string;
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
   *   upon execution, runs `callback`.
   * - Return `this`.
   */
  constructor(
    /**
     * Must be a valid Sass function signature that could appear after the
     * `@function` directive in a Sass stylesheet, such as
     * `mix($color1, $color2, $weight: 50%)`.
     */
    signature: string,
    callback: (args: Value[] | ArgumentList) => Value | Promise<Value>
  );

  /** `internal`'s signature. */
  get signature(): string;

  /** The callback that runs when `internal` is executed. */
  get callback(): (args: Value[] | ArgumentList) => Value | Promise<Value>;
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
    contents: Value[],
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
    contents: Value[],
    keywords: Record<string, Value>,
    /** @default ',' */
    separator?: ListSeparator
  );
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
      numeratorUnits?: string[];
      denominatorUnits?: string[];
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
  get numeratorUnits(): string[];

  /** `internal`'s denominator units. */
  get denominatorUnits(): string[];

  /** Whether `internal` has numerator or denominator units. */
  get hasUnits(): boolean;

  /**
   * Asserts that `internal`'s value is an integer:
   *
   * - If `internal`'s value `fuzzyEquals` an integer, return that integer.
   * - Otherwise, throw an error.
   */
  assertInt(): number;

  /**
   * Asserts that `internal`'s value is within the specified range:
   *
   * - If `internal`'s value is `fuzzyGreaterThan` `min` and `fuzzyLessThan`
   *   `max`, return it.
   * - Otherwise, if `internal`'s value `fuzzyEquals` `min`, return `min`.
   * - Otherwise, if `internal`'s value `fuzzyEquals` `max`, return `max`.
   * - Otherwise, throw an error.
   */
  assertInRange(min: number, max: number): number;

  /**
   * Asserts that `internal` is unitless:
   *
   * - If `internal` has any numerator or denominator units, throw an error.
   * - Otherwise, return `this`.
   */
  assertNoUnits(): SassNumber;

  /**
   * Asserts the type of `internal`'s unit:
   *
   * - If `internal` has any denominator units, or if `unit` is not `internal`'s
   *   only numerator unit, throw an error.
   * - Otherwise, return `this`.
   */
  assertUnit(unit: string): SassNumber;

  /**
   * Whether `internal` has the specified unit:
   *
   * - If `internal` has any denominator units, return false.
   * - Otherwise, return whether `unit` is `internal`'s only numerator unit.
   * */
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
   */
  convert(newNumerators: string[], newDenominators: string[]): SassNumber;

  /**
   * Creates a new copy of `this` with its units converted to the units of
   * `other`:
   *
   * - Let `newNumerators` be the numerator units of `other`.
   * - Let `newDenominators` be the denominator units of `other`.
   * - Return the result of `convert(newNumerators, newDenominators)`.
   */
  convertToMatch(other: SassNumber): SassNumber;

  /**
   * Returns the value of `internal`, converted to the units represented by
   * `newNumerators` and `newDenominators`:
   *
   * - Return the value of the result of `convert(newNumerators, newDenominators)`.
   */
  convertValue(newNumerators: string[], newDenominators: string[]): number;

  /**
   * Returns the value of `internal`, converted to the units of `other`.
   *
   * - Let `newNumerators` be the numerator units of `other`.
   * - Let `newDenominators` be the denominator units of `other`.
   * - Return the result of `convertValue(newNumerators, newDenominators)`.
   */
  convertValueToMatch(other: SassNumber): number;

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
   */
  coerce(newNumerators: string[], newDenominators: string[]): SassNumber;

  /**
   * Creates a new copy of `this` with its units converted to the units of
   * `other`:
   *
   * - Let `newNumerators` be the numerator units of `other`.
   * - Let `newDenominators` be the denominator units of `other`.
   * - Return the result of `coerce(newNumerators, newDenominators)`.
   */
  coerceToMatch(other: SassNumber): SassNumber;

  /**
   * Returns the value of `internal`, converted to the units represented by
   * `newNumerators` and `newDenominators`:
   *
   * - Return the value of the result of `coerce(newNumerators, newDenominators)`.
   */
  coerceValue(newNumerators: string[], newDenominators: string[]): number;

  /**
   * Returns the value of `internal`, converted to the units of `other`.
   *
   * - Let `newNumerators` be the numerator units of `other`.
   * - Let `newDenominators` be the denominator units of `other`.
   * - Return the result of `coerceValue(newNumerators, newDenominators)`.
   */
  coerceValueToMatch(other: SassNumber): number;
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
   *   > Sass indices count Unicode [code point]s, whereas JS indices count
   *   > UTF-16 [code units].
   *   >
   *   > [code point]: https://unicode.org/glossary/#code_point
   *   > [code unit]: https://unicode.org/glossary/#code_unit
   *
   * - Return `jsIndex`.
   */
  sassIndexToStringIndex(sassIndex: Value): number;
}
