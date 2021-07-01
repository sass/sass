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

/** ## API */

export class ListSeparator {
  static get comma(): ListSeparator;

  static get slash(): ListSeparator;

  static get space(): ListSeparator;

  static get undecided(): ListSeparator;

  get separator(): string;

  toString(): string;
}

/**
 * The JS API representation of a Sass value.
 *
 * > To make the spec terser and easier to author, each subclass that extends
 * > Value has a virtual, private property named `internal` that refers to the
 * > Sass value it is representing.
 */
export abstract class Value {
  get asList(): Value[];

  get hasBrackets(): boolean;

  get isTruthy(): boolean;

  get realNull(): Value | null;

  get separator(): ListSeparator;

  sassIndexToListIndex(sassIndex: Value, name?: string): number;

  assertBoolean(name?: string): SassBoolean;

  assertColor(name?: string): SassColor;

  assertFunction(name?: string): SassFunction;

  assertMap(name?: string): SassMap;

  tryMap(): SassMap | null;

  assertNumber(name?: string): SassNumber;

  assertString(name?: string): SassString;

  toString(): string;
}

/**
 * The JS API representation of a Sass color.
 *
 * `internal` refers to a Sass number.
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
   *   - $red set to `sassRed`
   *   - $green set to `sassGreen`
   *   - $blue set to `sassBlue`
   *   - If `alpha` was passed, $alpha set to `sassAlpha`
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
   *   - $hue set to `sassHue`
   *   - $saturation set to `sassSaturation`
   *   - $lightness set to `sassLightness`
   *   - If `alpha` was passed, $alpha set to `sassAlpha`
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
   *   - $hue set to `sassHue`
   *   - $whiteness set to `sassWhiteness`
   *   - $blackness set to `sassBlackness`
   *   - If `alpha` was passed, $alpha set to `sassAlpha`
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

  /**
   * Returns the result of calling [`red(internal)`][red].
   *
   * [red]: ../spec/built-in-modules/color.md#red
   */
  get red(): number;

  /**
   * Returns the result of calling [`green(internal)`][green].
   *
   * [green]: ../spec/built-in-modules/color.md#green
   */
  get green(): number;

  /**
   * Returns the result of calling [`blue(internal)`][blue].
   *
   * [blue]: ../spec/built-in-modules/color.md#blue
   */
  get blue(): number;

  /**
   * Returns the result of calling [`hue(internal)`][hue].
   *
   * [hue]: ../spec/built-in-modules/color.md#hue
   */
  get hue(): number;

  /**
   * Returns the result of calling [`saturation(internal)`][saturation].
   *
   * [saturation]: ../spec/built-in-modules/color.md#saturation
   */
  get saturation(): number;

  /**
   * Returns the result of calling [`lightness(internal)`][lightness].
   *
   * [lightness]: ../spec/built-in-modules/color.md#lightness
   */
  get lightness(): number;

  /**
   * Returns the result of calling [`whiteness(internal)`][whiteness].
   *
   * [whiteness]: ../spec/built-in-modules/color.md#whiteness
   */
  get whiteness(): number;

  /**
   * Returns the result of calling [`blackness(internal)`][blackness].
   *
   * [blackness]: ../spec/built-in-modules/color.md#blackness
   */
  get blackness(): number;

  /**
   * Returns the result of calling [`alpha(internal)`][alpha].
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

  /** Returns a new copy of `this` with the alpha channel set to `alpha`. */
  changeAlpha(alpha: number): SassColor;
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
   * - Set `internal` to a Sass number with value equal to `value` and a single
   *   numerator unit equal to `unit` (if passed).
   *
   * - Return `this`.
   */
  constructor(value: number, unit?: string);

  /**
   * Creates a Sass number:
   *
   * - Set `internal` to a Sass number with value equal to `value`, numerator
   *   units set to `options.numeratorUnits` (if passed), and denominator units
   *   set to `options.denominatorUnits` (if passed).
   *
   * - Set `internal` to the result of `simplify`ing `internal`.
   *
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
   * - Otherwise, return null.
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
   * Whether `internal`'s numerator and denominator units are all `compatible`
   * with `unit`.
   *
   * [`compatible`]: ../spec/types/number.md#compatible-units
   */
  compatibleWithUnit(unit: string): boolean;

  /**
   * Creates a new copy of `this` with its units converted to those represented
   * by `newNumerators` and `newDenominators`:
   *
   * - Let `converter` be the result of
   *   ```
   *   withUnits(0, {
   *     numeratorUnits: newNumerators,
   *     denominatorUnits: newDenominators,
   *   });
   *   ```
   * - If `converter` is not [`compatible`] with `internal`, throw an error.
   * - Set `converter` to the result of `simplify`ing `converter`.
   * - Return a new `SassNumber` with `internal` set to the result of
   *   `converter + internal`.
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
   * - Let `converted` be the result of
   *   `coerce(newNumerators, newDenominators)`.
   * - Return `converted.value`.
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

  /**
   * Creates a new copy of `this` with its units converted to those represented
   * by `newNumerators` and `newDenominators`:
   *
   * - If `hasUnits` is true, but `newNumerators` and `newDenominators` are both
   *   empty, throw an error.
   * - If `hasUnits` is false, but `newNumerators` and `newDenominators` are not
   *   both empty, throw an error.
   * - Return the result of `coerce(newNumerators, newDenominators)`.
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
   * - Let `converted` be the result of
   *   `convert(newNumerators, newDenominators)`.
   * - Return `converted.value`.
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
   * - Set `internal` to a Sass string with the same contents as `text` and
   *   quoted value equal to `options.quotes`.
   * - Return `this`.
   */
  constructor(
    text: string,
    options: {
      /** @default true */
      quotes: boolean;
    }
  );

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
   *   `fuzzyAsInt(value)`. If `index` is null, throw an error.
   *
   * - If `index === 0`, throw an error.
   *
   * - If the absolute value of `index` is greater than `sassLength`, throw
   *   an error.
   *
   *   > Sass indices start counting at 1, and may be negative in order to index
   *   > from the end of the string.
   *
   * - If `index > 0`, let `normalizedIndex = index - 1`.
   * - Otherwise, if `index < 0`, let `normalizedIndex = sassLength + index`.
   *
   *   > Sass indices count Unicode [code point]s, whereas JS indices count
   *   > UTF-16 [code units].
   *   > [code point]: https://unicode.org/glossary/#code_point
   *   > [code unit]: https://unicode.org/glossary/#code_unit
   *
   * - Let `jsIndex` be a JS index. Set `jsIndex` to the first code
   *   unit of the Unicode code point that `normalizedIndex` points to.
   *
   * - Return `jsIndex`.
   */
  sassIndexToStringIndex(sassIndex: Value): number;
}
