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
   * - Return this color.
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
   * - Return this color.
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
   * - Return this color.
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

  /** `internal`'s hue. */
  get hue(): number;

  /** `internal`'s saturation. */
  get saturation(): number;

  /** `internal`'s lightness. */
  get lightness(): number;

  /** `internal`'s whiteness. */
  get whiteness(): number;

  /** `internal`'s blackness. */
  get blackness(): number;

  /** `internal`'s alpha channel. */
  get alpha(): number;

  /**
   * Returns a new copy of this color with one or more changes made to the RGB
   * channels:
   *
   * - Let `oldColor` be this color.
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
   * Returns a new copy of this color with one or more changes made to the HSL
   * values:
   *
   * - Let `oldColor` be this color.
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
   * Returns a new copy of this color with one or more changes made to the HWB
   * values:
   *
   * - Let `oldColor` be this color.
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

  /** Returns a new copy of this color with the alpha channel set to `alpha`. */
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
   * - Set `internal` to a Sass number with value equal to `value` and with
   *   unit equal to `unit` (if passed).
   *
   * - Return this number.
   */
  constructor(value: number, unit?: string);

  /**
   * Creates a Sass number:
   *
   * - Let `simplifiedNumeratorUnits` and `simplifiedDenominatorUnits` be
   *   empty lists.
   *
   * - If either `options.numeratorUnits` or `options.denominatorUnits` were not
   *   passed, let them be empty lists.
   *
   * - Set {`simplifiedNumeratorUnits`, `simplifiedDenominatorUnits`} to the
   *   result of simplifying away all [compatible units] in
   *   {`options.numeratorUnits`, `options.denominatorUnits`}, according to the
   *   multiplicative factor.
   *
   *   [compatible units]: ../spec/types/number.md#compatible-units
   *
   * - Set `internal` to a Sass number with value equal to `value`, with
   *   numeratorUnits equal to `simplifiedNumeratorUnits` (if non-empty), and
   *   with denominatorUnits equal to `simplifiedDenominatorUnits`
   *   (if non-empty).
   *
   * - Return this number.
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

  /** `internal`'s numerator units. */
  get numeratorUnits(): string[];

  /** `internal`'s denominator units. */
  get denominatorUnits(): string[];

  /** Whether `internal` has units. */
  get hasUnits(): boolean;

  /** Whether `internal`'s value `fuzzyEquals` an integer. */
  get isInt(): boolean;

  /**
   * - If `internal`'s value `fuzzyEquals` an integer, return that integer.
   * - Otherwise, return null.
   */
  get asInt(): number | null;

  /**
   * - Throw an `Exception` if `internal` has units.
   * - Otherwise, return this number.
   */
  assertNoUnits(): SassNumber;

  /**
   * - Throw an `Exception` unless `unit` is `internal`'s only unit, as a
   *   numerator.
   * - Otherwise, return this number.
   */
  assertUnit(unit: string): SassNumber;

  /**
   * - If `internal`'s value `fuzzyEquals` an integer, return that integer.
   * - Otherwise, throw an `Exception`.
   */
  assertInt(): number;

  /**
   * - If `internal`'s value is `fuzzyGreaterThan` `min`, return it.
   * - If `internal`'s value is `fuzzyLessThan` `max`, return it.
   * - If `internal`'s value `fuzzyEquals` `min`, return `min`.
   * - If `internal`'s value `fuzzyEquals` `max`, return `max`.
   * - Otherwise, throw an `Exception`.
   */
  assertInRange(min: number, max: number): number;

  /** Whether `unit` is `internal`'s only unit, as a numerator. */
  hasUnit(unit: string): boolean;

  /** Whether `internal`'s units are compatible with `unit`. */
  compatibleWithUnit(unit: string): boolean;

  /**
   * Creates a new copy of this number with its units converted to those
   * represented by `newNumerators` and `newDenominators`:
   *
   * - If this number is unitless, return the result of
   *   ```
   *   withUnits(value, {
   *     numeratorUnits: newNumeratorUnits,
   *     denominatorUnits: newDenominatorUnits,
   *   });
   *   ```.
   *
   * - Set {`simplifiedNewNumeratorUnits`, `simplifiedNewDenominatorUnits`} to
   *   the result of simplifying away all compatible units in
   *   {`newNumeratorUnits`, `newDenominatorUnits`}.
   *
   * - Let `numeratorComponents` be a list of Sass numbers. For each `unit` in
   *   numeratorUnits, add to the list `withUnits(1, {numeratorUnits: unit})`.
   * - Let `denominatorComponents` be a list of Sass numbers. For each `unit` in
   *   denominatorUnits, add to the list `withUnits(1, {denominatorUnits: unit})`.
   *
   * - Let `convertedNumerators` be a list of Sass numbers. For each `number` in
   *   `numeratorComponents`, find the compatible unit `newUnit` in
   *   `simplifiedNewNumerators`. Add the result of [converting] `number` to
   *   `newUnit` to `convertedNumerators`.
   * - Let `convertedDenominators` be a list of Sass numbers. For each
   *   `number` in `denominatorComponents`, find the compatible unit
   *   `newUnit` in `simplifiedNewDenominators`. Add the result of [converting]
   *   `number` to `newUnit` to `convertedDenominators`.
   * - Each number in `numeratorComponents` must have been converted to exactly
   *   one unit in `simplifiedNewNumerators`, and each number in
   *   `denominatorComponents` must have been converted to exactly one unit in
   *   `simplifiedNewDenominators`. Otherwise, throw an Exception.
   *
   *   [converting]: ../spec/types/number.md#converting-a-number-to-a-unit
   *
   * - Let `newValue` be the result of multiplying `value` with all the values
   *   of the numbers in `convertedNumerators` and the inverse of all the values
   *   of the numbers in `convertedDenominators`.
   *
   * - Return the result of
   *   ```
   *   withUnits(newValue, {
   *     numeratorUnits: newNumeratorUnits,
   *     denominatorUnits: newDenominatorUnits,
   *   });
   *   ```.
   */
  coerce(newNumerators: string[], newDenominators: string[]): SassNumber;

  // TODO(awjin)
  coerceToMatch(other: SassNumber): SassNumber;

  // TODO(awjin)
  coerceValue(newNumerators: string[], newDenominators: string[]): number;

  // TODO(awjin)
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
   * - Set `internal` to a Sass string with the same contents as `text` and
   *   quoted value equal to `options.quotes`.
   *
   * - Return this string.
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
   * - If `sassIndex` is not a unitless Sass number, throw an exception.
   *
   * - Let `value` be the value of `sassIndex`. Let `index` be the result of
   *   `fuzzyAsInt(value)`. If `index` is null, throw an `Exception`.
   *
   * - If `index === 0`, throw an `Exception`.
   *
   * - If the absolute value of `index` is greater than `sassLength`, throw
   *   an `Exception`.
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
