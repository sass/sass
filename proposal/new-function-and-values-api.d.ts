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

/** The JS API representation of a Sass color. */
export class SassColor extends Value {
  /**
   * Creates an RGB color:
   *
   * - If `red` is not between `0` and `255` inclusive, throw an `Exception`.
   * - If `green` is not between `0` and `255` inclusive, throw an `Exception`.
   * - If `blue` is not between `0` and `255` inclusive, throw an `Exception`.
   * - If `alpha` is not between `0` and `1` inclusive, throw an `Exception`.
   * - Let `color` be a new `SassColor`:
   *   - Set `color`'s `red` channel to `red`.
   *   - Set `color`'s `green` channel to `green`.
   *   - Set `color`'s `blue` channel to `blue`.
   *   - If `alpha` was passed, set `color`'s `alpha` channel to `alpha`.
   *   - Otherwise, set `color`'s `alpha` channel to 1.
   * - Return `color`.
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
   * - If `saturation` is not between `0` and `100` inclusive, throw an
   *   `Exception`.
   * - If `lightness` is not between `0` and `100` inclusive, throw an
   *   `Exception`.
   * - If `alpha` is not between `0` and `1` inclusive, throw an `Exception`.
   * - Let `color` be a new `SassColor`:
   *   - Set `color`'s `hue` to `hue % 360`.
   *   - Set `color`'s `saturation` to `saturation`.
   *   - Set `color`'s `lightness` to `lightness`.
   *   - If `alpha` was passed, set `color`'s `alpha` channel to `alpha`.
   *   - Otherwise, set `color`'s `alpha` channel to 1.
   * - Return `color`.
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
   * - If `whiteness` is not between `0` and `100` inclusive, throw an
   *   `Exception`.
   * - If `blackness` is not between `0` and `100` inclusive, throw an
   *   `Exception`.
   * - If `alpha` is not between `0` and `1` inclusive, throw an `Exception`.
   * - Let `color` be a new `SassColor`.
   *   - Set `color`'s `hue` to `hue % 360`.
   *   - Set `color`'s `whiteness` to `whiteness`.
   *   - Set `color`'s `blackness` to `blackness`.
   *   - If `alpha` was passed, set `color`'s `alpha` channel to `alpha`.
   *   - Otherwise, set `color`'s `alpha` channel to 1.
   * - Return `color`.
   */
  static hwb(
    hue: number,
    whiteness: number,
    blackness: number,
    alpha?: number
  ): SassColor;

  /** This color's red channel. */
  get red(): number;

  /** This color's green channel. */
  get green(): number;

  /** This color's blue channel. */
  get blue(): number;

  /** This color's hue. */
  get hue(): number;

  /** This color's saturation. */
  get saturation(): number;

  /** This color's lightness. */
  get lightness(): number;

  /** This color's whiteness. */
  get whiteness(): number;

  /** This color's blackness. */
  get blackness(): number;

  /** This color's alpha channel. */
  get alpha(): number;

  /**
   * Returns a new copy of this color with one or more changes made to the RGB
   * channels:
   *
   * - Let `oldColor` be this color.
   * - If `red` was passed, let `newRed = red`.
   * - Otherwise, let `newRed = oldColor.red`.
   * - If `green` was passed, let `newGreen = green`.
   * - Otherwise, let `newGreen = oldColor.green`.
   * - If `blue` was passed, let `newBlue = blue`.
   * - Otherwise, let `newBlue = oldColor.blue`.
   * - If `alpha` was passed, let `newAlpha = alpha`.
   * - Otherwise, let `newAlpha = oldColor.alpha`.
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
   * - If `hue` was passed, let `newHue = hue`.
   * - Otherwise, let `newHue = oldColor.hue`.
   * - If `saturation` was passed, let `newSaturation = saturation`.
   * - Otherwise, let `newSaturation = oldColor.saturation`.
   * - If `lightness` was passed, let `newLightness = lightness`.
   * - Otherwise, let `newLightness = oldColor.lightness`.
   * - If `alpha` was passed, let `newAlpha = alpha`.
   * - Otherwise, let `newAlpha = oldColor.alpha`.
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
   * - If `hue` was passed, let `newHue = hue`.
   * - Otherwise, let `newHue = oldColor.hue`.
   * - If `whiteness` was passed, let `newWhiteness = whiteness`.
   * - Otherwise, let `newWhiteness = oldColor.whiteness`.
   * - If `blackness` was passed, let `newBlackness = blackness`.
   * - Otherwise, let `newBlackness = oldColor.blackness`.
   * - If `alpha` was passed, let `newAlpha = alpha`.
   * - Otherwise, let `newAlpha = oldColor.alpha`.
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

/** The JS API representation of a Sass string. */
export class SassString extends Value {
  /**
   * > This virtual property exists only to make the spec more concise and
   * > precise.
   *
   * An object that behaves like a Sass string instance.
   *
   * internalString: Value;
   */

  /**
   * - If `options.quotes === true`, set `internalString`'s contents to `text`.
   * - Otherwise, let `escapedText` be `text` with all of its escape sequences
   *   preserved as literal backslashes. Set `internalString`'s contents to
   *   `escapedText`.
   */
  constructor(
    text: string,
    options: {
      /** @default true */
      quotes: boolean;
    }
  );

  /** The contents of `internalString`. */
  get text(): string;

  /** Whether `internalString` has quotes. */
  get hasQuotes(): boolean;

  /** The number of Unicode code points in `text`. */
  get sassLength(): number;

  /**
   * Converts the Sass index `sassIndex` to a JS index into `text`:
   *
   * - If `sassIndex` is not a Sass number, throw an exception.
   *
   * - Let `value` be the value of `sassIndex`. Let `index` be the result of
   *   `fuzzyAsInt(value)`. If `index` is null, throw an `Exception`.
   *
   * - If `index === 0`, throw an `Exception`.
   *
   * - If the absolute value of `index` is greater than `sassLength`, throw
   *   an `Exception`.
   *
   * > Sass indices start counting at 1, and may be negative in order to index
   * > from the end of the string.
   *
   * - If `index > 0`, let `normalizedIndex = index - 1`.
   * - Otherwise, if `index < 0`, let `normalizedIndex = sassLength + index`.
   *
   * > Sass indices count Unicode [code point]s, whereas JS indices count UTF-16
   * > [code units].
   * > [code point]: https://unicode.org/glossary/#code_point
   * > [code unit]: https://unicode.org/glossary/#code_unit
   *
   * - Let `jsIndex` be a JS index. Set `jsIndex` to the first code
   *   unit of the Unicode code point that `normalizedIndex` points to.
   *
   * - Return `jsIndex`.
   */
  sassIndexToStringIndex(sassIndex: Value): number;
}
