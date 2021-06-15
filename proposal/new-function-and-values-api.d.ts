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
   * Creates a string with the given `text`:
   *
   * - If `options.quotes == true`, set this string's `text` to the semantic
   *   content of `text`, resolving any escape sequences to their Unicode
   *   values.
   * - Otherwise, set this string's `text` to the literal content of `text`,
   *   preserving the escape sequences as backslashes.
   * - Set this string's `sassLength` to the number of Unicode code points
   *   contained in `text`.
   *
   *   > Sass internally represents strings as sequences of Unicode
   *   > [code point]s, whereas JS represents them as UTF-16 [code unit]s.
   *   > Implementations must convert strings to Unicode code points before
   *   > setting `text`.
   *
   *   [code point]: https://unicode.org/glossary/#code_point
   *   [code unit]: https://unicode.org/glossary/#code_unit
   */
  constructor(
    text: string,
    options: {
      /** @default false */
      quotes: boolean;
    }
  );

  /** The contents of the string. */
  get text(): string;

  /** Whether the string has quotes. */
  get hasQuotes(): boolean;

  /** The number of Unicode code points in the string. */
  get sassLength(): number;

  /**
   * Converts the Sass index `sassIndex` to a JS index into `text`:
   *
   * - If `sassIndex` isn't an integer, throw an `Exception`.
   * - If `sassIndex == 0`, throw an `Exception`.
   * - If the absolute value of `sassIndex` is greater than `sassLength`, throw
   *   an `Exception`.
   * - Sass indices start counting at 1, and may be negative in order to index
   *   from the end of the string. Let `normalizedIndex` be equivalent to a Sass
   *   index, except it starts counting at 0 and does not support negative
   *   values.
   *   - If `sassIndex > 0`, set `normalizedIndex = sassIndex - 1`.
   *   - Otherwise, if `sassIndex < 0`, set `normalizedIndex = sassLength + sassIndex`.
   * - Sass indices count Unicode code points, whereas JS indices count UTF-16
   *   code units. Let `jsIndex` be a JS index. Set `jsIndex` to the first code
   *   unit of the Unicode code point that `normalizedIndex` points to.
   * - Return `jsIndex`.
   */
  sassIndexToStringIndex(sassIndex: Value): number;
}
