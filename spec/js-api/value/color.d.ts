import {Value} from './index';

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
