import {Value} from './index';

/**
 * The JS API representation of a Sass color.
 *
 * `internal` refers to a Sass color.
 */
export class SassColor extends Value {
  /**
   * - If `options.red` is set:
   *
   *   - Let `red` be a Sass number with a value of `options.red` `fuzzyRound`ed
   *     to the nearest integer.
   *
   *   - Let `green` be a Sass number with a value of `options.green`
   *     `fuzzyRound`ed to the nearest integer.
   *
   *   - Let `blue` be a Sass number with a value of `options.blue`
   *     `fuzzyRound`ed to the nearest integer.
   *
   *   - If `options.alpha` is set, let `alpha` be a Sass number with a value of
   *     `options.alpha`. Otherwise, let `alpha` be `null`.
   *
   *   - Set `internal` to the result of running [`rgb()`] with `$red`, `$green`,
   *     `$blue`, and `$alpha`.
   *
   *     [`rgb()`]: ../../functions.md#rgb-and-rgba
   *
   * - Otherwise, if `options.saturation` is set:
   *
   *   - Let `hue` be a Sass number with a value of `options.hue`.
   *
   *   - Let `saturation` be a Sass number with a value of `options.saturation`.
   *
   *   - Let `lightness` be a Sass number with a value of `options.lightness`.
   *
   *   - If `options.alpha` is set, let `alpha` be a Sass number with a value of
   *     `options.alpha`. Otherwise, let `alpha` be `null`.
   *
   *   - Set `internal` to the result of running [`hsl()`] with `$hue`, `$saturation`,
   *     `$lightness`, and `$alpha`.
   *
   *     [`hsl()`]: ../../functions.md#hsl-and-hsla
   *
   * - Otherwise, if `options.whiteness` is set:
   *
   *   - Let `hue` be a Sass number with a value of `options.hue`.
   *
   *   - Let `whiteness` be a Sass number with a value of `options.whiteness`.
   *
   *   - Let `blackness` be a Sass number with a value of `options.blackness`.
   *
   *   - If `options.alpha` is set, let `alpha` be a Sass number with a value of
   *     `options.alpha`. Otherwise, let `alpha` be `null`.
   *
   *   - Set `internal` to the result of running [`hwb()`] with `$hue`, `$whiteness`,
   *     `$blackness`, and `$alpha`.
   *
   *     [`hwb()`]: ../../built-in-modules/color.md#hwb
   *
   * - Return `this`.
   */
  constructor(options: {
    red: number;
    green: number;
    blue: number;
    alpha?: number;
  });

  constructor(options: {
    hue: number;
    saturation: number;
    lightness: number;
    alpha?: number;
  });

  constructor(options: {
    hue: number;
    whiteness: number;
    blackness: number;
    alpha?: number;
  });

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
   * - If `options.whiteness` or `options.blackness` is set:
   *
   *   - Let `hue` be `options.hue` if it was passed, or `this.hue` otherwise.
   *
   *   - Let `whiteness` be `options.whiteness` if it was passed, or
   *     `this.whiteness` otherwise.
   *
   *   - Let `blackness` be `options.blackness` if it was passed, or
   *     `this.blackness` otherwise.
   *
   *   - Let `alpha` be `options.alpha` if it was passed, or `this.alpha`
   *     otherwise.
   *
   *   - Return the result of `SassColor({hue, whiteness, blackness, alpha})`.
   *
   * - Otherwise, if `options.hue`, `options.saturation`, or `options.lightness`
   *   is set:
   *
   *   - Let `hue` be `options.hue` if it was passed, or `this.hue` otherwise.
   *
   *   - Let `saturation` be `options.saturation` if it was passed, or
   *     `this.saturation` otherwise.
   *
   *   - Let `lightness` be `options.lightness` if it was passed, or
   *     `this.lightness` otherwise.
   *
   *   - Let `alpha` be `options.alpha` if it was passed, or `this.alpha`
   *     otherwise.
   *
   *   - Return the result of `SassColor({hue, saturation, lightness, alpha})`.
   *
   * - Otherwise:
   *
   *   - Let `red` be `options.red` if it was passed, or `this.red` otherwise.
   *
   *   - Let `green` be `options.green` if it was passed, or `this.green`
   *     otherwise.
   *
   *   - Let `blue` be `options.blue` if it was passed, or `this.blue`
   *     otherwise.
   *
   *   - Let `alpha` be `options.alpha` if it was passed, or `this.alpha`
   *     otherwise.
   *
   *   - Return the result of `SassColor({red, green, blue, alpha})`.
   */
  change(options: {
    red?: number;
    green?: number;
    blue?: number;
    alpha?: number;
  }): SassColor;

  change(options: {
    hue?: number;
    saturation?: number;
    lightness?: number;
    alpha?: number;
  }): SassColor;

  change(options: {
    hue?: number;
    whiteness?: number;
    blackness?: number;
    alpha?: number;
  }): SassColor;
}
