import {Value} from './index';

/**
 * The JS API representation of a Sass string.
 *
 * `internal` refers to a Sass string.
 */
export class SassString extends Value {
  /**
   * Creates a Sass string:
   *
   * - If the first argument is a string:
   *   - Let `text` be the first argument.
   *   - Let `options` be the second argument, or `{}` if it's undefined.
   *
   * - Otherwise:
   *   - Let `text` be `""`.
   *   - Let `options` be the first argument, or `{}` if it's undefined.
   *
   * - Let `quotes` be `options.quotes`, or `true` if that's undefined.
   *
   * - Set `internal` to a Sass string with contents set to `text` and quoted
   *   set to `quotes`.
   *
   * - Return `this`.
   */
  constructor(
    text: string,
    options?: {
      quotes?: boolean;
    }
  );

  constructor(options?: {/** @default true */ quotes?: boolean});

  /**
   * Creates an empty Sass string:
   *
   * - Set `internal` to an empty Sass string with quoted value set to
   *   `options.quotes`.
   * - Return `this`.
   */

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
