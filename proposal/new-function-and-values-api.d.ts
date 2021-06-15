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
