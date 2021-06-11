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

declare abstract class Value {
  readonly asList: Value[];

  readonly hasBrackets: boolean;

  readonly isTruthy: boolean;

  readonly realNull: Value | null;

  readonly separator: ListSeparator;

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

export class SassString extends Value {
  /** The contents of the string. */
  readonly text: string;

  /** Whether the string has quotes. */
  readonly hasQuotes: boolean;

  /** The Unicode-code-point-aware length of the string. */
  readonly sassLength: number;

  /**
   * Creates a string with the given `text`.
   *
   * @param [options.quotes = false]
   */
  constructor(text: string, options: {quotes: boolean});

  /**
   * Converts `sassIndex` to a JS-style index into `text`.
   *
   * Given a Sass index `index`, perform the following procedure:
   * - If `sassIndex` isn't an integer or a valid index for this string, throw
   *   an `Exception`.
   * - Make `index` refer to UTF-16 code units, since Sass indices refer to
   *   Unicode code points.
   * - Convert `index` from one-based to zero-based.
   *   - If `index` is negative, it is indexing from the end of the string.
   *     Convert this to the equivalent positive index.
   */
  sassIndexToStringIndex(sassIndex: Value): number;
}
