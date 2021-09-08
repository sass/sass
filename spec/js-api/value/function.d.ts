import {Value} from './index';

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
   *   upon execution, runs `callback` and returns the result.
   * - Return `this`.
   */
  constructor(
    /**
     * Must be a valid Sass function signature that could appear after the
     * `@function` directive in a Sass stylesheet, such as
     * `mix($color1, $color2, $weight: 50%)`.
     */
    signature: string,
    callback: (args: Value[]) => Value
  );
}
