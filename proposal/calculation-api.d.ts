/**
 * # JavaScript Calculation API: Draft 2
 *
 * *([Issue](https://github.com/sass/sass/issues/818),
 * [Changelog](calculation-api.changes.md))*
 *
 * ## Background
 *
 * > This section is non-normative.
 *
 * This proposal simply exposes the [calculation type] to the JavaScript API.
 *
 * [calculation type]: ../accepted/first-class-calc.md
 *
 * ## Summary
 *
 * > This section is non-normative.
 *
 * ### Design Decisions
 *
 * #### Simplification
 *
 * We considered eagerly simplifying calculations as they were constructed to
 * match the behavior of values in Sass itself. However, this poses a problem
 * for API implementations that don't have direct access to compiler logic, such
 * as the Node.js embedded host: they would need to implement the simplification
 * logic locally, which is relatively complex and opens a broad surface area for
 * subtle cross-implementation incompatibilities.
 *
 * This could potentially be solved by adding an explicit request to the
 * embedded protocol, but this would pose its own problems given that JS is
 * strict about separating asynchronous calls (like those across process
 * boundaries) and synchronous calls (like this API).
 *
 * Given that, we chose instead to handle simplification only at the custom
 * function boundary rather than when a calculation is constructed.
 */

/* ## API */

import {List, ValueObject} from 'immutable';

import {Value, SassNumber, SassString} from '../spec/js-api/value';

declare module '../spec/js-api/value' {
  interface Value {
    /**
     * Asserts that `this` is a `SassCalculation`:
     *
     * - If `internal` is a Sass calculation, return `this`.
     * - Otherwise, throw an error.
     *
     * > The `name` parameter may be used for error reporting.
     */
    assertCalculation(name?: string): SassCalculation;
  }
}

declare module '../spec/js-api/options' {
  interface Options<sync extends 'sync' | 'async'> {
    /**
     * Replace this option's specification with:
     *
     * Before beginning compilation:
     *
     * - For each key/value pair `signature`/`function` in this record:
     *
     *   - If `signature` isn't an [<ident-token>] followed immediately by an
     *     `ArgumentDeclaration`, throw an error.
     *
     *     [<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
     *
     *   - Let `name` be `signature`'s <ident-token>.
     *
     *   - If there's already a global function whose name is
     *     underscore-insensitively equal to `name`, contineu to the next
     *     key/value pair.
     *
     *   - Otherwise, add a global function whose signature is `signature`. When
     *     this function is called:
     *
     *     - Let `result` be the result of calling the associated
     *       `CustomFunction` with the given arguments. If this call throws an
     *       error, treat it as a Sass error thrown by the Sass function.
     *
     *       > As in the rest of Sass, `_`s and `-`s are considered equivalent
     *       > when determining which function signatures match.
     *
     *     - Throw an error if `result` is or transitively contains:
     *
     *       - An object that's not an instance of the `Value` class.
     *
     *       - A `SassFunction` whose `signature` field isn't a valid Sass
     *         function signature that could appear after the `@function`
     *         directive in a Sass stylesheet.
     *
     *     - Return a copy of `result.internal` with all calculations it
     *       transitively contains (including the return value itself if it's a
     *       calculation) replaced with the result of [simplifying] those
     *       calculations.
     *
     *       [simplifying]: ../spec/types/calculation.md#simplifying-a-calculation
     */
    functions?: Record<string, CustomFunction<sync>>;
  }
}

/** The type of values that can be arguments to a `SassCalculation`. */
type CalculationValue =
  | SassNumber
  | SassCalculation
  | SassString
  | CalculationOperation
  | CalculationInterpolation;

/**
 * The JS API representation of a Sass [calculation].
 *
 * [calculation]: ../spec/types/calculation.md#types
 *
 * `internal` refers to a Sass calculation.
 */
export class SassCalculation extends Value {
  /**
   * Creates a value that represents `calc(argument)` expression.
   *
   * - If `argument` is or transitively contains a quoted `SassString`, throw an
   *   error.
   *
   * - Return a calculation with name `"calc"` and a `argument` as its single
   *   argument.
   */
  static calc(argument: CalculationValue): SassCalculation;

  /**
   * Creates a value that represents `min(...arguments)`.
   *
   * - If `argument` is or transitively contains a quoted `SassString`, throw an
   *   error.
   *
   * - Return a calculation with name `"min"` and `arguments` as its arguments.
   */
  static min(
    arguments: CalculationValue[] | List<CalculationValue>
  ): SassCalculation;

  /**
   * Creates a value that represents `min(...arguments)`.
   *
   * - If `arguments` transitively contains a quoted `SassString`, throw an
   *   error.
   *
   * - Return a calculation with name `"max"` and `arguments` as its arguments.
   */
  static max(
    arguments: CalculationValue[] | List<CalculationValue>
  ): SassCalculation;

  /**
   * Creates a value that represents `calc(min, value, max)` expression.
   *
   * - If `min`, `max`, or `clamp` is or transitively contains a quoted
   *   `SassString`, throw an error.
   *
   * - If `value` is undefined and `max` is not undefined, throw an error.
   *
   * - If `value` or `max` is undefined and neither `min` nor `value` is a
   *   `SassString` that begins with `"var("`, throw an error.
   *
   * - Return a calculation with name `"clamp"` and `min`, `value`, and `max` as
   *   its arguments, excluding any arguments that are undefined.
   */
  static clamp(
    min: CalculationValue,
    value?: CalculationValue,
    max?: CalculationValue
  ): SassCalculation;

  /** `internal`'s `name` field. */
  get name(): string;

  /** A list of `internal`'s arguments. */
  get arguments(): List<CalculationValue>;
}

/** The set of possible operators in a Sass calculation. */
type CalculationOperator = '+' | '-' | '*' | '/';

/**
 * The JS API representation of a Sass [CalculationOperation].
 *
 * [CalculationOperation]: ../spec/types/calculation.md#types
 *
 * `internal` refers to a Sass CalculationOperation.
 */
export abstract class CalculationOperation implements ValueObject {
  /**
   * Creates a Sass CalculationOperation by setting the fields to the arguments
   * of the corresponding names, and returns it.
   */
  constructor(
    operator: CalculationOperator,
    left: CalculationValue,
    right: CalculationValue
  );

  /** `internal`'s `operator` field. */
  get operator(): CalculationOperator;

  /** `internal`'s `left` field. */
  get left(): CalculationValue;

  /** `internal`'s `right` field. */
  get right(): CalculationValue;

  /** Whether `internal` is equal to `other.internal` in Sass. */
  equals(other: CalculationOperation): boolean;

  /** Must be the same for two equal values. */
  hashCode(): number;
}

/**
 * The JS API representation of a Sass [CalculationInterpolation].
 *
 * [CalculationInterpolation]: ../spec/types/calculation.md#types
 *
 * `internal` refers to a Sass CalculationInterpolation.
 *
 * Two `CalculationInterpolation`s are equal if their `value` fields are equal.
 */
export abstract class CalculationInterpolation implements ValueObject {
  /**
   * Creates a Sass CalculationInterpolation by setting the `value` field to the
   * argument of the corresponding name, and returns it.
   */
  constructor(value: string);

  /** `internal`'s `value` field. */
  get value(): string;

  /** Whether `internal` is equal to `other.internal` in Sass. */
  equals(other: CalculationOperation): boolean;

  /** Must be the same for two equal values. */
  hashCode(): number;
}
