/**
 * # JavaScript Calculation API: Draft 1
 *
 * *([Issue](https://github.com/sass/sass/issues/818))*
 *
 * ## Background
 *
 * > This section is non-normative.
 *
 * This proposal simply exposes the [calculation type] to the JavaScript API.
 *
 * [calculation type]: ../accepted/first-class-calc.md
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
   * - Let `calculation` be a calculation with name `"calc"` and a `argument` as
   *   its single argument.
   *
   * - Return the result of [simplifying] `calculation`.
   *
   *   [simplifying]: ../spec/types/calculation.md#simplifying-a-calculation
   */
  static calc(argument: CalculationValue): SassNumber | SassCalculation;

  /**
   * Creates a value that represents `min(...arguments)`.
   *
   * - If `argument` is or transitively contains a quoted `SassString`, throw an
   *   error.
   *
   * - Let `calculation` be a calculation with name `"min"` and `arguments` as
   *   its arguments.
   *
   * - Return the result of [simplifying] `calculation`.
   *
   *   [simplifying]: ../spec/types/calculation.md#simplifying-a-calculation
   */
  static min(
    arguments: CalculationValue[] | List<CalculationValue>
  ): SassNumber | SassCalculation;

  /**
   * Creates a value that represents `min(...arguments)`.
   *
   * - If `arguments` transitively contains a quoted `SassString`, throw an
   *   error.
   *
   * - Let `calculation` be a calculation with name `"max"` and `arguments` as
   *   its arguments.
   *
   * - Return the result of [simplifying] `calculation`.
   *
   *   [simplifying]: ../spec/types/calculation.md#simplifying-a-calculation
   */
  static max(
    arguments: CalculationValue[] | List<CalculationValue>
  ): SassNumber | SassCalculation;

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
   * - Let `calculation` be a calculation with name `"clamp"` and `min`,
   *   `value`, and `max` as its arguments, excluding any arguments that are
   *   undefined.
   *
   * - Return the result of [simplifying] `calculation`.
   *
   *   [simplifying]: ../spec/types/calculation.md#simplifying-a-calculation
   */
  static clamp(
    min: CalculationValue,
    value?: CalculationValue,
    max?: CalculationValue
  ): SassNumber | SassCalculation;

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
