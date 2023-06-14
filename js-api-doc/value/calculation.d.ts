import {List, ValueObject} from 'immutable';
import {Value, SassNumber, SassString} from './index';

/**
 * The type of values that can be arguments to a {@link SassCalculation}.
 * @category Custom Function
 * */
export type CalculationValue =
  | SassNumber
  | SassCalculation
  | SassString
  | CalculationOperation
  | CalculationInterpolation;

/**
 * Sass's [calculation
 * type](https://sass-lang.com/documentation/values/calculations).
 *
 * Note: in the JS API calculations are not simplified eagerly. This also means
 * that unsimplified calculations are not equal to the numbers they would be
 * simplified to.
 *
 * @category Custom Function
 */
export class SassCalculation extends Value {
  /**
   * Creates a value that represents `calc(argument)`.
   *
   * @throws `Error` if `argument` is or transitively contains a quoted
   * {@link SassString}
   * @returns A calculation with the name `calc` and `argument` as its single
   * argument.
   */
  static calc(argument: CalculationValue): SassCalculation;

  /**
   * Creates a value that represents `min(arguments...)`.
   *
   * @throws `Error` if any of `arguments` are or transitively contain a quoted
   * {@link SassString}
   * @returns A calculation with the name `min` and `arguments` as its arguments.
   */
  static min(
    arguments: CalculationValue[] | List<CalculationValue>
  ): SassCalculation;

  /**
   * Creates a value that represents `max(arguments...)`.
   *
   * @throws `Error` if any of `arguments` are or transitively contain a quoted
   * {@link SassString}
   * @returns A calculation with the name `max` and `arguments` as its arguments.
   */
  static max(
    arguments: CalculationValue[] | List<CalculationValue>
  ): SassCalculation;

  /**
   * Creates a value that represents `clamp(value, min, max)`.
   *
   * @throws `Error` if any of `value`, `min`, or `max` are or transitively
   * contain a quoted {@link SassString}.
   * @throws `Error` if `value` is undefined and `max` is not undefined.
   * @throws `Error` if `value` or `max` is undefined and `min` is not a
     {@link SassString} or {@link CalculationInterpolation} that contains
     comma-separated values that can be interpreted as values for `value` and
     `max` (for example `clamp(#{"1, 2, 3"})`).
     @returns A calculation with the name `clamp` and `min`, `value`, and `max`
     as it's arguments, excluding any arguments that are undefined.
   */
  static clamp(
    min: CalculationValue,
    value?: CalculationValue,
    max?: CalculationValue
  ): SassCalculation;

  /** Returns the calculation's `name` field. */
  get name(): string;

  /** Returns a list of the calculation's `arguments` */
  get arguments(): List<CalculationValue>;
}

/**
 * The set of possible operators in a Sass calculation.
 * @category Custom Function
 */
export type CalculationOperator = '+' | '-' | '*' | '/';

/**
 * A binary operation that can appear in a {@link SassCalculation}.
 * @category Custom Function
 */
export class CalculationOperation implements ValueObject {
  /**
   * Creates a Sass CalculationOperation by setting the fields to the arguments
   * of the corresponding names, and returns it.
   */
  constructor(
    operator: CalculationOperator,
    left: CalculationValue,
    right: CalculationValue
  );

  /** Returns the operation's `operator` field. */
  get operator(): CalculationOperator;

  /** Returns the operation's `left` field. */
  get left(): CalculationValue;

  /** Returns the operation's `right` field. */
  get right(): CalculationValue;

  equals(other: CalculationOperation): boolean;

  hashCode(): number;
}

/**
 * A string injected into a {@link SassCalculation} using interpolation.
 * @category Custom Function
 */
export class CalculationInterpolation implements ValueObject {
  /**
   * Creates a Sass CalculationInterpolation by setting the value field to the
   * value argument and returns it.
   */
  constructor(value: string);

  /**
   * Returns the interpolation's `value` field.
   */
  get value(): string;

  equals(other: CalculationInterpolation): boolean;

  hashCode(): number;
}
