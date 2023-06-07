import { List, ValueObject } from 'immutable'
import { Value, SassNumber, SassString } from './index';

/** The type of values that can be arguments to a SassCalculation. */
type CalculationValue =
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
   * @throws `Error` if `value` or `max` is undefined and neither `min` nor
     `value` is a {@link SassString} that begins with `var(`
     @returns A calculation with the name `clamp` and `min`, `value`, and `max`
     as it's arguments, excluding any arguments that are undefined.
   */
  static clamp(
    min: CalculationValue,
    value?: CalculationValue,
    max?: CalculationValue
  ): SassCalculation;

  /** Returns internal's `name` field. */
  get name(): string;

  /** Returns a list of internal's `arguments` */
  get arguments(): List<CalculationValue>;
}

/**
 * The set of possible operators in a Sass calculation.
*/
type CalculationOperator = '+' | '-' | '*' | '/';

/**
 * A binary operation that can appear in a {@link SassCalculation}.
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

  /** Returns internal's `operator` field. */
  get operator(): CalculationOperator;

  /** Returns internal's `left` field. */
  get left(): CalculationValue;

  /** Returns internal's `right` field. */
  get right(): CalculationValue;

  /** Whether internal is equal to `other.internal` in Sass. */
  equals(other: CalculationOperation): boolean;

  /** Returns the same number for any two `CalculationOperation`s that are equal
   * according to `equals`.
  */
  hashCode(): number;
}

/**
 * A string injected into a {@link SassCalculation} using interpolation.
*/
export abstract class CalculationInterpolation implements ValueObject {
  /**
   * Creates a Sass CalculationInterpolation by setting the value field to the
   * value argument and returns it.
  */
  constructor(value: string);

  /**
   * Return's internal's value field.
  */
  get value(): string;

  /**
   * Whether internal is equal to `other.internal` in Sass.
  */
  equals(other: CalculationInterpolation): boolean;

  /**
   * Returns the same number for any two `CalculationInterpolation`s that are
   * equal according to `equals`.
  */
  hashCode(): number;
}
