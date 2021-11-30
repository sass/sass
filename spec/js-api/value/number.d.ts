import {List} from 'immutable';

import {Value} from './index';

/**
 * The JS API representation of a Sass number.
 *
 * `internal` refers to a Sass number.
 */
export class SassNumber extends Value {
  /**
   * Creates a Sass number:
   *
   * - If the second argument is undefined:
   *
   *   - Set `internal` to a Sass number with a value of `value`.
   *
   * - Otherwise, if the second argument is a string:
   *
   *   - Set `internal` to a Sass number with a value of `value` and that string
   *     as its single numerator unit.
   *
   * - Otherwise,
   *
   *   - Let `options` be the second argument.
   *
   *   - Set `internal` to a Sass number with a value of `value`,
   *     `options.numeratorUnits` as its numerator units (if passed), and
   *     `options.denominatorUnits` as its denominator units (if passed).
   *
   * - Return `this`.
   */
  constructor(
    value: number,
    unit?:
      | string
      | {
          numeratorUnits?: string[] | List<string>;
          denominatorUnits?: string[] | List<string>;
        }
  );

  /** `internal`'s value. */
  get value(): number;

  /** Whether `internal`'s value `fuzzyEquals` an integer. */
  get isInt(): boolean;

  /**
   * Returns `internal`'s value as an integer:
   *
   * - If `internal`'s value `fuzzyEquals` an integer, return that integer.
   * - Otherwise, return `null`.
   */
  get asInt(): number | null;

  /** `internal`'s numerator units. */
  get numeratorUnits(): List<string>;

  /** `internal`'s denominator units. */
  get denominatorUnits(): List<string>;

  /** Whether `internal` has numerator or denominator units. */
  get hasUnits(): boolean;

  /**
   * Asserts that `internal`'s value is an integer:
   *
   * - If `internal`'s value `fuzzyEquals` an integer, return that integer.
   * - Otherwise, throw an error.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertInt(name?: string): number;

  /**
   * Asserts that `internal`'s value is within the specified range:
   *
   * - If `internal`'s value is `fuzzyGreaterThan` `min` and `fuzzyLessThan`
   *   `max`, return it.
   * - Otherwise, if `internal`'s value `fuzzyEquals` `min`, return `min`.
   * - Otherwise, if `internal`'s value `fuzzyEquals` `max`, return `max`.
   * - Otherwise, throw an error.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertInRange(min: number, max: number, name?: string): number;

  /**
   * Asserts that `internal` is unitless:
   *
   * - If `internal` has any numerator or denominator units, throw an error.
   * - Otherwise, return `this`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertNoUnits(name?: string): SassNumber;

  /**
   * Asserts the type of `internal`'s unit:
   *
   * - If `internal` has any denominator units, or if `unit` is not `internal`'s
   *   only numerator unit, throw an error.
   * - Otherwise, return `this`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  assertUnit(unit: string, name?: string): SassNumber;

  /**
   * Whether `internal` has the specified unit:
   *
   * - If `internal` has any denominator units, return false.
   * - Otherwise, return whether `unit` is `internal`'s only numerator unit.
   */
  hasUnit(unit: string): boolean;

  /**
   * Whether `internal` is [compatible] with `unit`.
   *
   * [compatible]: ../../types/number.md#compatible-units
   */
  compatibleWithUnit(unit: string): boolean;

  /**
   * Creates a new copy of `this` with its units converted to those represented
   * by `newNumerators` and `newDenominators`:
   *
   * - Let `converter` be the result of
   *
   *   ```
   *   withUnits(0, {
   *     numeratorUnits: newNumerators,
   *     denominatorUnits: newDenominators,
   *   });
   *   ```
   *
   * - If `converter` is not [compatible] with `internal`, throw an error.
   *
   * - Set `converter` to the result of `simplify`ing `converter`.
   *
   * - Return a new `SassNumber` with `internal` set to the result of the
   *   SassScript expression `converter + internal`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  convert(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): SassNumber;

  /**
   * Creates a new copy of `this` with its units converted to the units of
   * `other`:
   *
   * - Let `newNumerators` be the numerator units of `other`.
   * - Let `newDenominators` be the denominator units of `other`.
   * - Return the result of `convert(newNumerators, newDenominators)`.
   *
   * > The `name` and `otherName` parameters may be used for error reporting.
   */
  convertToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): SassNumber;

  /**
   * Return the value of the result of `convert(newNumerators,
   * newDenominators)`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  convertValue(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): number;

  /**
   * Returns the value of the result of `convertToMatch(other)`.
   *
   * > The `name` and `otherName` parameters may be used for error reporting.
   */
  convertValueToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): number;

  /**
   * Creates a new copy of `this` with its units converted to those represented
   * by `newNumerators` and `newDenominators`:
   *
   * - Support converting to/from unitless:
   *
   *   - If `internal` is unitless:
   *
   *     - If `newNumerators` and `newDenominators` are both empty, return
   *       `this`.
   *
   *     - Otherwise, for the duration of this procedure, let `internal` behave
   *       as if its numerator units were equal to `newNumerators` and its
   *       denominator units were equal to `newDenominators`.
   *
   *   - Otherwise, if `newNumerators` and `newDenominators` are both empty, set
   *     `newNumerators` to `internal`'s numerator units and `newDenominators`
   *     to `internal`'s denominator units.
   *
   * - Return the result of `convert(newNumerators, newDenominators)`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  coerce(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): SassNumber;

  /**
   * Creates a new copy of `this` with its units converted to the units of
   * `other`:
   *
   * - Let `newNumerators` be the numerator units of `other`.
   * - Let `newDenominators` be the denominator units of `other`.
   * - Return the result of `coerce(newNumerators, newDenominators)`.
   *
   * > The `name` and `otherName` parameters may be used for error reporting.
   */
  coerceToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): SassNumber;

  /**
   * Return the value of the result of `coerce(newNumerators, newDenominators)`.
   *
   * > The `name` parameter may be used for error reporting.
   */
  coerceValue(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): number;

  /**
   * Returns the value of the result of `coerceToMatch(other)`.
   *
   * > The `name` and `otherName` parameters may be used for error reporting.
   */
  coerceValueToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): number;
}
