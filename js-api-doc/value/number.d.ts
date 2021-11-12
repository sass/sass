import {List} from 'immutable';

import {Value} from './index';

export class SassNumber extends Value {
  constructor(value: number, unit?: string);

  constructor(
    value: number,
    options?: {
      numeratorUnits?: string[] | List<string>;
      denominatorUnits?: string[] | List<string>;
    }
  );

  get value(): number;

  get isInt(): boolean;

  get asInt(): number | null;

  get numeratorUnits(): List<string>;

  get denominatorUnits(): List<string>;

  get hasUnits(): boolean;

  assertInt(name?: string): number;

  assertInRange(min: number, max: number, name?: string): number;

  assertNoUnits(name?: string): SassNumber;

  assertUnit(unit: string, name?: string): SassNumber;

  hasUnit(unit: string): boolean;

  compatibleWithUnit(unit: string): boolean;

  convert(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): SassNumber;

  convertToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): SassNumber;

  convertValue(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): number;

  convertValueToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): number;

  coerce(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): SassNumber;

  coerceToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): SassNumber;

  coerceValue(
    newNumerators: string[] | List<string>,
    newDenominators: string[] | List<string>,
    name?: string
  ): number;

  coerceValueToMatch(
    other: SassNumber,
    name?: string,
    otherName?: string
  ): number;
}
