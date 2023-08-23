# Number API

```ts
import {List} from 'immutable';

import {Value} from './index';
```

## Table of Contents

* [Types](#types)
  * [`SassNumber`](#sassnumber)
    * [`internal`](#internal)
    * [Constructor](#constructor)
    * [`value`](#value)
    * [`isInt`](#isint)
    * [`asInt`](#asint)
    * [`numeratorUnits`](#numeratorunits)
    * [`denominatorUnits`](#denominatorunits)
    * [`hasUnits`](#hasunits)
    * [`assertInt`](#assertint)
    * [`assertInRange`](#assertinrange)
    * [`assertUnitless`](#assertunitless)
    * [`assertUnit`](#assertunit)
    * [`hasUnit`](#hasunit)
    * [`compatibleWithUnit`](#compatiblewithunit)
    * [`convert`](#convert)
    * [`convertToMatch`](#converttomatch)
    * [`convertValue`](#convertvalue)
    * [`convertValueToMatch`](#convertvaluetomatch)
    * [`coerce`](#coerce)
    * [`coerceToMatch`](#coercetomatch)
    * [`coerce`](#coerce-1)
    * [`coerceValueToMatch`](#coercevaluetomatch)

## Types

### `SassNumber`

The JS API representation of a Sass number.

```ts
export class SassNumber extends Value {
```

#### `internal`

The [private `internal` field] refers to [a Sass number].

[private `internal` field]: index.d.ts.md#internal
[a Sass number]: ../../types/number.md

#### Constructor

Creates a Sass number:

* If the second argument is undefined:

  * Set `internal` to a Sass number with a value of `value`.

* Otherwise, if the second argument is a string:

  * Set `internal` to a Sass number with a value of `value` and that string as
    its single numerator unit.

* Otherwise,

  * Let `options` be the second argument.

  * Set `internal` to a Sass number with a value of `value`,
    `options.numeratorUnits` as its numerator units (if passed), and
    `options.denominatorUnits` as its denominator units (if passed).

* Return `this`.

```ts
constructor(
  value: number,
  unit?:
    | string
    | {
        numeratorUnits?: string[] | List<string>;
        denominatorUnits?: string[] | List<string>;
      }
);
```

#### `value`

Returns [`internal`]'s value.

[`internal`]: #internal

```ts
get value(): number;
```

#### `isInt`

Whether [`internal`] is an [integer].

[integer]: ../../types/number.md#integer

```ts
get isInt(): boolean;
```

#### `asInt`

Returns [`internal`]'s [integer value] if it has one, or null if it doesn't.

[integer value]: ../../types/number.md#integer

```ts
get asInt(): number | null;
```

#### `numeratorUnits`

Returns [`internal`]'s numerator units.

```ts
get numeratorUnits(): List<string>;
```

#### `denominatorUnits`

Returns [`internal`]'s denominator units.

```ts
get denominatorUnits(): List<string>;
```

#### `hasUnits`

Whether [`internal`] has numerator or denominator units.

```ts
get hasUnits(): boolean;
```

#### `assertInt`

Returns [`internal`]'s [integer value] if it has one, and throws an error if it
doesn't.

> The `name` parameter may be used for error reporting.

```ts
assertInt(name?: string): number;
```

#### `assertInRange`

Asserts that [`internal`]'s value is within the specified range:

* If `internal`'s value is greater than `min` and less than `max`, return it.
* Otherwise, if `internal`'s value [fuzzy equals] `min`, return `min`.
* Otherwise, if `internal`'s value fuzzy equals `max`, return `max`.
* Otherwise, throw an error.

[fuzzy equals]: ../../types/number.md#fuzzy-equality

> The `name` parameter may be used for error reporting.

```ts
assertInRange(min: number, max: number, name?: string): number;
```

#### `assertUnitless`

Returns `this` if [`internal`] has no numerator or denominator units, and throws
an error otherwise.

> The `name` parameter may be used for error reporting.

```ts
assertNoUnits(name?: string): SassNumber;
```

#### `assertUnit`

Asserts the type of [`internal`]'s unit:

* If `internal` has any denominator units, or if `unit` is not `internal`'s
  only numerator unit, throw an error.
* Otherwise, return `this`.

> The `name` parameter may be used for error reporting.

```ts
assertUnit(unit: string, name?: string): SassNumber;
```

#### `hasUnit`

Returns whether `unit` is [`internal`]'s only numerator unit and `internal` has no
denominator units.

```ts
hasUnit(unit: string): boolean;
```

#### `compatibleWithUnit`

Whether `internal` is [compatible] with `unit`.

[compatible]: ../../types/number.md#compatible-units

```ts
compatibleWithUnit(unit: string): boolean;
```

#### `convert`

* Let `converter` be the [`internal`] field of the result of

  ```js
  withUnits(0, {
    numeratorUnits: newNumerators,
    denominatorUnits: newDenominators,
  });
  ```

* If `converter` is not [compatible] with `internal`, throw an error.

* Set `converter` to the result of [simplifying] `converter`.

  [simplifying]: ../../types/number.md#simplifying-a-number

* Return a new `SassNumber` with `internal` set to the result of the
  SassScript expression `converter + internal`.

> The `name` parameter may be used for error reporting.

```ts
convert(
  newNumerators: string[] | List<string>,
  newDenominators: string[] | List<string>,
  name?: string
): SassNumber;
```

#### `convertToMatch`

Return the result of `convert(other.numeratorUnits, other.denominatorUnits)`.

> The `name` and `otherName` parameters may be used for error reporting.

```ts
convertToMatch(
  other: SassNumber,
  name?: string,
  otherName?: string
): SassNumber;
```

#### `convertValue`

Return the result of `convert(newNumerators, newDenominators).value`.

> The `name` parameter may be used for error reporting.

```ts
convertValue(
  newNumerators: string[] | List<string>,
  newDenominators: string[] | List<string>,
  name?: string
): number;
```

#### `convertValueToMatch`

Returns the result of `convertToMatch(other).value`.

> The `name` and `otherName` parameters may be used for error reporting.

```ts
convertValueToMatch(
  other: SassNumber,
  name?: string,
  otherName?: string
): number;
```

#### `coerce`

Creates a new copy of `this` with its units converted to those represented
by `newNumerators` and `newDenominators`:

* If `newNumerators` and `newDenominators` are both empty, return the result of
  `new SassNumber(this.value)`.

* If `internal` is [unitless], return the result of:

  [unitless]: ../../types/number.md#

    ```js
    new SassNumber(this.value, {
      numeratorUnits: newNumerators,
      denominatorUnits: newDenominators
    });
    ```

* Return the result of `convert(newNumerators, newDenominators)`.

> The `name` parameter may be used for error reporting.

```ts
coerce(
  newNumerators: string[] | List<string>,
  newDenominators: string[] | List<string>,
  name?: string
): SassNumber;
```

#### `coerceToMatch`

Return the result of `coerce(other.numeratorUnits, other.denominatorUnits)`.

> The `name` and `otherName` parameters may be used for error reporting.

```ts
coerceToMatch(
  other: SassNumber,
  name?: string,
  otherName?: string
): SassNumber;
```

#### `coerce`

Return the result of `coerce(newNumerators, newDenominators).value`.

> The `name` parameter may be used for error reporting.

```ts
coerceValue(
  newNumerators: string[] | List<string>,
  newDenominators: string[] | List<string>,
  name?: string
): number;
```

#### `coerceValueToMatch`

Returns the value of the result of `coerceToMatch(other)`.

> The `name` and `otherName` parameters may be used for error reporting.

```ts
coerceValueToMatch(
  other: SassNumber,
  name?: string,
  otherName?: string
): number;
```

```ts
} // SassNumber
```
