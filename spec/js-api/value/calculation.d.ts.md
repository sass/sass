# Calculation API

```ts
import {List, ValueObject} from 'immutable';

import {Value, SassNumber, SassString} from './index';
```

## Table of Contents

* [Types](#types)
  * [`CalculationValue`](#calculationvalue)
  * [`SassCalculation`](#sasscalculation)
    * [`internal`](#internal)
    * [`calc`](#calc)
    * [`min`](#min)
    * [`max`](#max)
    * [`clamp`](#clamp)
    * [`name`](#name)
  * [`CalculationOperator`](#calculationoperator)
  * [`CalculationOperation`](#calculationoperation)
    * [`internal`](#internal-1)
    * [Constructor](#constructor)
    * [`operator`](#operator)
    * [`left`](#left)
    * [`right`](#right)
    * [`equals`](#equals)
    * [`hashCode`](#hashcode)
  * [`CalculationInterpolation`](#calculationinterpolation)
    * [`internal`](#internal-2)
    * [Constructor](#constructor-1)
    * [`value`](#value)
    * [`equals`](#equals-1)
    * [`hashCode`](#hashcode-1)

## Types

### `CalculationValue`

The type of values that can be arguments to a [`SassCalculation`].

```ts
export type CalculationValue =
  | SassNumber
  | SassCalculation
  | SassString
  | CalculationOperation
  | CalculationInterpolation;
```

### `SassCalculation`

The JS API representation of a Sass [calculation].

> Note: in the JS API calculations are not simplified eagerly. This also
> means that unsimplified calculations are not equal to the numbers they
> would be simplified to.

```ts
export class SassCalculation extends Value {
```

#### `internal`

The [private `internal` field] refers to a Sass [calculation].

[private `internal` field]: index.d.ts.md#internal
[calculation]: ../../types/calculation.md

#### `calc`

Creates a value that represents `calc(argument)`.

* If `argument` is or transitively contains a quoted `SassString`, throw an
  error.

* Return a calculation with name `"calc"` and `argument` as its single argument.

```ts
static calc(argument: CalculationValue): SassCalculation;
```

#### `min`

Creates a value that represents `min(...arguments)`.

* If `argument` is or transitively contains a quoted `SassString`, throw an
  error.

* Return a calculation with name `"min"` and `arguments` as its arguments.

```ts
static min(
  arguments: CalculationValue[] | List<CalculationValue>
): SassCalculation;
```

#### `max`

Creates a value that represents `max(...arguments)`.

* If `arguments` transitively contains a quoted `SassString`, throw an error.

* Return a calculation with name `"max"` and `arguments` as its arguments.

```ts
static max(
  arguments: CalculationValue[] | List<CalculationValue>
): SassCalculation;
```

#### `clamp`

Creates a value that represents `calc(min, value, max)` expression.

* If `min`, `max`, or `clamp` is or transitively contains a quoted `SassString`,
  throw an error.

* If `value` is undefined and `max` is not undefined, throw an error.

* If `value` or `max` is undefined and `min` is not a `SassString` or
  `CalculationInterpolation` that contains comma-separated values that can be
  interpreted as values for `value` and `max` (for example `clamp(#{"1, 2,
  3"})`).

* Return a calculation with name `"clamp"` and `min`, `value`, and `max` as its
  arguments, excluding any arguments that are undefined.

```ts
static clamp(
  min: CalculationValue,
  value?: CalculationValue,
  max?: CalculationValue
): SassCalculation;
```

#### `name`

Returns [`internal`]'s `name` field.

[`internal`]: #internal

```ts
get name(): string;
```

Returns a list of [`internal`]'s arguments.

```ts
get arguments(): List<CalculationValue>;
```

```ts
} // SassCalculation
```

### `CalculationOperator`

The set of possible operators in a Sass calculation.

```ts
export type CalculationOperator = '+' | '-' | '*' | '/';
```

### `CalculationOperation`

The JS API representation of a Sass [`CalculationOperation`].

[CalculationOperation]: ../../types/calculation.md#types

```ts
export class CalculationOperation implements ValueObject {
```

#### `internal`

A private property like [`Value.internal`] that refers to a Sass
[`CalculationOperation`].

[`Value.internal`]: index.d.ts.md

#### Constructor

Creates a Sass CalculationOperation by setting the fields to the arguments of
the corresponding names, and returns it.


```ts
constructor(
  operator: CalculationOperator,
  left: CalculationValue,
  right: CalculationValue
);
```

#### `operator`

Returns [`internal`][co-internal]'s `operator` field.

[co-internal]: #internal-1

```ts
get operator(): CalculationOperator;
```

#### `left`

Returns [`internal`][co-internal]'s `left` field.

```ts
get left(): CalculationValue;
```

#### `right`

Returns [`internal`][co-internal]'s `right` field.

```ts
get right(): CalculationValue;
```

#### `equals`

Whether [`internal`][co-internal] is equal to `other.internal` in Sass

```ts
equals(other: CalculationOperation): boolean;
```

#### `hashCode`

Returns the same number for any two `CalculationOperation`s that are equal
according to [`equals`](#equals).

```ts
hashCode(): number;
```

```ts
} // CalculationOperation
```

### `CalculationInterpolation`

The JS API representation of a Sass [`CalculationInterpolation`].

[`CalculationInterpolation`]: ../../types/calculation.md#types

```ts
export class CalculationInterpolation implements ValueObject {
```

#### `internal`

A private property like [`Value.internal`] that refers to a Sass
[`CalculationInterpolation`].

#### Constructor

Creates a Sass [`CalculationInterpolation`] by setting the `value` field to the
`value` argument and returns it.

```ts
constructor(value: string);
```

#### `value`

Returns [`internal`][ci-internal]'s `value` field.

[ci-internal]: #internal-2

```ts
get value(): string;
```

#### `equals`

Whether [`internal`][ci-internal] is equal to `other.internal` in Sass.

```ts
equals(other: CalculationInterpolation): boolean;
```

#### `hashCode`

Returns the same number for any two `CalculationInterpolation`s that are equal
according to [`equals`](#equals-1).

```ts
hashCode(): number;
```

```ts
} // CalculationInterpolation
```
