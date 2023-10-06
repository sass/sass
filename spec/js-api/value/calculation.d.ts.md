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

* If `argument` is a quoted `SassString`, throw an error.

* Return a calculation with name `"calc"` and `argument` as its single argument.

```ts
static calc(argument: CalculationValue): SassCalculation;
```

#### `min`

Creates a value that represents `min(...arguments)`.

* If `argument` contains a quoted `SassString`, throw an error.

* Return a calculation with name `"min"` and `arguments` as its arguments.

```ts
static min(
  arguments: CalculationValue[] | List<CalculationValue>
): SassCalculation;
```

#### `max`

Creates a value that represents `max(...arguments)`.

* If `arguments` contains a quoted `SassString`, throw an error.

* Return a calculation with name `"max"` and `arguments` as its arguments.

```ts
static max(
  arguments: CalculationValue[] | List<CalculationValue>
): SassCalculation;
```

#### `clamp`

Creates a value that represents `calc(min, value, max)` expression.

* If `min`, `max`, or `clamp` is a quoted `SassString`, throw an error.

* If `value` is undefined and `max` is not undefined, throw an error.

* If either `value` or `max` is undefined and neither `min` nor `value` is a
  `SassString` or `CalculationInterpolation`, throw an error.

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

```ts
export class CalculationOperation implements ValueObject {
```

#### `internal`

A private property like [`Value.internal`] that refers to a Sass
[`CalculationOperation`].

[`Value.internal`]: index.d.ts.md

#### Constructor

Creates a Sass `CalculationOperation`:

* Throw an error if `left` or `right` is a quoted `SassString`.
* Set the fields to the arguments of the corresponding names.
* Return the resulting `CalculationOperation`.

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
equals(other: unknown): boolean;
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

A deprecated alternative JS API representation of an unquoted Sass string that's
always surrounded by parentheses. It's never returned by the Sass compiler, but
for backwards-compatibility users may still construct it and pass it to the Sass
compiler.

> `CalculationInterpolation`s are no longer generated by the Sass compiler,
> because it can now tell at evaluation time whether an interpolation was
> originally surrounded by parentheses. However, until we make a breaking
> revision of the JS API, users may continue to pass `CalculationInterpolation`s

```ts
export class CalculationInterpolation implements ValueObject {
```

#### `internal`

A private property like [`Value.internal`] that refers to a Sass string.

#### Constructor

Creates a `CalculationInterpolation` with `internal` set to an unquoted Sass
string with text `"(" + value + ")"` and returns it.

```ts
constructor(value: string);
```

#### `value`

Returns [`internal`][ci-internal]'s `value` field's text, without the leading
and trailing parentheses.

[ci-internal]: #internal-1

```ts
get value(): string;
```

#### `equals`

Whether `other` is a `CalculationInterpolation` and [`internal`][ci-internal] is
equal to `other.internal` in Sass.

```ts
equals(other: unknown): boolean;
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
