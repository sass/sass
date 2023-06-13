# JavaScript Calculation API: Draft 2

*([Issue](https://github.com/sass/sass/issues/818),
[Changelog](calculation-api.changes.md))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Simplification](#simplification)
* [API](#api)
* [Types](#types)
  * [`Value`](#value)
    * [`assertCalculation`](#assertcalculation)
  * [`Options`](#options)
    * [`functions`](#functions)
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

## Background

> This section is non-normative.

This proposal simply exposes the [calculation type] to the JavaScript API.

[calculation type]: ../accepted/first-class-calc.md

## Summary

> This section is non-normative.

### Design Decisions

#### Simplification

We considered eagerly simplifying calculations as they were constructed to
match the behavior of values in Sass itself. However, this poses a problem
for API implementations that don't have direct access to compiler logic, such
as the Node.js embedded host: they would need to implement the simplification
logic locally, which is relatively complex and opens a broad surface area for
subtle cross-implementation incompatibilities.

This could potentially be solved by adding an explicit request to the
embedded protocol, but this would pose its own problems given that JS is
strict about separating asynchronous calls (like those across process
boundaries) and synchronous calls (like this API).

Given that, we chose instead to handle simplification only at the custom
function boundary rather than when a calculation is constructed.

## API

```ts
import {List, ValueObject} from 'immutable';

import {Value, SassNumber, SassString} from '../spec/js-api/value';
```

## Types

### `Value`

```ts
declare module '../spec/js-api/value' {
  interface Value {
```

#### `assertCalculation`

Returns `this` if it's a [`SassCalculation`] and throws an error otherwise.

[`SassCalculation`]: #sasscalculation

> The `name` parameter may be used for error reporting.

```ts
assertCalculation(name?: string): SassCalculation;
```

```ts
  } // Value
} // module
```

### `Options`

```ts
declare module '../spec/js-api/options' {
  interface Options<sync extends 'sync' | 'async'> {
```

#### `functions`

Replace this option's specification with:

Before beginning compilation:

* For each key/value pair `signature`/`function` in this record:

  * If `signature` isn't an [<ident-token>] followed immediately by an
    `ArgumentDeclaration`, throw an error.

  * Let `name` be `signature`'s <ident-token>.

  * If there's already a global function whose name is
    underscore-insensitively equal to `name`, continue to the next
    key/value pair.

  * Otherwise, add a global function whose signature is `signature`. When
    this function is called:

    * Let `result` be the result of calling the associated
      `CustomFunction` with the given arguments. If this call throws an
      error, treat it as a Sass error thrown by the Sass function.

      > As in the rest of Sass, `_`s and `-`s are considered equivalent
      > when determining which function signatures match.

    * Throw an error if `result` is or transitively contains:

      * An object that's not an instance of the `Value` class.

      * A [`SassFunction`] whose `signature` field isn't a valid Sass
        function signature that could appear after the `@function`
        directive in a Sass stylesheet.

    * Return a copy of `result.internal` with all calculations it
      transitively contains (including the return value itself if it's a
      calculation) replaced with the result of [simplifying] those
      calculations.

[<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
[`SassFunction`]: ../spec/js-api/value/function.d.ts.md
[simplifying]: ../spec/types/calculation.md#simplifying-a-calculation

```ts
functions?: Record<string, CustomFunction<sync>>;
```

```ts
  } // Options
} // module
```

### `CalculationValue`

The type of values that can be arguments to a [`SassCalculation`].

```ts
type CalculationValue =
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

[private `internal` field]: ../spec/js-api/value/index.d.ts.md#internal
[calculation]: ../spec/types/calculation.md

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

* If `value` or `max` is undefined and neither `min` nor `value` is a
  `SassString` that begins with `"var("`, throw an error.

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
type CalculationOperator = '+' | '-' | '*' | '/';
```

### `CalculationOperation`

The JS API representation of a Sass [`CalculationOperation`].

[CalculationOperation]: ../spec/types/calculation.md#types

```ts
export abstract class CalculationOperation implements ValueObject {
```

#### `internal`

A private property like [`Value.internal`] that refers to a Sass
[`CalculationOperation`].

[`Value.internal`]: ../spec/js-api/value/index.d.ts.md

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

[`CalculationInterpolation`]: ../spec/types/calculation.md#types

```ts
export abstract class CalculationInterpolation implements ValueObject {
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
equals(other: CalculationOperation): boolean;
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
