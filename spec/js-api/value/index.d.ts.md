# Value API

```ts
import {List, ValueObject} from 'immutable';

import {SassBoolean} from './boolean';
import {SassCalculation} from './calculation';
import {SassColor} from './color';
import {SassFunction} from './function';
import {ListSeparator} from './list';
import {SassMap} from './map';
import {SassNumber} from './number';
import {SassString} from './string';

export {SassArgumentList} from './argument_list';
export {SassBoolean, sassTrue, sassFalse} from './boolean';
export {
  SassCalculation,
  CalculationValue,
  CalculationOperator,
  CalculationOperation,
  CalculationInterpolation
} from './calculation';
export {SassColor} from './color';
export {SassFunction} from './function';
export {SassList, ListSeparator} from './list';
export {SassMap} from './map';
export {SassNumber} from './number';
export {SassString} from './string';
```

## Table of Contents

* [Fields](#fields)
  * [`sassNull`](#sassnull)
* [Types](#types)
  * [`Value`](#value)
    * [`internal`](#internal)
    * [`asList`](#aslist)
    * [`hasBrackets`](#hasbrackets)
    * [`isTruthy`](#istruthy)
    * [`realNull`](#realnull)
    * [`separator`](#separator)
    * [`sassIndexToListIndex`](#sassindextolistindex)
    * [`get`](#get)
    * [`assertBoolean`](#assertboolean)
    * [`assertCalculation`](#assertcalculation)
    * [`assertColor`](#assertcolor)
    * [`assertFunction`](#assertfunction)
    * [`assertMap`](#assertmap)
    * [`assertNumber`](#assertnumber)
    * [`assertString`](#assertstring)
    * [`tryMap`](#trymap)
    * [`equals`](#equals)
    * [`hashCode`](#hashcode)
    * [`toString`](#tostring)

## Fields

### `sassNull`

A `Value` whose [`internal`] is the SassScript null value.

[`internal`]: #internal

```ts
export const sassNull: Value;
```

## Types

### `Value`

The JS API representation of a Sass value.

Sass values are immutable. Therefore, all subclasses of Value must have an API
that obeys immutability. Their APIs must not expose ways to modify Sass values,
including lists and maps. An API call that returns a new copy of a Sass value
must ensure that the copy preserves the metadata of the original value (e.g.
units).

```ts
export abstract class Value implements ValueObject {
  protected constructor();
```

#### `internal`

To make the spec terser and easier to author, each `Value` instance has a
private property named `internal` that refers to the Sass value it represents.
This property is only used for spec purposes and is not visible in any sense to
JavaScript.

#### `asList`

Returns `this` as an array:

* If [`internal`] is a Sass list, return an array of its contents.
* If [`internal`] is a Sass map, return an array of its keys and values as
  two-element `SassList`s.
* Otherwise, return a list containing `this`.

```ts
get asList(): List<Value>;
```

#### `hasBrackets`

Whether [`internal`] is a bracketed Sass list.

```ts
get hasBrackets(): boolean;
```

#### `isTruthy`

Whether `this` is truthy.

```ts
get isTruthy(): boolean;
```

#### `realNull`

Returns JS null if [`internal`] is Sass null. Otherwise, returns `this`.

```ts
get realNull(): null | Value;
```

#### `separator`

Return [`internal`]'s separator if it's a Sass list, and `null` otherwise.

```ts
get separator(): ListSeparator;
```

#### `sassIndexToListIndex`

Converts the Sass index `sassIndex` to a JS index into the array returned by
`asList`:

- If `sassIndex` is not a unitless Sass number, throw an error.

- Let `value` be the value of `sassIndex`. Let `index` be the result of
  `fuzzyAsInt(value)`. If `index === null`, throw an error.

- If `index === 0`, or the absolute value of `index` is greater than
  `asList.length`, throw an error.

- If `index > 0`, return `index - 1`.
- Otherwise, if `index < 0`, return `asList.length + index`.

  > Sass indices start counting at 1, and may be negative in order to index from
  > the end of the list.

> The `name` parameter may be used for error reporting.

```ts
sassIndexToListIndex(sassIndex: Value, name?: string): number;
```

#### `get`

Returns `this.asList.get(index)`.

> Note that the `immutable` package uses zero-based indexing, with negative
> numbers indexing backwards from the end of the list. Non-integer indices are
> rounded down.

```ts
get(index: number): Value | undefined;
```

#### `assertBoolean`

Returns `this` if it's a [`SassBoolean`] and throws an error otherwise.

[`SassBoolean`]: boolean.d.ts.md

> The `name` parameter may be used for error reporting.

```ts
assertBoolean(name?: string): SassBoolean;
```

#### `assertCalculation`

Returns `this` if it's a [`SassCalculation`] and throws an error otherwise.

[`SassCalculation`]: calculation.d.ts.md

> The `name` parameter may be used for error reporting.

```ts
assertCalculation(name?: string): SassCalculation;
```

#### `assertColor`

Returns `this` if it's a [`SassColor`] and throws an error otherwise.

[`SassColor`]: color.d.ts.md

> The `name` parameter may be used for error reporting.

```ts
assertColor(name?: string): SassColor;
```

#### `assertFunction`

Returns `this` if it's a [`SassFunction`] and throws an error otherwise.

[`SassFunction`]: function.d.ts.md

> The `name` parameter may be used for error reporting.

```ts
assertFunction(name?: string): SassFunction;
```

#### `assertMap`

Return `this.tryMap()` if it's not null, and throw an error otherwise.

> The `name` parameter may be used for error reporting.

```ts
assertMap(name?: string): SassMap;
```

#### `assertNumber`

Returns `this` if it's a [`SassNumber`] and throws an error otherwise.

[`SassNumber`]: number.d.ts.md

> The `name` parameter may be used for error reporting.

```ts
assertNumber(name?: string): SassNumber;
```

#### `assertString`

Returns `this` if it's a [`SassString`] and throws an error otherwise.

[`SassString`]: string.d.ts.md

> The `name` parameter may be used for error reporting.

```ts
assertString(name?: string): SassString;
```

#### `tryMap`

Returns `this` interpreted as a map.

* If `this` is a [`SassMap`], return `this`.

* Otherwise, if [`internal`] is an empty Sass list, return a `SassMap` with its
  `internal` set to an empty map.

* Otherwise, return `null`.

```ts
tryMap(): SassMap | null;
```

#### `equals`

Returns whether [`internal`] is `==` to `other`'s `internal` in SassScript.

```ts
equals(other: Value): boolean;
```

#### `hashCode`

Returns the same number for any two `Value`s that are equal according to
[`equals`].

[`equals`]: #equals

> This is _not_ required to be different for different values, although having
> overlap between common values is likely to cause performance issues.

```ts
hashCode(): number;
```

#### `toString`

Returns a string representation of `this`.

> The specific format can vary from implementation to implementation and is not
> guaranteed to be valid Sass source code.

```ts
toString(): string;
```

```ts
} // Value
```
