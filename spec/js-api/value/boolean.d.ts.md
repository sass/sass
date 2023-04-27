# Boolean API

```ts
import {Value} from './index';
```

## Table of Contents

* [Fields](#fields)
  * [`sassTrue`](#sasstrue)
  * [`sassFalse`](#sassfalse)
* [Types](#types)
  * [`SassBoolean`](#sassboolean)
    * [`value`](#value)

## Fields

### `sassTrue`

A `Value` whose [`internal`] is the SassScript true value.

[`internal`]: index.d.ts.md#internal

```ts
export const sassTrue: SassBoolean;
```

### `sassFalse`

A `Value` whose [`internal`] is the SassScript false value.

```ts
export const sassFalse: SassBoolean;
```

## Types

### `SassBoolean`

The JS API representation of a Sass boolean.

```ts
export class SassBoolean extends Value {
  private constructor();
```

#### `value`

```ts
get value(): boolean;
```

```ts
} // SassBoolean
```
