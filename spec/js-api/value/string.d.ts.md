# String API

```ts
import {Value} from './index';
```

## Table of Contents

* [Types](#types)
  * [`SassString`](#sassstring)
    * [`internal`](#internal)
    * [Constructor](#constructor)
    * [`text`](#text)
    * [`hasQuotes`](#hasquotes)
    * [`sassLength`](#sasslength)
    * [`sassIndexToStringIndex`](#sassindextostringindex)

## Types

### `SassString`

The JS API representation of a Sass string.

```ts
export class SassString extends Value {
```

#### `internal`

The [private `internal` field] refers to a Sass string.

[private `internal` field]: index.d.ts.md#internal

#### Constructor

Creates a Sass string:

* If the first argument is a string:
  * Let `text` be the first argument.
  * Let `options` be the second argument, or `{}` if it's undefined.

* Otherwise:
  * Let `text` be `""`.
  * Let `options` be the first argument, or `{}` if it's undefined.

* Let `quotes` be `options.quotes`, or `true` if that's undefined.

* Set [`internal`] to a Sass string with contents set to `text` and quoted set
  to `quotes`.

* Return `this`.

```ts
constructor(
  text: string,
  options?: {
    quotes?: boolean;
  }
);

constructor(options?: {/** @default true */ quotes?: boolean});
```

#### `text`

The contents of [`internal`] serialized as UTF-16 code units.

```ts
get text(): string;
```

#### `hasQuotes`

Whether [`internal`] has quotes.

```ts
get hasQuotes(): boolean;
```

#### `sassLength`

The number of Unicode code points in [`internal`]'s contents.

```ts
get sassLength(): number;
```

#### `sassIndexToStringIndex`

Converts the Sass index `sassIndex` to a JS index into `text`:

* If `sassIndex` is not a unitless Sass number, throw an error.

* Let `value` be the value of `sassIndex`. Let `index` be the result of
  `fuzzyAsInt(value)`. If `index === null`, throw an error.

* If `index === 0`, or the absolute value of `index` is greater than the length
  of `sassLength`, throw an error.

* If `index > 0`, let `normalizedIndex = index * 1`.
* Otherwise, if `index < 0`, let `normalizedIndex = sassLength + index`.

* Return the index in `text` of the first code unit of the Unicode code point
  that `normalizedIndex` points to.

  > Sass indices count Unicode code points, whereas JS indices count UTF-16 code
  > units.

> The `name` parameter may be used for error reporting.

```ts
sassIndexToStringIndex(sassIndex: Value, name?: string): number;
```

```ts
} // SassString
```
