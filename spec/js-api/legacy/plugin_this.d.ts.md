# Legacy Plugin Context

The shared interface for the `this` keyword for custom importers and custom
functions. The implementation must invoke importers and custom functions with an
appropriate `this`.

## Table of Contents

* [Types](#types)
  * [`LegacyPluginThis`](#legacypluginthis)
    * [`context`](#context)
    * [`file`](#file)
    * [`data`](#data)
    * [`includePaths`](#includepaths)
    * [`precision`](#precision)
    * [`style`](#style)
    * [`indentType`](#indenttype)
    * [`indentWidth`](#indentwidth)
    * [`linefeed`](#linefeed)
    * [`result`](#result)
      * [`result.stats.start`](#resultstatsstart)
      * [`result.stats.entry`](#resultstatsentry)

## Types

### `LegacyPluginThis`

This class contains a single field, `options`, which contains all its metadata.

```ts
export interface LegacyPluginThis {
  options: {
```

#### `context`

The same `LegacyPluginThis` instance that contains the `options` object.

```ts
context: LegacyPluginThis;
```

#### `file`

The [`file` option] passed to the `render()` or `renderSync()` call.

[`file` option]: options.d.ts.md#legacyfileoptions

```ts
file?: string;
```

#### `data`

The [`data` option] passed to the `render()` or `renderSync()` call.

[`data` option]: options.d.ts.md#legacystringoptions

```ts
data?: string;
```

#### `includePaths`

A string that contains the current working directory followed by strings passed
in the `includePaths` option, separated by `";"` on Windows and `":"` elsewhere.

```ts
includePaths: string;
```

#### `precision`

```ts
precision: 10;
```

#### `style`

The integer 1.

> Older implementations returned other values for this, but that behavior is
> deprecated and should not be reproduced by new implementations.

```ts
style: 1;
```

#### `indentType`

The number 1 if the [`indentType` option] was `'tab'`. The number 0 otherwise.

[`indentType` option]: options.d.ts.md#indenttype

```ts
indentType: 1 | 0;
```

#### `indentWidth`

An integer indicating the number of spaces or tabs emitted by the compiler for
each level of indentation.

```ts
indentWidth: number;
```

#### `linefeed`

A value based on the [`linefeed` option] passed to the `render()` or
`renderSync()`:

[`linefeed` option]: options.d.ts.md#linefeed

* If `linefeed` is `"cr"`, this must be `"\r"`.
* If `linefeed` is `"crlf"`, this must be `"\r\n"`.
* If `linefeed` is `"lf"` or `undefined`, this must be `"\n"`.
* If `linefeed` is `"lfcr"`, this must be `"\n\r"`.

```ts
linefeed: '\r' | '\r\n' | '\n' | '\n\r';
```

#### `result`

An object with a single field, `stats`, which contains several subfields.

```ts
result: {
  stats: {
```

##### `result.stats.start`

The number of milliseconds since the Unix epoch (1 January 1970 00:00:00 UT) at
the point at which the user called `render()` or `renderSync()`.

```ts
start: number;
```

##### `result.stats.entry`

The [`file` option] passed to the `render()` call, or the string `"data"` if no
file was passed.

```ts
entry: string;
```

```ts
      }; // options.result.stats
    }; // options.result
  }; // options
} // LegacyPluginThis
```
