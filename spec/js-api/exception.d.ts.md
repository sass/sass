# Exception API

The error thrown by the compiler when a Sass compilation fails. This should
*not* be thrown for errors that occur outside of Sass compilation, such as
argument verification errors.

```ts
import {SourceSpan} from './logger';
```

## Table of Contents

* [Types](#types)
  * [`Exception`](#exception)
    * [`message`](#message)
    * [`sassMessage`](#sassmessage)
    * [`sassStack`](#sassstack)
    * [`span`](#span)
    * [`toString()`](#tostring)

## Types

### `Exception`

```ts
export class Exception extends Error {
  private constructor();
```

#### `message`

The compiler supplies this error message to the JS runtime. This should contain
the description of the Sass exception as well as human-friendly representations
of `span` and `sassStack` (if they're set).

This message must be passed directly to the super constructor.

> The format can vary from implementation to implementation.

```ts
message: string;
```

#### `sassMessage`

The Sass error message, excluding the human-friendly representation of `span`
and `sassStack`.

> The format can vary from implementation to implementation.

```ts
readonly sassMessage: string;
```

#### `sassStack`

A human-friendly representation of the loads, function calls, and mixin includes
that were active when this error was thrown.

> The format can vary from implementation to implementation.

```ts
readonly sassStack: string;
```

#### `span`

A span whose `url` is the canonical URL of the stylesheet being parsed or
evaluated, and whose `start` points to the line in that stylesheet on which the
error occurred.

> The other details of this span can vary from implementation to implementation,
> but implementations are strongly encouraged to ensure that this covers a span
> of text that clearly indicates the location of the error.

```ts
readonly span: SourceSpan;
```

#### `toString()`

Provides a formatted string with useful information about the error.

> This likely includes the Sass error message, span, and stack. The format can
> vary from implementation to implementation.

```ts
toString(): string; // TODO(awjin): Mark this as `override` once TS 4.3 is released.
```

```ts
} // Exception
```
