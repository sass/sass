# Logger API

```ts
import {Deprecation} from '../deprecations';
import {SourceSpan} from './source_span';

export {SourceLocation} from './source_location';
export {SourceSpan} from './source_span';
```

## Table of Contents

* [Types](#types)
  * [`LoggerWarnOptions`](#loggerwarnoptions)
  * [`Logger`](#logger)
    * [`warn`](#warn)
    * [`debug`](#debug)
* [Fields](#fields)
  * [`Logger`](#logger-1)
    * [`silent`](#silent)

## Types

### `LoggerWarnOptions`

The options passed to `Logger.warn`. These are split out for documentation
purposes.

```ts
type LoggerWarnDeprecationOptions =
  | {
      deprecation: true;
      deprecationType: Deprecation;
    }
  | {deprecation: false};

interface LoggerWarnOptions extends LoggerWarnDeprecationOptions {
  span?: SourceSpan;
  stack?: string;
}
```

### `Logger`

An object that provides callbacks for handling messages from the compiler.

```ts
export interface Logger {
```

#### `warn`

If this field is defined, the compiler must invoke it under the following
circumstances:

* When it encounters a `@warn` rule:

  * Let `value` be the result of evaluating the rule's expression.
  * Let `message` be `value`'s text if it's a string, or the result of
    serializing `value` if it's not.
  * Invoke `warn` with `message` and an object with `deprecation` set to `false`
    and `stack` set to a string representation of the current Sass stack trace.

    > The specific format of the stack trace may vary from implementation to
    > implementation.

* When it encounters anything else that the user needs to be warned about:

  > This is intentionally vague about what counts as a warning. Implementations
  > have a considerable degree of flexibility in defining this for themselves,
  > although in some cases warnings are mandated by the specification (such as
  > in preparation for a breaking change).

  * Let `options` be an empty object.
  * If this warning is caused by behavior that used to be allowed but will be
    disallowed in the future, set `options.deprecation` to `true` and set
    `options.deprecationType` to the relevant `Deprecation`. Otherwise, set
    `options.deprecation` to `false` and leave `options.deprecationType`
    undefined.
  * If this warning is associated with a specific span of a Sass stylesheet, set
    `options.span` to a `SourceSpan` that covers that span.
  * If this warning occurred during execution of a stylesheet, set
    `options.stack` to a string representation of the current Sass stack trace.
  * Invoke `warn` with a string describing the warning and `options`.

If this field is defined, the compiler must not surface warnings in any way
other than inkoving `warn`.

```ts
warn?(message: string, options: LoggerWarnOptions): void;
```

#### `debug`

If this field is defined, the compiler must invoke it when it encounters a
`@debug` rule using the following procedure:

* Let `value` be the result of evaluating the rule's expression.
* Let `message` be `value`'s text if it's a string, or the result of serializing
  `value` if it's not.
* Invoke `debug` with `message` and an object with `span` set to the span
  covering the `@debug` rule and its expression.

If this field is defined, the compiler must not surface debug messages in any
way other than invoking `debug`.

```ts
debug?(message: string, options: {span: SourceSpan}): void;
```

```ts
} // interface Logger
```

## Fields

### `Logger`

A namespace for built-in logger implementations.

```ts
export namespace Logger {
```

#### `silent`

A [`Logger`] that does nothing when it warn or debug methods are called.

[`Logger`]: #logger

```ts
export const silent: Logger;
```

```ts
} // namespace Logger
```
