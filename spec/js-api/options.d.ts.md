## API Options

> The options object that's passed to the [compile API] to control various
> aspects of Sass compilation.
>
> [compile API]: compile.d.ts.md

```ts
import {FileImporter, Importer} from './importer';
import {Logger} from './logger';
import {Value} from './value';
import {PromiseOr} from './util/promise_or';
```

## Table of Contents

* [Types](#types)
* [`Syntax`](#syntax)
* [`OutputStyle`](#outputstyle)
* [`CustomFunction`](#customfunction)
* [`Options`](#options)
  * [`alertAscii`](#alertascii)
  * [`alertColor`](#alertcolor)
  * [`charset`](#charset)
  * [`functions`](#functions)
  * [`importers`](#importers)
  * [`loadPaths`](#loadpaths)
  * [`logger`](#logger)
  * [`quietDeps`](#quietdeps)
  * [`sourceMap`](#sourcemap)
  * [`sourceMapIncludeSources`](#sourcemapincludesources)
  * [`style`](#style)
  * [`usePkgImporter`](#usepkgimporter)
  * [`verbose`](#verbose)
* [`StringOptionsWithoutImporter`](#stringoptionswithoutimporter)
  * [`syntax`](#syntax)
  * [`url`](#url)
* [`StringOptionsWithImporter`](#stringoptionswithimporter)
  * [`importer`](#importer)
  * [`url`](#url-1)
* [`StringOptions`](#stringoptions)

## Types

### `Syntax`

The types of input syntax that the compiler can parse.

```ts
export type Syntax = 'scss' | 'indented' | 'css';
```

### `OutputStyle`

The ways in which the compiler can format the emitted CSS. See [`Options.style`]
for details.

[`Options.style`]: #style

```ts
export type OutputStyle = 'expanded' | 'compressed';
```

### `CustomFunction`

A custom function that can be called from Sass stylesheets.

```ts
export type CustomFunction<sync extends 'sync' | 'async'> = (
  args: Value[]
) => PromiseOr<Value, sync>;
```

### `Options`

All of the options for a Sass compilation that are shared between compiling from
a path and by compiling from a string.

```ts
export interface Options<sync extends 'sync' | 'async'> {
```

#### `alertAscii`

If true, the compiler must use only ASCII characters in the formatted message of
errors and logs that aren't handled by a `logger`. Defaults to false.

```ts
alertAscii?: boolean;
```

#### `alertColor`

If true, the compiler may use terminal colors in the formatted message of errors
and logs that aren't handled by a `logger`. Implementations may choose the
default value for this based on their own heuristics of whether colored output
would be useful or render appropriately. Implementations are not obligated to
use colors even if this is `true`.

> The specific format of colored output can vary from implementation to
> implementation.

```ts
alertColor?: boolean;
```

#### `charset`

If true, the compiler must prepend `@charset "UTF-8";` or U+FEFF (byte-order
marker) if it emits non-ASCII CSS.

If false, the compiler must not prepend these byte sequences.

Defaults to true.

> This is ideal when concatenating CSS or embedding it in HTML `<style>` tags.
> Note that the output will still be UTF-8 regardless of this option.

```ts
charset?: boolean;
```

#### `functions`

When the compiler encounters a global function call with a signature that does
not match that of a built-in function, but matches a key in this record, it must
call the associated `CustomFunction` and return its result. If the function
throws an error or returns anything other than a `Value`, the compiler should
treat it as the Sass function throwing an error.

> As in the rest of Sass, `_`s and `-`s are considered equivalent when
> determining which function signatures match.

Before beginning compilation, if any key in this record is not an
[<ident-token>] followed immediately by an `ArgumentDeclaration`, the compiler
must throw an error.

[<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram

If the `CustomFunction` returns an invalid value, or a value that transitively
contains an invalid value, the compiler must treat that as the Sass function
throwing an error. The following values are considered invalid:

* An object that's not an instance of the `Value` class.

* A `SassFunction` whose `signature` field isn't a valid Sass function signature
  that could appear after the `@function` directive in a Sass stylesheet.
  
```ts
functions?: Record<string, CustomFunction<sync>>;
```

#### `importers`

The list of [custom importers] to use to resolve file loads.

[custom importers]: importer.d.ts.md

```ts
importers?: (Importer<sync> | FileImporter<sync>)[];
```

#### `loadPaths`

If set, the compiler must use these paths to resolve imports.

```ts
loadPaths?: string[];
```

#### `logger`

A [custom logger] that provides callbacks for the compiler to use in lieu of its
default messaging behavior.

[custom logger]: logger/index.d.ts.md

The compiler must treat an `undefined` logger identically to an object that
doesn't have `warn` or `debug` fields.

```ts
logger?: Logger;
```

#### `quietDeps`

If true, the compiler must not print deprecation warnings for stylesheets that
are transitively loaded through an import path.

Defaults to false.

```ts
quietDeps?: boolean;
```

#### `sourceMap`

If true, the compiler must set [`CompileResult.sourceMap`] to a sourceMap object
that represents the mapping between the generated CSS and the source files.

[`CompileResult.sourceMap`]: compile.d.ts.md#compileresult

Defaults to false.

> Except as otherwise specified, the exact structure of this file and how it
> maps between CSS and Sass is left up to the implementation.

```ts
sourceMap?: boolean;
```

#### `sourceMapIncludeSources`

If true, the compiler must include the full Sass source text in
[`CompileResult.sourceMap`].

Defaults to false.

```ts
sourceMapIncludeSources?: boolean;
```

#### `style`

If present, the compiler must format the emitted CSS in this style.

Implementations may support any subset of `OutputStyle`s, provided that:

* They support the `'expanded'` style.
* They produce CSS that is semantically equivalent regardless of style.
* They throw an error if they receive a value for this option that they do not
  support.

> The specifics of each format can vary from implementation to implementation.
> If an implementation wants to add a new `OutputStyle`, the `OutputStyle` type
> should be expanded in this spec first to ensure that style names and
> TypeScript types remain consistent across implementations.

```ts
style?: OutputStyle;
```

#### `usePkgImporter`

If true, the compiler will use the built-in [package importer] to resolve any url with the `pkg` scheme.

[package importer]: ../../proposal/package-importer.md

Defaults to false.

```ts
usePkgImporter?: boolean;
```

#### `verbose`

If true, the compiler must print every single deprecation warning it encounters
(except for those silenced by [`quietDeps`]).

[`quietDeps`]: #quietdeps

If false, the compiler may choose not to print repeated deprecation warnings.

Defaults to false.

```ts
verbose?: boolean;
```

```ts
} // Options
```

### `StringOptionsWithoutImporter`

> This interface is used for calls to [`compileString()`] and
> [`compileStringAsync()`] that don't pass the `importer` parameter, and so
> don't support relative imports.
>
> [`compileString()`]: compile.d.ts.md#compilestring
> [`compileStringAsync()`]: compile.d.ts.md#compilestringasync

```ts
export interface StringOptionsWithoutImporter<sync extends 'sync' | 'async'>
  extends Options<sync> {
```

#### `syntax`

The compiler must parse `source` using this syntax. Defaults to `'scss'`.

```ts
syntax?: Syntax;
```

#### `url`

The URL of the stylesheet being parsed.

> When `importer` isn't passed, this is purely advisory and only used for error
> reporting.

```ts
url?: URL;
```

```ts
} // StringOptionsWithoutImporter
```

### `StringOptionsWithImporter`

> This interface is used for calls to [`compileString()`] and
> [`compileStringAsync()`] that _do_ pass the `importer` parameter, and so _do_
> support relative imports.

```ts
export interface StringOptionsWithImporter<sync extends 'sync' | 'async'>
  extends StringOptionsWithoutImporter<sync> {
```

#### `importer`

The [importer] to use to resolve relative imports in the entrypoint.

[importer]: importer.d.ts.md

```ts
importer: Importer<sync> | FileImporter<sync>;
```

#### `url`

The canonical URL of the entrypoint.

> This _must_ be passed when `importer` is passed, since otherwise there's
> nothing to resolve relative URLs relative to.

```ts
url: URL;
```

```ts
} // StringOptionsWithImporter
```

### `StringOptions`

```ts
export type StringOptions<sync extends 'sync' | 'async'> =
  | StringOptionsWithImporter<sync>
  | StringOptionsWithoutImporter<sync>;
```
