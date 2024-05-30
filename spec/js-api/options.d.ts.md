## API Options

> The options object that's passed to the [compile API] to control various
> aspects of Sass compilation.
>
> [compile API]: compile.d.ts.md

```ts
import {DeprecationOrId, Version} from './deprecations';
import {FileImporter, Importer, NodePackageImporter} from './importer';
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
  * [`fatalDeprecations`](#fataldeprecations)
  * [`functions`](#functions)
  * [`futureDeprecations`](#futuredeprecations)
  * [`importers`](#importers)
  * [`loadPaths`](#loadpaths)
  * [`logger`](#logger)
  * [`quietDeps`](#quietdeps)
  * [`silenceDeprecations`](#silencedeprecations)
  * [`sourceMap`](#sourcemap)
  * [`sourceMapIncludeSources`](#sourcemapincludesources)
  * [`style`](#style)
  * [`verbose`](#verbose)
* [`StringOptions`](#stringoptions)
  * [`syntax`](#syntax)
  * [`importer`](#importer)
  * [`url`](#url)
* [`StringOptionsWithoutImporter`](#stringoptionswithoutimporter)
* [`StringOptionsWithImporter`](#stringoptionswithimporter)

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

#### `fatalDeprecations`

A set of deprecations to treat as fatal.

If a deprecation warning of any provided type is encountered during compilation,
the compiler must error instead.

The compiler should convert any string passed here to a `Deprecation` by
indexing `deprecations`. If an invalid deprecation ID is passed here, the
compiler must emit a warning. If a version is passed here, it should be treated
equivalently to passing all active deprecations whose `deprecatedIn` version is
less than or equal to it.

The compiler must emit a warning if a future deprecation that's not also
included in `futureDeprecations` or any obsolete deprecation is included here.

If a deprecation is passed both here and to `silenceDeprecations`, a warning
must be emitted, but making the deprecation fatal must take precedence.

```ts
fatalDeprecations?: (DeprecationOrId | Version)[];
```

#### `functions`

Before beginning compilation:

* For each key/value pair `signature`/`function` in this record:

  * If `signature` isn't an [<ident-token>] followed immediately by an
    `ArgumentDeclaration`, throw an error.

  * Let `name` be `signature`'s <ident-token>.

  * If there's already a global function whose name is
    [underscore-insensitively] equal to `name`, continue to the next key/value
    pair.

  * Otherwise, add a global function whose signature is `signature`. When this
    function is called:

    * Let `result` be the result of calling the associated `CustomFunction` with
      the given arguments. If this call throws an error, treat it as a Sass
      error thrown by the Sass function.

      > As in the rest of Sass, `_`s and `-`s are considered equivalent when
      > determining which function signatures match.

    * Throw an error if `result` is or transitively contains:

      * An object that's not an instance of the `Value` class.

      * A [`SassFunction`] whose `signature` field isn't a valid Sass function
        signature that could appear after the `@function` directive in a Sass
        stylesheet.

    * Return a copy of `result.internal` with all calculations it transitively
      contains (including the return value itself if it's a calculation)
      replaced with the result of [simplifying] those calculations.

[<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
[underscore-insensitively]: ../modules.md#underscore-insensitive
[`SassFunction`]: value/function.d.ts.md
[simplifying]: https://github.com/sass/sass/tree/main/spec/types/calculation.md#simplifying-a-calculation

```ts
functions?: Record<string, CustomFunction<sync>>;
```

#### `futureDeprecations`

A set of future deprecations to opt into early.

For each future deprecation provided here, the compiler must treat that
deprecation as if it is active, emitting warnings as necessary (subject to
`fatalDeprecations` and `silenceDeprecations`).

The compiler should convert any string passed here to a `Deprecation` by
indexing `Deprecations`. If an invalid deprecation ID is passed here, the
compiler must emit a warning.

The compiler must emit a warning if a non-future deprecation is included here.

```ts
futureDeprecations?: DeprecationOrId[];
```

#### `importers`

The list of [custom importers] or [package importers] to use to resolve file
loads.

[custom importers]: importer.d.ts.md
[package importers]: importer.d.ts.md

```ts
importers?: (Importer<sync> | FileImporter<sync> | NodePackageImporter)[];
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

#### `silenceDeprecations`

A set of active deprecations to ignore.

If a deprecation warning of any provided type is encountered during compilation,
the compiler must ignore it.

The compiler should convert any string passed here to a `Deprecation` by
indexing `Deprecations`. If an invalid deprecation ID is passed here, the
compiler must emit a warning.

The compiler must emit a warning if any non-active deprecation is included here.
If a future deprecation is included both here and in `futureDeprecations`, then
silencing it takes precedence.

```ts
silenceDeprecations?: DeprecationOrId[];
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

### `StringOptions`

> This interface is used for calls to [`compileString()`] and
> [`compileStringAsync()`].
>
> [`compileString()`]: compile.d.ts.md#compilestring
> [`compileStringAsync()`]: compile.d.ts.md#compilestringasync

```ts
export interface StringOptions<sync extends 'sync' | 'async'>
  extends Options<sync> {
```

#### `syntax`

The compiler must parse `source` using this syntax. Defaults to `'scss'`.

```ts
syntax?: Syntax;
```

#### `importer`

The [importer] to use to resolve relative imports in the entrypoint.

[importer]: importer.d.ts.md

> Relative entrypoint imports are resolved differently depending on whether the
> `url` parameter is also passed:
>
> * If `url` *is* passed, relative URLs are first resolved relative to that URL
>   and then passed to `importer`. This is the same behavior that's used when
>   resolving relative URLs in any other Sass file.
>
> * If `url` *is not* passed, relative URLs are passed directly to `importer`
>   without being resolved. This is a bit of a hack that relies on the fact that
>   some importers work like "load paths", resolving relative URLs based on some
>   implicit base URL. It's useful for situations like evaluating a stylesheet
>   passed via stdin and giving it access to relative loads in the working
>   directory without *also* giving it a fake canonical URL that would show up
>   in error messages and source maps.

```ts
importer?: Importer<sync> | FileImporter<sync>;
```

#### `url`

The canonical URL of the stylesheet being parsed.

> When `importer` isn't passed, this is purely advisory and only used for error
> reporting. When it is, it's used as the base URL of the relative imports
> within the entrypoint.

```ts
url?: URL;
```

```ts
} // StringOptions
```

### `StringOptionsWithoutImporter`

> This alias exists for backwards-compatibility only.

```ts
type StringOptionsWithoutImporter<sync extends 'sync' | 'async'> =
  StringOptions<sync>;
```

### `StringOptionsWithImporter`

> This alias exists for backwards-compatibility only.

```ts
type StringOptionsWithImporter<sync extends 'sync' | 'async'> =
  StringOptions<sync>;
```
