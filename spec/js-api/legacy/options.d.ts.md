# Legacy API Options

```ts
import {DeprecationOrId, Version} from '../deprecations';
import {Logger} from '../logger';
import {LegacyImporter} from './importer';
import {LegacyFunction} from './function';
import {NodePackageImporter} from '../importer';
```

## Table of Contents

* [Types](#types)
  * [`LegacySharedOptions`](#legacysharedoptions)
    * [`includePaths`](#includepaths)
    * [`indentType`](#indenttype)
    * [`indentWidth`](#indentwidth)
    * [`linefeed`](#linefeed)
    * [`omitSourceMapUrl`](#omitsourcemapurl)
    * [`outFile`](#outfile)
    * [`outputStyle`](#outputstyle)
    * [`sourceMap`](#sourcemap)
    * [`sourceMapContents`](#sourcemapcontents)
    * [`sourceMapEmbed`](#sourcemapembed)
    * [`sourceMapRoot`](#sourcemaproot)
    * [`importer`](#importer)
    * [`functions`](#functions)
    * [`charset`](#charset)
    * [`quietDeps`](#quietdeps)
    * [`fatalDeprecations`](#fataldeprecations)
    * [`futureDeprecations`](#futuredeprecations)
    * [`silenceDeprecations`](#silencedeprecations)
    * [`verbose`](#verbose)
    * [`logger`](#logger)
    * [`pkgImporter`](#pkgimporter)
  * [`LegacyFileOptions`](#legacyfileoptions)
  * [`LegacyStringOptions`](#legacystringoptions)
  * [`LegacyOptions`](#legacyoptions)

## Types

### `LegacySharedOptions`

All the options for a legacy Sass compilation except those that specify the
specific input format.

```ts
export interface LegacySharedOptions<sync extends 'sync' | 'async'> {
```

#### `includePaths`

```ts
includePaths?: string[];
```

#### `indentType`

```ts
indentType?: 'space' | 'tab';
```

#### `indentWidth`

```ts
indentWidth?: number;
```

#### `linefeed`

```ts
linefeed?: 'cr' | 'crlf' | 'lf' | 'lfcr';
```

#### `omitSourceMapUrl`

```ts
omitSourceMapUrl?: boolean;
```

#### `outFile`

```ts
outFile?: string;
```

#### `outputStyle`

```ts
outputStyle?: 'compressed' | 'expanded' | 'nested' | 'compact';
```

#### `sourceMap`

```ts
sourceMap?: boolean | string;
```

#### `sourceMapContents`

```ts
sourceMapContents?: boolean;
```

#### `sourceMapEmbed`

```ts
sourceMapEmbed?: boolean;
```

#### `sourceMapRoot`

```ts
sourceMapRoot?: string;
```

#### `importer`

```ts
importer?: LegacyImporter<sync> | LegacyImporter<sync>[];
```

#### `functions`

```ts
functions?: {[key: string]: LegacyFunction<sync>};
```

#### `charset`

If true, the compiler must prepend `@charset "UTF-8";` or U+FEFF (byte-order
marker) if it emits non-ASCII CSS.

If false, the compiler must not prepend these byte sequences.

Defaults to true.

```ts
charset?: boolean;
```

#### `quietDeps`

If true, the compiler must not print deprecation warnings for stylesheets that
are transitively loaded through an import path.

Defaults to false.

```ts
quietDeps?: boolean;
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

#### `verbose`

If true, the compiler must print every single deprecation warning it encounters
(except for those silenced by [`quietDeps`]).

[`quietDeps`]: #quietdeps

If false, the compiler may choose not to print repeated deprecation warnings.

Defaults to false.

```ts
verbose?: boolean;
```

#### `logger`

A [custom logger] that provides callbacks for the compiler to use in lieu of its
default messaging behavior.

[custom logger]: ../logger/index.d.ts.md

The compiler must treat an `undefined` logger identically to an object that
doesn't have `warn` or `debug` fields.

```ts
logger?: Logger;
```

#### `pkgImporter`

If set to an instance of [NodePackageImporter], the compiler will use it as the
specified built-in package importer to resolve any URL with the `pkg:` scheme.
This importer will be checked immediately before the existing legacy importer
logic, and if it returns `null`, the legacy importer logic will be invoked.

[NodePackageImporter]: ../importer.d.ts.md#nodepackageimporter

```ts
pkgImporter?: NodePackageImporter;
```

```ts
} // LegacySharedOptions
```

### `LegacyFileOptions`

```ts
export interface LegacyFileOptions<sync extends 'sync' | 'async'>
  extends LegacySharedOptions<sync> {
  file: string;

  data?: never;
}
```

### `LegacyStringOptions`

```ts
export interface LegacyStringOptions<sync extends 'sync' | 'async'>
  extends LegacySharedOptions<sync> {
  data: string;

  file?: string;

  indentedSyntax?: boolean;
}
```

### `LegacyOptions`

```ts
export type LegacyOptions<sync extends 'sync' | 'async'> =
  | LegacyFileOptions<sync>
  | LegacyStringOptions<sync>;
```
