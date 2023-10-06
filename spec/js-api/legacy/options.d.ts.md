# Legacy API Options

```ts
import {Logger} from '../logger';
import {LegacyImporter} from './importer';
import {LegacyFunction} from './function';
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
    * [`verbose`](#verbose)
    * [`logger`](#logger)
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
