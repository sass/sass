# Compile API

> These APIs are the entrypoints for compiling Sass to CSS.

```ts
import {RawSourceMap} from 'source-map-js'; // https://www.npmjs.com/package/source-map-js

import {Options, StringOptions} from './options';
```

## Table of Contents

* [Types](#types)
  * [`CompileResult`](#compileresult)
* [Functions](#functions)
  * [`compile`](#compile)
  * [`compileAsync`](#compileasync)
  * [`compileString`](#compilestring)
  * [`compileStringAsync`](#compilestringasync)

## Types

### `CompileResult`

The object returned by the compiler when a Sass compilation succeeds.

```ts
export interface CompileResult {
  css: string;

  loadedUrls: URL[];

  sourceMap?: RawSourceMap;
}
```

## Functions

### `compile`

Compiles the Sass file at `path`:

* If any object in `options.importers` has both `findFileUrl` and `canonicalize`
  fields, throw an error.

* Let `css` be the result of [compiling `path`] with `options.importers` as
  `importers` and `options.loadPaths` as `load-paths`. The compiler must respect
  the configuration specified by the `options` object.

  [compiling `path`]: ../spec.md#compiling-a-path

* If the compilation succeeds, return a `CompileResult` object composed as
  follows:

  * Set `CompileResult.css` to `css`.

  * Set `CompileResult.loadedUrls` to a list of unique canonical URLs of source
    files [loaded] during the compilation. The order of URLs is not guaranteed.

    [loaded]: ../modules.md#loading-a-source-file

  * If `options.sourceMap` is `true`, set `CompileResult.sourceMap` to a
    sourceMap object describing how sections of the Sass input correspond to
    sections of the CSS output.

    > The structure of the sourceMap can vary from implementation to
    > implementation.

* Otherwise, throw an `Exception`.

```ts
export function compile(path: string, options?: Options<'sync'>): CompileResult;
```

### `compileAsync`

Like [`compile`], but runs asynchronously.

[`compile`]: #compile

The compiler must support asynchronous plugins when running in this mode.

```ts
export function compileAsync(
  path: string,
  options?: Options<'async'>
): Promise<CompileResult>;
```

### `compileString`

Compiles the Sass `source`:

* If `options.importer` or any object in `options.importers` has both
  `findFileUrl` and `canonicalize` fields, throw an error.

* Let `css` be the result of [compiling a string] with:

  * `options.source` as `string`;
  * `options.syntax` as `syntax`, or "scss" if `options.syntax` is not set;
  * `options.url` as `url`;
  * `options.importer` as `importer`;
  * `options.importers` as `importers`;
  * `options.loadPaths` as `load-paths`.

  The compiler must respect the configuration specified by the `options` object.

  [compiling a string]: ../spec.md#compiling-a-string

* If the compilation succeeds, return a `CompileResult` object composed as
  follows:

  * Set `CompileResult.css` to `css`.

  * Set `CompileResult.loadedUrls` to a list of unique canonical URLs of source
    files [loaded] during the compilation. The order of URLs is not guaranteed.

    * If `options.url` is set, include it in the list.
    * Otherwise, do not include a URL for `source`.

  * If `options.sourceMap` is `true`, set `CompileResult.sourceMap` to a
    sourceMap object describing how sections of the Sass input correspond to
    sections of the CSS output.

    > The structure of the sourceMap can vary from implementation to
    > implementation.

  [loaded]: ../modules.md#loading-a-source-file

* If the compilation fails, throw an `Exception`.

```ts
export function compileString(
  source: string,
  options?: StringOptions<'sync'>
): CompileResult;
```

### `compileStringAsync`

Like `compileString`, but runs asynchronously.

The compiler must support asynchronous plugins when running in this mode.

```ts
export function compileStringAsync(
  source: string,
  options?: StringOptions<'async'>
): Promise<CompileResult>;
```
