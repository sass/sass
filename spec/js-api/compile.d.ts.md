# Compile API

> These APIs are the entrypoints for compiling Sass to CSS.

```ts
import {RawSourceMap} from 'source-map-js'; // https://www.npmjs.com/package/source-map-js

import {Options, StringOptions} from './options';
```

## Table of Contents

* [Types](#types)
  * [`CompileResult`](#compileresult)
  * [`Compiler`](#compiler)
    * [`dispose()`](#dispose)
  * [`AsyncCompiler`](#asynccompiler)
    * [`dispose()`](#dispose-1)
* [Functions](#functions)
  * [`compile`](#compile)
  * [`compileAsync`](#compileasync)
  * [`compileString`](#compilestring)
  * [`compileStringAsync`](#compilestringasync)
  * [`initCompiler`](#initcompiler)
  * [`initAsyncCompiler`](#initasynccompiler)

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

### `Compiler`

The class returned by creating a synchronous compiler with [`initCompiler()`].
If the class is directly constructed (as opposed to the Compiler being created
via `initCompiler`), throw an error.

Only synchronous compilation methods [`compile()`] and [`compileString()`] must
be included, and must have with identical semantics to the [Sass interface].

[`initCompiler()`]: #initcompiler
[`compile()`]: #compile
[`compilestring()`]: #compilestring
[Sass interface]: ./index.d.ts.md

```ts
export class Compiler {
  private constructor();
  compile(path: string, options?: Options<'sync'>): CompileResult;

  compileString(source: string, options?: StringOptions<'sync'>): CompileResult;
```

#### `dispose()`

When `dispose` is invoked on a Compiler:

* Any subsequent invocations of `compile` and `compileString` must throw an
  error.

```ts
  dispose(): void;
}
```

### `AsyncCompiler`

The object returned by creating an asynchronous compiler with
[`initAsyncCompiler()`]. If the class is directly constructed (as opposed to the
AsyncCompiler being created via `initAsyncCompiler`), throw an error.

Only asynchronous compilation methods [`compileAsync()`] and
[`compileStringAsync()`] must be included, and must have with identical
semantics to the [Sass interface].

[`initAsyncCompiler()`]: #initasynccompiler
[`compileasync()`]: #compileasync
[`compilestringasync()`]: #compilestringasync

```ts
export class AsyncCompiler {
  private constructor();
  compileAsync(
    path: string,
    options?: Options<'async'>
  ): Promise<CompileResult>;

  compileStringAsync(
    source: string,
    options?: StringOptions<'async'>
  ): Promise<CompileResult>;
```

#### `dispose()`

When `dispose` is invoked on an Async Compiler:

* Any subsequent invocations of `compileAsync` and `compileStringAsync` must
  throw an error.

* Any compilations that have not yet been settled must be allowed to settle, and
  not be cancelled.

* Resolves a Promise when all compilations have been settled, and disposal is
  complete.

```ts
  dispose(): Promise<void>;
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

### `initCompiler`

Returns a synchronous [Compiler].

[Compiler]: #compiler

```ts
export function initCompiler(): Compiler;
```

### `initAsyncCompiler`

Resolves with an [Async Compiler].

[Async Compiler]: #asynccompiler

```ts
export function initAsyncCompiler(): Promise<AsyncCompiler>;
```
