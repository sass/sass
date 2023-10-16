# Shared Resources in JavaScript API

*([Issue](https://github.com/sass/sass/issues/3296))*

This proposal adds an API design that allows for sharing resources across
multiple invocations of the Sass compiler's JavaScript API. This will provide
Sass's users with a more efficient way of running Sass compilations across
multiple files.

## Table of Contents

* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Splitting Sync and Async Compilers](#splitting-sync-and-async-compilers)
    * [Limited API interface](#limited-api-interface)
    * [Flexibility for interfaces on process management](#flexibility-for-interfaces-on-process-management)
    * [No shared state](#no-shared-state)
  * [Example](#example)
    * [Sync Compiler](#sync-compiler)
    * [Async Compiler](#async-compiler)
* [API](#api)
  * [Types](#types)
    * [initCompiler()](#initcompiler)
    * [initAsyncCompiler()](#initasynccompiler)
    * [Compiler](#compiler)
      * [dispose()](#dispose)
    * [Async Compiler](#async-compiler-1)
      * [dispose()](#dispose-1)
* [Semantics](#semantics)
  * [Compiler](#compiler-1)
  * [Async Compiler](#async-compiler-2)
  * [Disposing a Compiler](#disposing-a-compiler)
  * [Disposing an Async Compiler](#disposing-an-async-compiler)

## Summary

Currently, the JavaScript API for Sass only accommodates a single compilation
per process. In practice, we have observed build tools are compiling multiple
times in response to a single user action. For instance, Vue.js authors using
Vite will see a Sass compilation for each `<style lang="scss">` tag that appears
in their codebase.

While processes can be spun up and down quickly, the combined time can add up to
a noticeable impact on performance. The embedded client supports long running
processes, and this proposal adds a method for the embedded host to manage the
lifecycle of these processes through a Compiler interface.

### Design Decisions

#### Splitting Sync and Async Compilers

When creating a Compiler, users will need to choose either a compiler that
provides access to synchronous or asynchronous compilation. While providing both
simultaneously from a single Compiler would offer more flexibility, it also adds
significant complexity to the API. In practice, we expect most users will only
want to use one mode, generally in the mode that is the fastest for the
implementation. If synchronous and asynchronous compilations are both needed,
users can create multiple Compilers.

#### Limited API interface

Many build tools allow passing the Sass module as a parameter, which offers
flexibility to users on what implementation of Sass is used. Because users may
still want to use portions of the JavaScript API unrelated to compilation, we
considered having the Compiler interface mirror the top level Sass interface,
which would allow users to replace instances of the imported `sass` class with
an instance of the compiler. However, this adds an additional cost to ongoing
Sass development. The proposed API does not eliminate this as a possibility in
the future.

#### Flexibility for interfaces on process management

In environments without access to a long-running compiler—for instance, the Dart
Sass implementation—the Compiler interface will continue to perform a single
compilation per process.

#### No shared state

This proposal does not change how a single compilation is done, and no state is
shared across compilations. Options and importers must be set for each
compilation. Future enhancements may introduce [shared state], but this proposal
only adds the ability to run multiple compilations on a single process.

[shared state]: https://github.com/sass/sass/issues/3296

This also means that the proposal makes no assertions about whether file content
has changed. It is also up to the user to determine when to start and stop a
long-running compiler process.

### Example

#### Sync Compiler

```js
import * as sass from 'sass';
function setup() {
  const compiler = sass.initCompiler();
  const result1 = compiler.compileString('a {b: c}').css;
  const result2 = compiler.compileString('a {b: c}').css;
  compiler.dispose();

  // throws error
  const result3 = sass.compileString('a {b: c}').css;
}
```

#### Async Compiler

```js
import * as sass from 'sass';
async function setup() {
  const compiler = await sass.initAsyncCompiler();
  const result1 = await compiler.compileStringAsync('a {b: c}').css;
  const result2 = await compiler.compileStringAsync('a {b: c}').css;
  compiler.dispose();

  // throws error
  const result3 = sass.compileStringAsync('a {b: c}').css;
}
```

## API

### Types

```ts
import {CompileResult} from '../spec/js-api/compile';
import {Options, StringOptions} from '../spec/js-api/options';
```

#### initCompiler()

Returns a synchronous [Compiler].

[Compiler]: #compiler

```ts
export function initCompiler(): Compiler;
```

#### initAsyncCompiler()

Resolves with an [Async Compiler].

[Async Compiler]: #async-compiler

```ts
export function initAsyncCompiler(): Promise<AsyncCompiler>;
```

#### Compiler

An instance of the synchronous [Compiler]. Re-exports [`compile()`]
and [`compileString()`].

[`compile()`]: ../spec/js-api/compile.d.ts.md#compile
[`compilestring()`]: ../spec/js-api/compile.d.ts.md#compilestring

```ts
export interface Compiler {
  compile(path: string, options?: Options<'sync'>): CompileResult;
  compileString(source: string, options?: StringOptions<'sync'>): CompileResult;
```

##### dispose()

Returns the result of [Disposing a Compiler].

[disposing a compiler]: #disposing-a-compiler

```ts
  dispose(): Boolean;
}
```

#### Async Compiler

An instance of the asynchronous [Compiler interface]. Re-exports
[`compileAsync()`] and [`compileStringAsync()`].

[`compileasync()`]: ../spec/js-api/compile.d.ts.md#compileasync
[`compilestringasync()`]: ../spec/js-api/compile.d.ts.md#compilestringasync

```ts
export interface AsyncCompiler {
  compileAsync(
    path: string,
    options?: Options<'async'>
  ): Promise<CompileResult>;
  compileStringAsync(
    source: string,
    options?: StringOptions<'async'>
  ): Promise<CompileResult>;
```

##### dispose()

* Resolves with the result of [Disposing a Compiler].

```ts
  dispose(): Promise<Boolean>;
}
```

## Semantics

### Compiler

A Compiler must:

* Have a lifetime, which starts upon construction.

* `compile` and `compileString` must have identical semantics to the [Sass
  interface].

* Have a `dispose` method, which ends the Compiler's lifetime.

[sass interface]: ../spec/js-api/index.d.ts.md

### Async Compiler

An Async Compiler must:

* Have a lifetime, which starts upon construction.

* `compileAsync` and `compileStringAsync` must have identical semantics to the
  [Sass interface].

* Have a `dispose` method, which ends the Compiler's lifetime.

### Disposing a Compiler

When `dispose` is invoked on a Compiler:

* Any subsequent invocations of `compile` and `compileString` must throw an
  error.

* Returns `true` when disposal is complete.

### Disposing an Async Compiler

When `dispose` is invoked on a Compiler or Async Compiler:

* Any subsequent invocations of `compileAsync` and `compileStringAsync` must
  throw an error.

* Resolves a Promise with `true` when disposal is complete.
