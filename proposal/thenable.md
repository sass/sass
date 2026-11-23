# Meta Escape: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4295))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [No Deprecation](#no-deprecation)
* [Definitions](#definitions)
  * [Thenable](#thenable)
* [Types](#types)
  * [`PromiseOr`](#promiseor)
  * [`FileImporter`](#fileimporter)
  * [`Importer`](#importer)
  * [`CustomFunction`](#customfunction)

## Background

> This section is non-normative.

Currently, the JS API specification defines several places where the API behaves
differently if a user value is a promise rather than any other value. What it
means to "be a promise" isn't elaborated on, but in practice both Dart Sass and
the embedded host check `instanceof Promise`, which is the most sensible option.

However, this breaks in circumstances where the global `Promise` value has been
monkey-patched to something that's incompatible with the promises returned by
`async` and other language features. This is done in practice by various
JavaScript libraries.

JavaScript itself handles this issue by checking for the existence of a `then()`
function when determining how to handle an `await` statement rather than
checking for a specific type. Types with this method are referred to as
"thenable".

## Summary

> This section is non-normative.

This proposal changes all existing checks for whether values are promises to
checks for thenables instead.

### Design Decisions

#### No Deprecation

This could technically be a breaking change. For example, it's possible that
code exists which returns an `ImporterResult` from `Importer.load()` which also
has a `then` method defined on it. However, this is vanishingly unlikely; what's
omre, JavaScript already treats thenables like promises in language features
like `await`, so nay use of such objects would already be extremely error-prone.
As such, we won't treat this as a breaking change but rather as a bug-fix for
alignment with JavaScript semantics.

## Definitions

### Thenable

Following [the ECMAScript definition], a "thenable" is any object with a `then`
property that returns a [function object].

[the ECMAScript definition]: https://tc39.es/ecma262/multipage/control-abstraction-objects.html#sec-promise-objects
[function object]: https://tc39.es/ecma262/multipage/ecmascript-data-types-and-values.html#function-object

> In practice, a value `v` is thenable if `typeof v?.then === 'function'`.

A thenable's `then` method is assumed to take two functions as arguments and to
call at most one of them. It's said to "resolve" or "complete" if it calls the
first function; the argument it passes to that function is its "value". It's
said to "reject" if it calls the second function; the argument it passes to that
function is its "error".

## Types

### `PromiseOr`

Replace `Promise<T>` in the definition with `PromiseLike<T>`.

### `FileImporter`

Replace all instances of "If it returns a promise" with "If it returns a
[thenable]."

[thenable]: #thenable

### `Importer`

Replace all instances of "If it returns a promise" with "If it returns a
[thenable]."

### `CustomFunction`

When invoking a custom function, treat any [thenable] as an asynchronous return.
