# JavaScript Mixin API: Draft 1

*([Issue](https://github.com/sass/sass/issues/626))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [API](#api)
* [Types](#types)
  * [`SassMixin`](#sassmixin)
    * [`assertMixin`](#assertmixin)
    * [`internal`](#internal)
    * [Constructor](#constructor)

## Background

> This section is non-normative.

This proposal exposes the [mixin type] to the JavaScript API.

[mixin type]: ../proposal/first-class-mixins.md

## Summary

> This section is non-normative.

### Design Decisions

Mixins differ from functions in that the result of their execution is a Sass AST
node, and not a SassScript value. Sass today does not expose ways to create or
manipulate AST nodes through the JavaScript API, nor does it intend to do so in
the future.

For this reason, it is not meaningful -- or even possible -- to construct or 
execute a mixin through the JavaScript API. A mixin object shall be opaque, and
the only operation available shall be to return the object as-is.

## API

```ts
import {Value} from '../spec/js-api/value';
```

## Types

### `SassMixin`

The JS API representation of a Sass mixin.

```ts
export class SassMixin extends Value {
```

#### `assertMixin`

Returns `this` if it's a [`SassMixin`] and throws an error otherwise.

[`SassMixin`]: #sassmixin

> The `name` parameter may be used for error reporting.

```ts
assertMixin(name?: string): SassMixin;
```

#### `internal`

The [private `internal` field] refers to a Sass mixin.

[private `internal` field]: ../spec/js-api/value/index.d.ts.md#internal

#### Constructor

Throws an error.

```ts
constructor();
```

```ts
} // SassMixin
```
