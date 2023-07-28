# First Class Mixins: Draft 1

*([Issue](https://github.com/sass/sass/issues/626))*

This proposal promotes mixins to first-class values and adds members to the
`sass:meta` module for manipulating them.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Types](#types)
  * [Operations](#operations)
    * [Equality](#equality)
  * [Serialization](#serialization)
* [Functions](#functions)
  * [`meta.type-of()`](#metatype-of)
  * [`meta.get-mixin()`](#metaget-mixin)
  * [`meta.module-mixins()`](#metamodule-mixins)
  * [`meta.accepts-content()`](#metaaccepts-content)
* [Mixins](#mixins)
  * [`meta.apply()`](#metaapply)
* [JavaScript API](#javascript-api)
  * [Design Decisions](#design-decisions)
  * [API](#api)
  * [Types](#types-1)
    * [`SassMixin`](#sassmixin)
      * [`assertMixin`](#assertmixin)
      * [`internal`](#internal)
      * [Constructor](#constructor)
* [Embedded Protocol](#embedded-protocol)

## Background

> This section is non-normative.

Sass today has two callables: the function and the mixin. Functions in Sass are
a first class SassScript value type; they can be assigned to variables and
invoked dynamically.

Mixins, however, are not first class values. This is a stumbling block many
users (sass/sass#626, sass/sass#673, sass/sass#3328, sass/sass#3439, among
others) of Sass run into.

Promoting mixins to be first class values would resolve a number of commonly
requested features while also rounding out the language and adding feature
parity between Sass's two callables.

## Summary

> This section is non-normative.

This proposal promotes mixins to first-class values, giving Sass composability
for both of its kinds of callables.

Additionally, it provides ways to interact with mixin values similarly to how
one would with function values:

* `meta.get-function()` => `meta.get-mixin()`

* `meta.module-functions()` => `meta.module-mixins()`

* `meta.call()` => `meta.apply()`

## Types

This proposal promotes the [mixin value][] to a Sass value type.

[mixin value]: ../spec/at-rules/mixin.md#mixin

### Operations

The only operation permitted for mixin objects is checking for equality. All
other operations throw an error.

#### Equality

When the Sass interpreter encounters an `@mixin` rule in Sass source code, it
constructs a mixin object in memory. Additionally, some mixin objects are
pre-defined by the Sass language and accessible though the builtin modules.

Mixin objects, like function objects, use pointer equality.

If the same file were to be imported multiple times, the Sass interpreter would
create a new mixin object for each `@mixin` rule each time the file is imported.
Because a new mixin object has been created, although the name, body, and source
span of a given mixin from the file would be the same between imports, the
objects would not be equal because they refer to different objects in memory.
Mixins pre-defined by the Sass language are instatiated at most once during the
entire evaluation of a program.

> As an example, if we declare two mixins:
> 
> ```scss
> @mixin mixin1 {
>   color: red;
> }
> 
> $a: meta.get-mixin(mixin1);
> 
> @mixin mixin1 {
>   color: red;
> }
> 
> $b: meta.get-mixin(mixin1);
> ```
> 
> Although every aspect of the two mixins is the same, `$a != $b`, because they
> refer to separate mixin declarations/objects.

### Serialization

To serialize a `Mixin`:

* If the value is not being inspected, throw an error.

* Otherwise:

  * Emit `"get-mixin("`.

  * Emit a double quote (`"`), then the `name` of the mixin, then another double
    quote.

  * Emit `")"`.

## Functions

### `meta.type-of()`

Add the following clause to the [`meta.type-of()`] function and the top-level
`type-of()` function:

[`meta.type-of()`]: ../spec/built-in-modules/meta.md#type-of

* If `$value` is a mixin, return an unquoted string with value `"mixin"`.

### `meta.get-mixin()`

This is a new function in the `sass:meta` module.

```
meta.get-mixin($name, $module: null)
```

* If `$name` is not a string, throw an error.

* If `$module` is null:

  * Return the result of resolving a mixin named `$name`. If this returns
    null, throw an error.

* Otherwise:
  
  * If `$module` is not a string, throw an error.

  * Let `use` be the `@use` rule in [the current source file][] whose
    namespace is equal to `$module`. If no such rule exists, throw an error.

  * Return [`use`'s module][]'s mixin named `$name`, or throw an error if no
    such mixin exists.

  [the current source file]: ../spec/spec.md#current-source-file
  [`use`'s module]: ../spec/at-rules/use.md#a-use-rules-module

### `meta.module-mixins()`

This is a new function in the `sass:meta` module.

```
meta.module-mixins($module)
```

* If `$module` is not a string, throw an error.

* Let `use` be the `@use` rule in [the current source file][] whose namespace is
  equal to `$module`. If no such rule exists, throw an error.

* Return a map whose keys are the quoted string names of mixins in
  [`use`'s module][] and whose values are the corresponding mixins.

### `meta.accepts-content()`

This is a new function in the `sass:meta` module.

```
meta.accepts-content($mixin)
```

* If `$mixin` is not a mixin, throw an error.

* Return a boolean which is true if the body of the mixin has an `@content`
  rule.

## Mixins

### `meta.apply()`

```
meta.apply($mixin, $args...)
```

* If `$mixin` is not a mixin, throw an error.

* If the current `@include` rule has a `ContentBlock`, it should be passed down
  to `$mixin`. Mixins must be invoked with `@include`, so the current include
  rule is guaranteed to exist.

* Execute the `ArgumentInvocation` `(...$args)` with `$mixin`'s
  `ArgumentDeclaration` in `$mixin`'s scope.

* Execute each statement in `$mixin`.

## JavaScript API

### Design Decisions

Mixins differ from functions in that the result of their execution is a Sass AST
node, and not a SassScript value. Sass today does not expose ways to create or
manipulate AST nodes through the JavaScript API, nor does it intend to do so in
the future.

For this reason, it is not meaningful -- or even possible -- to construct or 
execute a mixin through the JavaScript API. A mixin object shall be opaque, and
the only operation available shall be to return the object as-is.

### API

```ts
import {Value} from '../spec/js-api/value';
```

### Types

#### `SassMixin`

The JS API representation of a Sass mixin.

```ts
export class SassMixin extends Value {
```

##### `assertMixin`

Returns `this` if it's a [`SassMixin`] and throws an error otherwise.

[`SassMixin`]: #sassmixin

> The `name` parameter may be used for error reporting.

```ts
assertMixin(name?: string): SassMixin;
```

##### `internal`

The [private `internal` field] refers to a Sass mixin.

[private `internal` field]: ../spec/js-api/value/index.d.ts.md#internal

##### Constructor

Throws an error.

```ts
constructor();
```

```ts
} // SassMixin
```

## Embedded Protocol

This document proposes adding a new value:

```proto
// A first-class mixin defined in the compiler. New `CompilerMixin`s may
// only be created by the compiler, but the host may pass `CompilerMixin`s
// back to the compiler as long as their IDs match IDs of mixins received
// by the host during that same compilation.
message CompilerMixin {
  // A unique ID for this mixin. The compiler is responsible for generating
  // this ID and ensuring it's unique across all mixins passed to the host
  // for this compilation. Mandatory.
  uint32 id = 1;
}
```

The protocol allows first-class mixins defined in the compiler to be passed
to the host and vice-versa as `Value.CompilerMixin`s.

Two first-class mixins are equal if they have the same ID.