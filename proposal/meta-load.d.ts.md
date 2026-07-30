# `meta.load()`: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/739))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [A New Type](#a-new-type)
    * [Inspect Format](#inspect-format)
    * [No JavaScript Constructor](#no-javascript-constructor)
    * [No JavaScript Members](#no-javascript-members)
* [Types](#types)
  * [Operations](#operations)
    * [Equality](#equality)
  * [Serialization](#serialization)
* [Functions](#functions)
  * [`meta.type-of()`](#metatype-of)
  * [`meta.load()`](#metaload)
  * [`meta.get-module()`](#metaget-module)
  * [Existing Functions](#existing-functions)
* [Mixins](#mixins)
  * [`meta.css()`](#metacss)
* [JavaScript API](#javascript-api)
  * [Types](#types-1)
    * [`SassModule`](#sassmodule)
      * [`assertModule`](#assertmodule)
      * [`internal`](#internal)
      * [Constructor](#constructor)
* [Embedded Protocol](#embedded-protocol)

## Background

> This section is non-normative.

The ability to load stylesheets dynamically is one of the oldest and most
popular feature requests in Sass. Many requests have taken the form of allowing
interpolation in `@import` and later `@use` rules, which is unfortunately a
non-starter as it violates the [module system goal] of static analyzability.
Instead, the [`meta.load-css()` mixin] was added, which includes CSS from a
stylesheet based on an argument which can be dynamic.

[module system goal]: ../accepted/module-system.md#low-level
[`meta.load-css()` mixin]: ../accepted/module-system.md#metaload-css

This has one notable drawback, though: it doesn't provide any means of accessing
stylesheet *members* dynamically. `meta.load-css()` evaluates the stylesheet's
module to CSS and then discards it, leaving any variables, mixins, or functions
it defined inaccessible to the caller. Although the CSS is the primary use case
of dynamic includes, this is a notable missing feature.

## Summary

> This section is non-normative.

This proposal adds a new data type, "module", which represents a loaded Sass
module. As with first-class functions and mixins, this type can't be constructed
literally, but can be returned by Sass core library functions. Values of this
type can be used as the `$module` parameter in
[`meta.global-variable-exists()`], [`meta.module-variables()`],
[`meta.function-exists()`], [`meta.get-function()`],
[`meta.module-functions()`], [`meta.mixin-exists()`], [`meta.get-mixin()`], and
[`meta.module-mixins()`], allowing the caller to access its variables,
functions, and mixins.

[`meta.function-exists()`]: https://sass-lang.com/documentation/modules/meta/#function-exists
[`meta.get-function()`]: https://sass-lang.com/documentation/modules/meta/#get-function
[`meta.module-functions()`]: https://sass-lang.com/documentation/modules/meta/#module-functions
[`meta.global-variable-exists()`]: https://sass-lang.com/documentation/modules/meta/#global-variable-exists
[`meta.module-variables()`]: https://sass-lang.com/documentation/modules/meta/#module-variables
[`meta.mixin-exists()`]: https://sass-lang.com/documentation/modules/meta/#mixin-exists
[`meta.get-mixin()`]: https://sass-lang.com/documentation/modules/meta/#get-mixin
[`meta.module-mixins()`]: https://sass-lang.com/documentation/modules/meta/#module-mixins

This proposal adds two new functions:

* `meta.load($url, $with: null)` takes the same arguments as `meta.load-css()`,
  but instead of directly including the CSS it returns the loaded module.

* `meta.get-module($module)` takes the namespace of a `@use` rule in the current
  stylesheet and returns the module loaded by that rule. This does *not*
  re-evaluate the module.

It also adds one new mixin:

* `meta.css($module)` includes a copy of the given module's CSS, like
  `meta.load-css()` but for an already-loaded module.

### Design Decisions

#### A New Type

The decision to define a new type, rather than returning a more *ad hoc* map of
values, was chosen for two reasons:

1. If we returned a map, it would represent only a snapshot of the module at
   that point in time. For functions, mixins, and CSS, this would be fine;
   they're structurally immutable, and functions and mixins would still be able
   to access the module's mutable state. However, for variables it poses a
   problem, since the user may wish to have visibility into the changing
   values of the module's variables over time.

2. It makes it much simpler to define APIs that consistently work across both
   statically- and dynamically-loaded modules. For example, if we later wanted
   to add a `meta.module-url()` function that returned a module's canonical URL,
   we wouldn't have to define it separately for the module map value.

#### Inspect Format

We could emit a module's full canonical URL when inspecting the object. However,
this could hypothetically provide a vector for inspecting a user's internal
filesystem structure from Sass code. That information is likely low-sensitivity
(as indicated by other languages providing easy access to it) and Sass doesn't
provide any built-in way to expropriate that information anyway; however,
there's little harm in being careful, so instead we just show the module's
basename.

This decision also matches the serialization of first-class functions and
mixins, in that the serialized format mirrors the `meta.get-*()` function used
to access the value.

#### No JavaScript Constructor

It's possible that being able to construct a module through JavaScript will be
desirable in the future as a means of allowing Sass plugins to define custom
Sass APIs beyond top-level functions (which have known forwards-compatibility
risks with CSS). However, we don't want to commit to a design for a constructor
without knowing the design for plugin-provided modules, so we leave this for a
future proposal.

#### No JavaScript Members

The JavaScript API for a first-class module is left intentionally minimal by
this proposal. Although it could make sense to expose the same information to
JavaScript as is exposed to Sass (the module's variables, mixins, and
functions), doing so would substantially complicate the embedded protocol in
particular. This would require some combination of including a bunch of extra
metadata about available functions and mixins in the module proto, making it
heavier-weight than would typically be useful, or adding a large number of
additional incoming requests to get information.

A better solution would be to have a general way for the embedded host to invoke
specific Sass functions, so it could naturally access the same information as
Sass, but that's out of scope for this proposal.

## Types

This proposal promotes the [module value] to a Sass value type.

[module value]: ../spec/modules.md#module

### Operations

The only operation permitted for module objects is checking for equality. All
other operations throw an error.

#### Equality

Module values use identity equality.

> In practice, this means that any two module values with the same canonical URL
> are equal, because Sass's module-loading operation will return an
> already-loaded module if it exists.

### Serialization

To serialize a module:

* If the value is not being inspected, throw an error.

* Let `namespace` be the result of [determining the namespace] for a `@use` rule
  whose URL is the module's canonical URL.

* If `namespace` is null, emit `"get-module()"`.

* Otherwise:

  * Emit `"get-module("`.

  * Emit `namespace`, serialized as a quoted string.

  * Emit `")"`.

[determining the namespace]: ../spec/at-rules/use.md#determining-a-use-rules-namespace

## Functions

### `meta.type-of()`

Add the following clause to the [`meta.type-of()`] function and the top-level
`type-of()` function:

[`meta.type-of()`]: ../spec/built-in-modules/meta.md#type-of

* If `$value` is a module, return an unquoted string with value `"module"`.

### `meta.load()`

Add a new function to the `sass:meta` module:

```
meta.load($url, $with: null)
```

* If `$url` isn't a string, throw an error.

* If `$with` isn't null or a map, or if it's a map with any keys that aren't
  strings, throw an error.

* Let `config` be a configuration whose variable names and values are given by
  `$with` if `$with` isn't null, or the empty configuration otherwise.

* Return the result of [loading] `$url` with `config`.

  [loading]: ../spec/modules.md#loading-a-module

  > Importantly, merely loading a module does not emit its CSS.

### `meta.get-module()`

Add a new function to the `sass:meta` module:

```
meta.get-module($module)
```

* If `$module` is a module, return it.

* Otherwise, if `$module` isn't a string, throw an error.

* Otherwise, let `use` be the `@use` rule in [the current source file] whose
  namespace is equal to `$module`. If no such rule exists, throw an error.

  [the current source file]: ../spec/spec.md#current-source-file

* Return [`use`'s module].

  [`use`'s module]: ../spec/at-rules/use.md#a-use-rules-module

### Existing Functions

For the functions in `sass:meta` [`meta.global-variable-exists()`],
[`meta.module-variables()`], [`meta.function-exists()`],
[`meta.get-function()`], [`meta.module-functions()`], [`meta.mixin-exists()`],
[`meta.get-mixin()`], and [`meta.module-mixins()`]:

* Replace all logic to do with the `$module` parameter with "Let `module` be the
  result of calling [`get-module($module)`]".

  [`get-module($module)`]: #metaget-module

* Replace the text "`use`'s module" with "`module`".

## Mixins

### `meta.css()`

Add a new mixin to the `sass:meta` module:

```
meta.css($module)
```

* Let `module` be the result of calling [`get-module($module)`].

* Let `css` be the result of [resolving `module`'s extensions].

  [resolving `module`'s extensions]: ../spec/at-rules/extend.md#resolving-a-modules-extensions

  > This means that, if `module` shares some dependencies with the entrypoint
  > module, those dependencies' CSS will be included twice.

* Treat `css` as though it were the contents of the mixin.

## JavaScript API

```ts
import {Value} from '../spec/js-api/value';
```

### Types

#### `SassModule`

The JS API representation of a Sass module.

```ts
export class SassModule extends Value {
```

##### `assertModule`

Returns `this` if it's a [`SassModule`] and throws an error otherwise.

[`SassModule`]: #sassmodule

> The `name` parameter may be used for error reporting.

```ts
assertModule(name?: string): SassModule;
```

##### `internal`

The [private `internal` field] refers to a Sass module.

[private `internal` field]: ../spec/js-api/value/index.d.ts.md#internal

##### Constructor

Throws an error.

```ts
constructor();
```

```ts
} // SassModule
```

## Embedded Protocol

This document proposes adding a new value:

```proto
// A first-class module loaded by the compiler. New `CompilerModule`s may only
// be created by the compiler, but the host may pass `CompilerModule`s back to
// the compiler as long as their IDs match IDs of modules received by the host
// during that same compilation.
message CompilerModule {
  // A unique ID for this module. The compiler is responsible for generating
  // this ID and ensuring it's unique across all modules passed to the host
  // for this compilation. Mandatory.
  uint32 id = 1;
}
```

The protocol allows first-class modules loaded by the compiler to be passed to
the host and vice-versa as `Value.CompilerModule`s.

Two first-class modules are equal if they have the same ID.
