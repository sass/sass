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
## Background

> This section is non-normative.

Composability is a very powerful feature typically seen in functional languages
and scripting languages. Sass partially supports this, but only for one of its
two kinds of callables.

## Summary

> This section is non-normative.

This proposal promotes mixins to first-class values, giving Sass composability
for both of its kinds of callables.

Additionally, it provides ways to interact with mixin values similarly to how
one would with function values:

* `get-function()` → `get-mixin()`

* `module-functions()` → `module-mixins()`

* `call()` → `apply()`

## Types

This proposal promotes the [mixin value][] to a Sass value type.

[mixin value]: /spec/at-rules/mixin.md#mixin

### Operations

The only operation permitted for mixin objects is checking for equality. All
other operations throw an error.

#### Equality

Two user-declared mixins are considered equal if they refer to the exact same
mixin as declared in the source code.

Builtin mixins -- i.e. those declared by the Sass standard library, and not declared directly in Sass -- are equal to only themselves.

> Even if two mixin objects have the same name and the exact same body, they
> may not be considered equal. This can be thought of as pointer equality and
> models what one would expect when comparing two function pointers.
>
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

[`meta.type-of()`]: /spec/built-in-modules/meta.md#type-of

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

  [the current source file]: /spec/spec.md#current-source-file
  [`use`'s module]: /spec/at-rules/use.md#a-use-rules-module

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
  to `$mixin`. Mixins must be invoked with `@include`, so `include` is
  guaranteed to exist.

* Create an `ArgumentInvocation` from `(...$args)`

* Execute the `ArgumentInvocation` `(...$args)` with `$mixin`'s `ArgumentDeclaration` in `$mixin`'s scope

* Execute each statement in `$mixin`.
