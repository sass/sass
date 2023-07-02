# First Class Mixins: Draft 1

*([Issue](https://github.com/sass/sass/issues/626))*

This proposal promotes mixins to first-class values and adds members to the
`sass:meta` module for manipulating them.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Types](#types)
  * [Operations](#operations)
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

This proposal introduces a new value type known as a "mixin", with the
following structure:

```ts
interface Mixin {
  /**
   * The name of this mixin
   */
  name: string;

  /**
   * Whether or not the body of this mixin contains the `@content` rule
   */
  hasContent: boolean;
}
```

> The internal representation of a mixin may also include function arguments, a body, a span, or other fields, but these properties are not meaningfully exposed to users.
>
> When a mixin value is created, it captures the current execution environment and a pointer to the mixin object being referenced. This is also not directly exposed to users.

### Operations

The only operation permitted for mixin objects is checking for equality. 

#### Equality

Two mixins are considered equal if they refer to the exact same mixin as declared in the source code.

> Even if two mixin objects have the same name and the exact same body, they may not be considered equal. This can be thought of as pointer equality and models what one would expect when comparing two function pointers.
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
> Although every aspect of the two mixins is the same, `$a != $b`, because they refer to separate mixin declarations/objects.

### Serialization

To serialize a `Mixin`:

* If the current evaluation context is plain CSS, throw an error.

* If the value is not being inspected, throw an error.

* Otherwise:

  * Emit `"get-mixin("`.

  * Emit a double quote (`"`), then the `name` of the mixin, then another double quote.

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
get-mixin($name, $module: null)
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

### `meta.module-mixins()`

This is a new function in the `sass:meta` module.

```
meta.module-mixins($module)
```

* If `$module` is not a string, throw an error.

* Let `use` be the `@use` rule in [the current source file][] whose namespace is
  equal to `$module`. If no such rule exists, throw an error.

* Return a map whose keys are the quoted string names of mixins in [`use`'s module][] and
  whose values are the corresponding mixins.

### `meta.accepts-content()`

This is a new function in the `sass:meta` module.

```
meta.accepts-content($mixin)
```

* If `$mixin` is not a mixin, throw an error.

* Return a boolean which is true if the body of the mixin has an `@content` rule. This is the same value as `mixin.hasContent`.

## Mixins

### `meta.apply()`

```
meta.apply($mixin, $args...)
```

> Mixins must be invoked with `@include`, so `include` is guaranted to exist.

* If `$mixin` is not a mixin, throw an error.

* If `include` has a `ContentBlock`, it should be passed down to `$mixin`.

* Assign $args to `$mixin`'s ArgumentDeclaration in `$mixin`'s scope.

* Execute each statement in `$mixin`.

* Let `content` be the result of executing `$mixin` with `$args`.

* Treat `content` as though it were the contents of this mixin.
