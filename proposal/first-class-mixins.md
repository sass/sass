# First Class Mixins: Draft 1

*([Issue](https://github.com/sass/sass/issues/626))*

This proposal promotes mixins to first-class values and adds members to the
`sass:meta` module for manipulating them.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Functions](#functions)
  * [`get-mixin()`](#get-mixin)
  * [`module-mixins()`](#module-mixins)
* [Mixins](#mixins)
  * [`apply()`](#apply)

## Background

> This section is non-normative.

Composability is a very powerful feature typically seen in functional languages
and scripting languages. Sass partially supports this, but only for one of its
[two "colors" of callables](http://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/).

## Summary

> This section is non-normative.

This proposal promotes mixins to first-class values, giving sass composability
for both of its "colors" of callables.

Additionally, it provides ways to interact with mixin values similarly to how
you would with function values:

* `get-function()` → `get-mixin()`

* `module-functions()` → `module-mixins()`

* `call()` → `apply()`

## Functions

### `get-mixin()`

```
get-mixin($name, $module: null)
```

* If `$name` isn't a string, throw an error.

* If `$module` is null:

  * Return the result of resolving a mixin named `$name`. If this returns
    null, throw an error.

* Otherwise:
  
  * If `$module` isn't a string, throw an error.

  * Let `use` be the `@use` rule in [the current source file][] whose
    namespace is equal to `$module`. If no such rule exists, throw an error.

  * Return [`use`'s module][]'s mixin named `$name`, or throw an error if no
    such mixin exists.

### `module-mixins()`

```
module-mixins($module)
```

* If `$module` isn't a string, throw an error.

* Let `use` be the `@use` rule in [the current source file][] whose namespace is
  equal to `$module`. If no such rule exists, throw an error.

* Return a map whose keys are the names of mixins in [`use`'s module][] and
  whose values are the corresponding mixins.

## Mixins

### `apply()`

```
apply($mixin, $args...)
```

> Mixins must be invoked with `@include`, so `include` is guaranted to exist.

* If `$mixin` isn't a mixin, throw an error.

* If `include` has a `ContentBlock`, it should be passed down to `$mixin`.

* Let `content` be the result of executing `$mixin` with `$args`.

* Treat `content` as though it were the contents of this mixin.
