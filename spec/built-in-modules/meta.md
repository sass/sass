# Meta-Programming Module

This built-in module is available from the URL `sass:meta`.

## Table of Contents

* [Functions](#functions)
  * [`accepts-content()`](#accepts-content)
  * [`calc-name()`](#calc-name)
  * [`calc-args()`](#calc-args)
  * [`call()`](#call)
  * [`content-exists()`](#content-exists)
  * [`feature-exists()`](#feature-exists)
  * [`function-exists()`](#function-exists)
  * [`get-function()`](#get-function)
  * [`get-mixin()`](#get-mixin)
  * [`global-variable-exists()`](#global-variable-exists)
  * [`inspect()`](#inspect)
  * [`keywords()`](#keywords)
  * [`mixin-exists()`](#mixin-exists)
  * [`module-functions()`](#module-functions)
  * [`module-mixins()`](#module-mixins)
  * [`module-variables()`](#module-variables)
  * [`type-of()`](#type-of)
  * [`variable-exists()`](#variable-exists)
* [Mixins](#mixins)
  * [`apply()`](#apply)
  * [`load-css()`](#load-css)

## Functions

### `accepts-content()`

This is a new function in the `sass:meta` module.

```
accepts-content($mixin)
```

* If `$mixin` is not a [mixin], throw an error.

  [mixin]: ../types/mixins.md

* Return whether `$mixin` accepts a content block as a SassScript boolean.

### `calc-name()`

```
calc-name($calc)
```

* If `$calc` is not a calculation, throw an error.

* Return `$calc`'s name as a quoted string.

### `calc-args()`

```
calc-args($calc)
```

* If `$calc` is not a calculation, throw an error.

* Let `args` be an empty list.

* For each argument `arg` in `$calc`'s arguments:

  * If `arg` is a number or a calculation, add it to `args`.

  * Otherwise, [serialize] `arg` and add the result to `args` as an unquoted
    string.

  [serialize]: ../types/calculation.md#serialization

* Return `args` as an unbracketed comma-separated list.

### `call()`

```
call($function, $args...)
```

This function is also available as a global function named `call()`.

### `content-exists()`

```
content-exists()
```

This function is also available as a global function named `content-exists()`.

### `feature-exists()`

```
feature-exists($feature)
```

This function is also available as a global function named `feature-exists()`.

### `function-exists()`

```
function-exists($name, $module: null)
```

This function is also available as a global function named `function-exists()`.

* Emit a deprecation warning named `feature-exists`.

* If `$name` is not a string, throw an error.

* If `$name` is not an [`<ident-token>`], return false.

  [`<ident-token>`]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram

* If `$module` is null:

  * Return whether [resolving a function] named `$name` returns null.

  [resolving a function]: ../modules.md#resolving-a-member

* Otherwise, if `$module` isn't a string, throw an error.

* Otherwise, let `use` be the `@use` rule in [the current source file] whose
  namespace is equal to `$module`. If no such rule exists, throw an error.

  [the current source file]: ../spec.md#current-source-file

* Return whether [`use`'s module] contains a function named `$name`.

  [`use`'s module]: ../at-rules/use.md#a-use-rules-module

### `get-function()`

```
get-function($name, $css: false, $module: null)
```

This function is also available as a global function named `get-function()`.

* If `$name` is not a string, throw an error.

* If `$name` is not an [`<ident-token>`], throw an error.

* If `$module` is null:

  * If `$css` is falsey:

    * Return the result of [resolving a function] named `$name`. If this
      returns null, throw an error.

  * Otherwise, return a function object that takes arguments `($args...)`. When
    this function is called:

    * If `$args` has any keyword arguments, throw an error.

    * Return a plain CSS function string with the name `$name` and the arguments
      `$args`.

* Otherwise:

  * If `$module` isn't a string, throw an error.

  * If `$css` is truthy, throw an error.

  * Let `use` be the `@use` rule in [the current source file] whose
    namespace is equal to `$module`. If no such rule exists, throw an error.

  * Return [`use`'s module]'s function named `$name`, or throw an error if no
    such function exists.

### `get-mixin()`

```
get-mixin($name, $module: null)
```

* If `$name` is not a string, throw an error.

* If `$name` is not an [`<ident-token>`], throw an error.

* If `$module` is null:

  * Return the result of [resolving a mixin] named `$name`. If this returns
    null, throw an error.

  [resolving a mixin]: ../modules.md#resolving-a-member

* Otherwise:

  * If `$module` is not a string, throw an error.

  * Let `use` be the `@use` rule in [the current source file] whose namespace is
    equal to `$module`. If no such rule exists, throw an error.

  * Return [`use`'s module]'s mixin named `$name`, or throw an error if no such
    mixin exists.

### `global-variable-exists()`

```
global-variable-exists($name, $module: null)
```

This function is also available as a global function named `global-variable-exists()`.

* If `$name` is not a string, throw an error.

* If `$name` is not a [`PlainVariable`], return false.

  [`PlainVariable`]: ../variables.md#syntax

* If `$module` is null:

  * Return whether [resolving a variable] named `$name`, ignoring local
    scopes, returns null.

  [resolving a variable]: ../modules.md#resolving-a-member

* Otherwise, if `$module` isn't a string, throw an error.

* Otherwise, let `use` be the `@use` rule in the [current source file] whose
  namespace is equal to `$module`. If no such rule exists, throw an error.

  [current source file]: ../spec.md#current-source-file

* Return whether [`use`'s module] contains a function named `$name`.

### `inspect()`

```
inspect($value)
```

This function is also available as a global function named `inspect()`.

### `keywords()`

```
keywords($args)
```

This function is also available as a global function named `keywords()`.

### `mixin-exists()`

```
mixin-exists($name, $module: null)
```

This function is also available as a global function named `mixin-exists()`.

* If `$name` is not a string, throw an error.

* If `$name` is not an [`<ident-token>`], return false.

* If `$module` is null:

  * Return whether [resolving a mixin] named `$name` returns null.

* Otherwise, if `$module` isn't a string, throw an error.

* Otherwise, let `use` be the `@use` rule in [the current source file] whose
  namespace is equal to `$module`. If no such rule exists, throw an error.

* Return whether [`use`'s module] contains a mixin named `$name`.

### `module-functions()`

```
module-functions($module)
```

This function is also available as a global function named `module-functions()`.

* If `$module` is not a string, throw an error.

* Let `use` be the `@use` rule in [the current source file] whose namespace is
  equal to `$module`. If no such rule exists, throw an error.

* Return a map whose keys are the names of functions in [`use`'s module] and
  whose values are the corresponding functions.

### `module-mixins()`

This is a new function in the `sass:meta` module.

```
module-mixins($module)
```

* If `$module` is not a string, throw an error.

* Let `use` be the `@use` rule in [the current source file] whose namespace is
  equal to `$module`. If no such rule exists, throw an error.

* Return a map whose keys are the quoted string names of mixins in
  [`use`'s module] and whose values are the corresponding mixins.

### `module-variables()`

```
module-variables($module)
```

This function is also available as a global function named `module-variables()`.

* If `$module` is not a string, throw an error.

* Let `use` be the `@use` rule in [the current source file] whose namespace is
  equal to `$module`. If no such rule exists, throw an error.

* Return a map whose keys are the names (without `$`) of variables in [`use`'s
  module] and whose values are the corresponding values.

### `type-of()`

```
type-of($value)
```

This function is also available as a global function named `type-of()`.

* Look up `$value`'s type in the "Type" column of the table below, and return an
  unquoted string whose value is the corresponding cell in the "Result" column:

  | Type          | Result          |
  | ------------- | --------------- |
  | Argument list | `"arglist"`     |
  | Boolean       | `"bool"`        |
  | Calculation   | `"calculation"` |
  | Color         | `"color"`       |
  | Function      | `"function"`    |
  | List          | `"list"`        |
  | Map           | `"map"`         |
  | Mixin         | `"mixin"`       |
  | Null          | `"null"`        |
  | Number        | `"number"`      |
  | String        | `"string"`      |

### `variable-exists()`

```
variable-exists($name, $module: null)
```

This function is also available as a global function named `variable-exists()`.

* If `$name` is not a string, throw an error.

* If `$name` is not a [`PlainVariable`], return false.

* If `$module` is null:

  * Return whether [resolving a variable] named `$name` returns null.

* Otherwise, if `$module` isn't a string, throw an error.

* Otherwise, let `use` be the `@use` rule in [the current source file] whose
  namespace is equal to `$module`. If no such rule exists, throw an error.

* Return whether [`use`'s module] contains a mixin named `$name`.

## Mixins

### `apply()`

```
apply($mixin, $args...)
```

* If `$mixin` is not a [mixin], throw an error.

* If the current `@include` rule has a `ContentBlock` and `$mixin` doesn't
  accept a block, throw an error.

* Execute `$mixin` with the [`ArgumentList`] `(...$args)`. Treat the
  `@include` rule that invoked `apply` as the `@include` rule that invoked
  `$mixin`.

  > This ensures that any `@content` rules in `$mixin` will use `apply()`'s
  > `ContentBlock`.

[`ArgumentList`]: ../syntax.md#argumentlist

### `load-css()`

```
load-css($url, $with: null)
```

* If `$url` isn't a string, throw an error.

* Let `config` be a configuration whose variable names and values are given by
  `$with` if `$with` isn't null, or the empty configuration otherwise.

* Let `module` be the result of [loading] `$url` with `config`.

  [loading]: ../modules.md#loading-a-module

* Let `css` be the result of [resolving `module`'s extensions].

  [resolving `module`'s extensions]: ../at-rules/extend.md#resolving-a-modules-extensions

  > This means that, if a module loaded by `load-css()` shares some dependencies
  > with the entrypoint module, those dependencies' CSS will be included twice.

* Treat `css` as though it were the contents of the mixin.
