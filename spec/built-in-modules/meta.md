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
  * [`get-module()`](#get-module)
  * [`global-variable-exists()`](#global-variable-exists)
  * [`inspect()`](#inspect)
  * [`keywords()`](#keywords)
  * [`load()`](#load)
  * [`mixin-exists()`](#mixin-exists)
  * [`module-functions()`](#module-functions)
  * [`module-mixins()`](#module-mixins)
  * [`module-variables()`](#module-variables)
  * [`type-of()`](#type-of)
  * [`variable-exists()`](#variable-exists)
* [Mixins](#mixins)
  * [`apply()`](#apply)
  * [`css()`](#css)
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

* Return `$calc`'s name [as an identifier].

[as an identifier]: ../types/string.md#converting-a-string-to-an-identifier

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

* Let `name` be `$name`'s [semantic value].

  [semantic value]: ../types/string.md#getting-the-semantic-value-of-a-string

* If `$module` is null:

  * Return whether [resolving a function] named `name` returns null.

  [resolving a function]: ../modules.md#resolving-a-member

* Let `module` be the result of calling [`get-module($module)`].

  [`get-module($module)`]: #get-module

* Return whether `module` contains a function named `name`.

  [`use`'s module]: ../at-rules/use.md#a-use-rules-module

### `get-function()`

```
get-function($name, $css: false, $module: null)
```

This function is also available as a global function named `get-function()`.

* If `$name` is not a string, throw an error.

* Let `name` be `$name`'s [semantic value].

* If `$module` is null:

  * If `$css` is falsey:

    * Return the result of [resolving a function] named `name`. If this returns
      null, throw an error.

  * Otherwise, return a function object that takes arguments `($args...)`. When
    this function is called:

    * If `$args` has any keyword arguments, throw an error.

    * Return a plain CSS function string whose name is `name` [as an identifier]
      and whose arguments are `$args`.

* Otherwise:

  * If `$module` isn't a string, throw an error.

  * If `$css` is truthy, throw an error.

  * Let `use` be the `@use` rule in [the current source file] whose
    namespace is equal to `$module`. If no such rule exists, throw an error.

  * Return [`use`'s module]'s function named `name`, or throw an error if no
    such function exists.

### `get-mixin()`

```
get-mixin($name, $module: null)
```

* If `$name` is not a string, throw an error.

* Let `name` be `$name`'s [semantic value].

* If `$module` is null:

  * Return the result of [resolving a mixin] named `name`. If this returns
    null, throw an error.

  [resolving a mixin]: ../modules.md#resolving-a-member

* Otherwise, let `module` be the result of calling [`get-module($module)`].

* Return `module`'s mixin named `name`, or throw an error if no such mixin
  exists.

### `get-module()`

```
get-module($module)
```

* If `$module` is a module, return it.

* Otherwise, if `$module` isn't a string, throw an error.

* Let `namespace` be `$module`'s [semantic value].

* Otherwise, let `use` be the `@use` rule in [the current source file] whose
  namespace is equal to `namespace`. If no such rule exists, throw an error.

  [the current source file]: ../spec.md#current-source-file

* Return [`use`'s module].

### `global-variable-exists()`

```
global-variable-exists($name, $module: null)
```

This function is also available as a global function named `global-variable-exists()`.

* If `$name` is not a string, throw an error.

* Let `name` be `$name`'s [semantic value].

* If `$module` is null:

  * Return whether [resolving a variable] named `name`, ignoring local
    scopes, returns null.

  [resolving a variable]: ../modules.md#resolving-a-member

* Let `module` be the result of calling [`get-module($module)`].

* Return whether `module` contains a variable named `name`.

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

* If `$args` is not an argument list, throw an error.

* Return a map whose keys are the named arguments in `$args` [as identifiers]
  and whose values are those arguments' values.

[as identifiers]: ../types/string.md#converting-a-string-to-an-identifier

### `load()`

```
load($url, $with: null)
```

* If `$url` isn't a string, throw an error.

* If `$with` isn't null or a map, or if it's a map with any keys that aren't
  strings, throw an error.

* Let `config` be the empty configuration if `$with` is null.

* Otherwise, let `config` be a configuration whose variable names are the
  [semantic values] of `$with`'s keys and whose values are those keys' values.
  Throw an error if multiple keys have the same semantic value.

* Return the result of [loading] `$url` with `config`.

  [loading]: ../modules.md#loading-a-module

  > Importantly, merely loading a module does not emit its CSS.

[semantic values]: ../types/string.md#getting-the-semantic-value-of-a-string

### `mixin-exists()`

```
mixin-exists($name, $module: null)
```

This function is also available as a global function named `mixin-exists()`.

* If `$name` is not a string, throw an error.

* Let `name` be `$name`'s [semantic value].

* If `$module` is null:

  * Return whether [resolving a mixin] named `name` returns null.

* Otherwise, let `module` be the result of calling [`get-module($module)`].

* Return whether `module` contains a mixin named `name`.

### `module-functions()`

```
module-functions($module)
```

This function is also available as a global function named `module-functions()`.

* Let `module` be the result of calling [`get-module($module)`].

* Return a map whose keys are the names of functions in `module` and whose
  values are the corresponding functions.

### `module-mixins()`

This is a new function in the `sass:meta` module.

```
module-mixins($module)
```

* Let `module` be the result of calling [`get-module($module)`].

* Return a map whose keys are the names of mixins in `module` [as identifiers]
  and whose values are the corresponding mixins.

### `module-variables()`

```
module-variables($module)
```

This function is also available as a global function named `module-variables()`.

* Let `module` be the result of calling [`get-module($module)`].

* Return a map whose keys are the names (without `$`) of variables in `module`
  [as identifiers] and whose values are the corresponding values.

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
  | Module        | `"module"`      |
  | Null          | `"null"`        |
  | Number        | `"number"`      |
  | String        | `"string"`      |

### `variable-exists()`

```
variable-exists($name, $module: null)
```

This function is also available as a global function named `variable-exists()`.

* If `$name` is not a string, throw an error.

* Let `name` be `$name`'s [semantic value].

* If `$module` is null:

  * Return whether [resolving a variable] named `name` returns null.

* Otherwise, if `$module` isn't a string, throw an error.

* Otherwise, let `use` be the `@use` rule in [the current source file] whose
  namespace is equal to `$module`. If no such rule exists, throw an error.

* Return whether [`use`'s module] contains a mixin named `name`.

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

[`ArgumentList`]: ../arguments.md#argumentlist

### `css()`

```
css($module)
```

* Let `module` be the result of calling [`get-module($module)`].

* Let `css` be the result of [resolving `module`'s extensions].

  [resolving `module`'s extensions]: ../at-rules/extend.md#resolving-a-modules-extensions

  > This means that, if `module` shares some dependencies with the entrypoint
  > module, those dependencies' CSS will be included twice.

* Treat `css` as though it were the contents of the mixin.

### `load-css()`

```
load-css($url, $with: null)
```

* Let `module` be the result of calling [`load($url, $with)`].

  [`load($url, $with)`]: #load

* Include [`css(module)`].

  [`css(module)`]: #css
