# Functions

## Table of Contents

* [Definitions](#definitions)
  * [Special Number](#special-number)
  * [Special Variable String](#special-variable-string)
* [Syntax](#syntax)
* [Semantics](#semantics)
  * [`EmptyFallbackVar`:](#emptyfallbackvar)
  * [`FunctionCall`](#functioncall)
* [Global Functions](#global-functions)
  * [`adjust-hue()`](#adjust-hue)
  * [`alpha()`](#alpha)
  * [`rgb()` and `rgba()`](#rgb-and-rgba)
  * [`hsl()` and `hsla()`](#hsl-and-hsla)
  * [`if()`](#if)

## Definitions

### Special Number

A *special number* is either:

* a [calculation], or
* an unquoted string that CSS will recognize as a function that may return a
  number. For the purposes of Sass, this is any unquoted string that begins with
  `calc(`, `var(`, `env(`, `clamp(`, `min(`, or `max(`. This matching is
  case-insensitive.

[calculation]: types/calculation.md

> Sass functions that shadow CSS functions must work with any invocation that
> CSS allows, which includes allowing special numbers anywhere a number would be
> allowed.

### Special Variable String

A *special variable string* is [special number] that begins with `var(`. This
matching is case-insensitive.

[special number]: #special-number

> Unlike other special numbers, variables can expand into multiple arguments to
> a single function.

## Syntax

<x><pre>
**FunctionExpression**¹ ::= [CssMinMax]
&#32;                     | [SpecialFunctionExpression]
&#32;                     | [CalculationExpression]
&#32;                     | EmptyFallbackVar
&#32;                     | FunctionCall
**EmptyFallbackVar**²   ::= 'var(' Expression ',' ')'
**FunctionCall**⁴       ::= [NamespacedIdentifier] ArgumentInvocation
</pre></x>

[CssMinMax]: types/calculation.md#cssminmax
[SpecialFunctionExpression]: syntax.md#specialfunctionexpression
[CalculationExpression]: types/calculation.md#calculationexpression
[NamespacedIdentifier]: modules.md#syntax

1: Both `CssMinMax` and `EmptyFallbackVar` take precedence over `FunctionCall`
   if either could be consumed.

2: `'var('` is matched case-insensitively.

4: `FunctionCall` may not have any whitespace between the `NamespacedIdentifier`
   and the `ArgumentInvocation`. It may not start with [`SpecialFunctionName`],
   `'calc('`, or `'clamp('` (case-insensitively).

[`SpecialFunctionName`]: #specialfunctionexpression

<x><pre>
**FunctionCall** ::= [NamespacedIdentifier][] ArgumentInvocation
</pre></x>

[NamespacedIdentifier]: modules.md#syntax

No whitespace is allowed between the `NamespacedIdentifier` and the
`ArgumentInvocation` in `FunctionCall`.

## Semantics

### `EmptyFallbackVar`:

To evaluate an `EmptyFallbackVar` `call`:

* Let `argument` be the result of evaluating `call`'s `Expression`.

* Let `function` be the result of [resolving a function] named `'var'`.

  [resolving a function]: modules.md#resolving-a-member

* If `function` is null, return an unquoted string consisting of `'var('`
  followed by `argument`'s CSS representation followed by `',)'`.

* Return the result of calling `function` with `argument` as its first argument
  and an empty unquoted string as its second argument.

### `FunctionCall`

To evaluate a `FunctionCall` `call`:

* Let `name` be `call`'s `NamespacedIdentifier`.

* Let `function` be the result of [resolving a function][] named `name`.

* If `function` is null and `name` is not a plain `Identifier`, throw an error.

* If `function` is null, set it to the [global function](#global-functions)
  named `name`.

* If `function` is still null:

  * Let `list` be the result of evaluating `call`'s `ArgumentInvocation`.

  * If `list` has keywords, throw an error.

  * Return an unquoted string representing a CSS function call with name `name`
    and arguments `list`.

* Execute `call`'s `ArgumentInvocation` with `function`'s `ArgumentDeclaration`
  in `function`'s scope.

* Execute each statement in `function` until a `ReturnRule` `return` that's
  lexically contained in `function`'s `Statements` is encountered. If no such
  statement is encountered, throw an error.

* Evaluate `return`'s `Expression` and return the result.

## Global Functions

> While most built-in Sass functions are defined in [built-in modules][], a few
> are globally available with no `@use` necessary. These are mostly functions
> that expand upon the behavior of plain CSS functions.
>
> [built-in modules]: modules.md#built-in-module
>
> In addition, many functions that *are* defined in built-in modules have global
> aliases for backwards-compatibility with stylesheets written before `@use` was
> introduced. These global aliases should be avoided by stylesheet authors if
> possible.

### `adjust-hue()`

```
adjust-hue($color, $degrees)
```

* If `$color` isn't a color or `$degrees` isn't a number, throw an error.

* Let `degrees` be the result of [converting] `$degrees` to `deg` allowing
  unitless.

* Let `saturation` and `lightness` be the result of calling
  [`color.saturation($color)`] and [`color.lightness($color)`], respectively.

* Return the result of calling [`hsl()`] with `degree`, `saturation`,
  `lightness`, and `$color`'s alpha channel.

[`hsl()`]: #hsl-and-hsla
[`color.saturation($color)`]: built-in-modules/color.md#saturation
[`color.lightness($color)`]: built-in-modules/color.md#lightness

### `alpha()`

* ```
  alpha($color)
  ```

  * If `$color` is not a string, call the other overload and return its result.

  * Return the alpha channel of `$color` as a unitless number.

* ```
  alpha($args...)
  ```

  > This overload exists to support Microsoft's proprietary [`alpha()`
  > function][].

  [`alpha()` function]: https://blogs.msdn.microsoft.com/ie/2010/08/17/ie9-opacity-and-alpha/

  * If `$args` is empty, throw an error.

  * If `$args` has any keyword arguments, throw an error.

  * Unless all arguments of `$args` are unquoted strings that begin with a
    sequence of ASCII letters, followed by one or more spaces, followed by `=`
    throw an error.

  * Return a plain CSS function string with the name `"alpha"` and the arguments
    `$args`.

### `rgb()` and `rgba()`

The `rgba()` function is identical to `rgb()`, except that if it would return a
plain CSS function named `"rgb"` that function is named `"rgba"` instead.

* ```
  rgb($red, $green, $blue, $alpha)
  ```

  * If any argument is a [special number], return a plain CSS function
    string with the name `"rgb"` and the arguments `$red`, `$green`, `$blue`,
    and `$alpha`.

  * If any of `$red`, `$green`, `$blue`, or `$alpha` aren't numbers, throw an
    error.

  * Let `red`, `green`, and `blue` be the result of [percent-converting][]
    `$red`, `$green`, and `$blue`, respectively, with a `max` of 255.

  * Let `alpha` be the result of percent-converting `$alpha` with a `max` of 1.

  * Return a color with the given `red`, `green`, `blue`, and `alpha` channels.

  [percent-converting]: built-in-modules/color.md#percent-converting-a-number

* ```
  rgb($red, $green, $blue)
  ```

  * If any argument is a [special number], return a plain CSS function string
    with the name `"rgb"` and the arguments `$red`, `$green`, and `$blue`.

  * Otherwise, return the result of calling `rgb()` with `$red`, `$green`,
    `$blue`, and `1`.

* ```
  rgb($color, $alpha)
  ```

  * If either argument is a [special variable string][], return a plain CSS
    function string with the name `"rgb"` and the same arguments.

  * If `$color` isn't a color, throw an error.

  * Call `rgb()` with `$color`'s red, green, and blue channels as unitless
    number arguments, and with `$alpha` as the final argument. Return the
    result.

* ```
  rgb($channels)
  ```

  * If `$channels` is a [special variable string][], return a plain CSS function
    string with the name `"rgb"` and the argument `$channels`.

  * If `$channels` is an unbracketed slash-separated list:

    * If `$channels` doesn't have exactly two elements, throw an error.
      Otherwise, let `rgb` be the first element and `alpha` the second element.

    * If either `rgb` or `alpha` is a special variable string, return a plain
      CSS function string with the name `"rgb"` and the argument `$channels`.

    * If `rgb` is not an unbracketed space-separated list, throw an error.

    * If `rgb` has more than three elements, throw an error.

    * If `rgb` has fewer than three elements:

      * If any element of `rgb` is a [special variable string][], return a
        plain CSS function string with the name `"rgb"` and the argument
        `$channels`.

      * Otherwise, throw an error.

    * Let `red`, `green`, and `blue` be the three elements of `rgb`.

    * Call `rgb()` with `red`, `green`, `blue`, and `alpha` as arguments and
      return the result.

  * If `$channels` is not an unbracketed space-separated list, throw an error.

  * If `$channels` has more than three elements, throw an error.

  * If `$channels` has fewer than three elements:

    * If any element of `$channels` is a [special variable string][], return a
      plain CSS function string with the name `"rgb"` and the argument
      `$channels`.

    * If the last element of `$channels` is an unquoted string that begins with
      `var(` and contains `/`, return a plain CSS function string with the name
      `"rgb"` and the argument `$channels`.

    * Otherwise, throw an error.

  * Let `red` and `green` be the first two elements of `$channels`.

  * If the third element of `$channels` is an unquoted string that contains `/`:

    * Return a plain CSS function string with the name `"rgb"` and the argument
      `$channels`.

  * Otherwise, if the third element of `$channels` has preserved its status as
    two slash-separated numbers:

    * Let `blue` be the number before the slash and `alpha` the number after the
      slash.

  * Otherwise:

    * Let `blue` be the third element of `$channels`.

  * Call `rgb()` with `red`, `green`, `blue`, and `alpha` (if it's defined) as
    arguments and return the result.

  [special variable string]: #special-variable-string

### `hsl()` and `hsla()`

The `hsla()` function is identical to `hsl()`, except that if it would return a
plain CSS function named `"hsl"` that function is named `"hsla"` instead.

* ```
  hsl($hue, $saturation, $lightness, $alpha: 1)
  ```

  * If any argument is a [special number], return a plain CSS function
    string with the name `"hsl"` and the arguments `$hue`, `$saturation`,
    `$lightness`, and `$alpha`.

  * If any of `$hue`, `$saturation`, `$lightness`, or `$alpha` aren't numbers,
    throw an error.

  * Let `hue` be the result of [converting] `$hue` to `deg` allowing unitless.

  * If `$saturation` and `$lightness` don't have unit `%`, throw an error.

  * Let `saturation` and `lightness` be the result of clamping `$saturation` and
    `$lightness`, respectively, between `0%` and `100%` and dividing by `100%`.

  * Let `red`, `green`, and `blue` be the result of converting `hue`,
    `saturation`, and `lightness` [to RGB][].

  * Set `red`, `green`, and `blue` to their existing values multiplied by 255
    and rounded to the nearest integers.

  * Let `alpha` be the result of [percent-converting][] `$alpha` with a `max` of 1.

  * Return a color with the given `red`, `green`, `blue`, and `alpha` channels.

  [converting]: types/number.md#converting-a-number-to-a-unit
  [to RGB]: https://www.w3.org/TR/css-color-4/#hsl-to-rgb

* ```
  hsl($hue, $saturation, $lightness)
  ```

  * If any argument is a [special number], return a plain CSS function string
    with the name `"hsl"` and the arguments `$hue`, `$saturation`, and
    `$lightness`.

  * Otherwise, return the result of calling `hsl()` with `$hue`, `$saturation`,
    `$lightness`, and `1`.

* ```
  hsl($hue, $saturation)
  ```

  * If either argument is a [special variable string][], return a plain CSS
    function string with the name `"hsl"` and the same arguments.

  * Otherwise, throw an error.

* ```
  hsl($channels)
  ```

  * If `$channels` is a [special variable string][], return a plain CSS function
    string with the name `"hsl"` and the argument `$channels`.

  * If `$channels` is an unbracketed slash-separated list:

    * If `$channels` doesn't have exactly two elements, throw an error.
      Otherwise, let `hsl` be the first element and `alpha` the second element.

    * If either `hsl` or `alpha` is a special variable string, return a plain
      CSS function string with the name `"hsl"` and the argument `$channels`.

    * If `hsl` is not an unbracketed space-separated list, throw an error.

    * If `hsl` has more than three elements, throw an error.

    * If `hsl` has fewer than three elements:

      * If any element of `hsl` is a [special variable string][], return a
        plain CSS function string with the name `"hsl"` and the argument
        `$channels`.

      * Otherwise, throw an error.

    * Let `hue`, `saturation`, and `lightness` be the three elements of `hsl`.

    * Call `hsl()` with `hue`, `saturation`, `lightness`, and `alpha` as
      arguments and return the result.

  * If `$channels` is not an unbracketed space-separated list, throw an error.

  * If `$channels` has more than three elements, throw an error.

  * If `$channels` has fewer than three elements:

    * If any element of `$channels` is a [special variable string][], return a
      plain CSS function string with the name `"hsl"` and the argument
      `$channels`.

    * If the last element of `$channels` is an unquoted string that begins with
      `var(` and contains `/`, return a plain CSS function string with the name
      `"hsl"` and the argument `$channels`.

    * Otherwise, throw an error.

  * Let `hue` and `saturation` be the first two elements of `$channels`.

  * If the third element of `$channels` is an unquoted string that contains `/`:

    * Return a plain CSS function string with the name `"rgb"` and the argument
      `$channels`.

  * Otherwise, if the third element of `$channels` has preserved its status as
    two slash-separated numbers:

    * Let `lightness` be the number before the slash and `alpha` the number
      after the slash.

  * Otherwise:

    * Let `lightness` be the third element of `$channels`.

  * Call `hsl()` with `hue`, `saturation`, `lightness`, and `alpha` (if it's
    defined) as arguments and return the result.

  [special variable string]: #special-variable-string

### `if()`

```
if($condition, $if-true, $if-false)
```
