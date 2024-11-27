# Functions

## Table of Contents

* [Definitions](#definitions)
  * [Special Number](#special-number)
  * [Special Variable String](#special-variable-string)
* [Syntax](#syntax)
* [Semantics](#semantics)
  * [`EmptyFallbackVar`](#emptyfallbackvar)
  * [`FunctionCall`](#functioncall)
* [Global Functions](#global-functions)
  * [`adjust-hue()`](#adjust-hue)
  * [`alpha()`](#alpha)
  * [`color()`](#color)
  * [`hsl()` and `hsla()`](#hsl-and-hsla)
  * [`hwb()`](#hwb)
  * [`if()`](#if)
  * [`lab()`](#lab)
  * [`lch()`](#lch)
  * [`oklab()`](#oklab)
  * [`oklch()`](#oklch)
  * [`rgb()` and `rgba()`](#rgb-and-rgba)

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
**FunctionExpression**¹ ::= [SpecialFunctionExpression]
&#32;                     | EmptyFallbackVar
&#32;                     | FunctionCall
**EmptyFallbackVar**²   ::= 'var(' Expression ',' ')'
**FunctionCall**⁴       ::= [NamespacedIdentifier] ArgumentInvocation
</pre></x>

[SpecialFunctionExpression]: syntax.md#specialfunctionexpression
[NamespacedIdentifier]: modules.md#syntax

1: Both `CssMinMax` and `EmptyFallbackVar` take precedence over `FunctionCall`
   if either could be consumed.

2: `'var('` is matched case-insensitively.

4: `FunctionCall` may not have any whitespace between the `NamespacedIdentifier`
   and the `ArgumentInvocation`. It may not start with [`SpecialFunctionName`],
   `'calc('`, or `'clamp('` (case-insensitively).

[`SpecialFunctionName`]: syntax.md#specialfunctionexpression

<x><pre>
**FunctionCall** ::= [NamespacedIdentifier] ArgumentInvocation
</pre></x>

No whitespace is allowed between the `NamespacedIdentifier` and the
`ArgumentInvocation` in `FunctionCall`.

## Semantics

### `EmptyFallbackVar`

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

* Let `function` be the result of [resolving a function] named `name`.

* If `function` is null and `name` is not a plain `Identifier`, throw an error.

* If `function` is null; `name` is case-insensitively equal to `"min"`, `"max"`,
  `"round"`, or `"abs"`; `call`'s `ArgumentInvocation` doesn't have any
  `KeywordArgument`s or `RestArgument`s; and all arguments in `call`'s
  `ArgumentInvocation` are [calculation-safe], return the result of evaluating
  `call` [as a calculation].

  [calculation-safe]: types/calculation.md#calculation-safe-expression
  [as a calculation]: types/calculation.md#evaluating-a-functioncall-as-a-calculation

  > For calculation functions that overlap with global Sass function names, we
  > want anything Sass-specific like this to end up calling the Sass function.
  > For all other calculation functions, we want those constructs to throw an
  > error (which they do when evaluating `call` [as a calculation]).

* If `function` is null and `name` is case-insensitively equal to `"calc"`,
  `"clamp"`, `"hypot"`, `"sin"`, `"cos"`, `"tan"`, `"asin"`, `"acos"`, `"atan"`,
  `"sqrt"`, `"exp"`, `"sign"`, `"mod"`, `"rem"`, `"atan2"`, `"pow"`, `"log"`, or
  `"calc-size"`, return the result of evaluating `call` [as a calculation].

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

> While most built-in Sass functions are defined in [built-in modules], a few
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

[converting]: types/number.md#converting-a-number-to-a-unit
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
  > function].

  [`alpha()` function]: https://blogs.msdn.microsoft.com/ie/2010/08/17/ie9-opacity-and-alpha/

  * If `$args` is empty, throw an error.

  * If `$args` has any keyword arguments, throw an error.

  * Unless all arguments of `$args` are unquoted strings that begin with a
    sequence of ASCII letters, followed by one or more spaces, followed by `=`
    throw an error.

  * Return a plain CSS function string with the name `"alpha"` and the arguments
    `$args`.

### `color()`

* ```
  color($description)
  ```

  * Let `parsed` be the result of [parsing] `$description` without a space.

  * If `parsed` is a string, return a plain CSS function string with the name
    "color" and the argument `parsed`.

  * Let `space` be the color space, `channels` the channel list, and `alpha`
    the alpha value of `parsed`.

  * Return a color in `space`, with the given `channels` and `alpha` value.

### `hsl()` and `hsla()`

The `hsla()` function is identical to `hsl()`, except that if it would return a
plain CSS function named "hsl" that function is named "hsla" instead.

* ```
  hsl($hue, $saturation, $lightness, $alpha: 1)
  ```

  * If any argument is an unquoted string that's case-insensitively equal to
    "none", throw an error.

    > Missing channels are not allowed in legacy syntax.

  * If any argument is a [special number], return a plain CSS function string
    with the name "hsl" and the arguments `$hue`, `$saturation`, `$lightness`,
    and `$alpha`.

  * If `$alpha` is not a number, throw an error.

  * Let `alpha` be the result of [percent-converting] `alpha` with a max of 1,
    and then clamping the value between 0 and 1, inclusive.

  * Let `hue`, `saturation`, and `lightness` be the three elements returned
    by [normalizing] `($hue, $saturation, $lightness)` in the
    [known color space] named `hsl`.

  > Conversion to rgb has been removed.

  * Return an `hsl` color with the given `hue`, `saturation`, and `lightness`
    channels, and `alpha` value.

* ```
  hsl($hue, $saturation, $lightness)
  ```

  * If any argument is a [special number], return a plain CSS function string
    with the name "hsl" and the arguments `$hue`, `$saturation`, and
    `$lightness`.

  * Otherwise, return the result of calling `hsl($hue, $saturation, $lightness,
    1)`.

* ```
  hsl($hue, $saturation)
  ```

  * If either argument is a [special variable string], return a plain CSS
    function string with the name "hsl" and the same arguments.

  * Otherwise, throw an error.

* ```
  hsl($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `hsl` space.

    > Normalization and clamping is handled as part of the [parsing] process.

  * If `parsed` is a string, return a plain CSS function string with the name
    "hsl" and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `hue`, `saturation`, and `lightness` be the three elements of `channels`.

  * Return an `hsl` color with the given `hue`, `saturation`, and `lightness`
    channels, and `alpha` value.

### `hwb()`

* ```
  hwb($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `hwb` space.

    > Normalization and clamping is handled as part of the [parsing] process.

  * If `parsed` is a string, return a plain CSS function string with the name
    "hwb" and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `hue`, `whiteness`, and `blackness` be the three elements of `channels`.

  * Return an `hwb` color with the given `hue`, `whiteness`, and `blackness`
    channels, and `alpha` value.

[percent-converting]: built-in-modules/color.md#percent-converting-a-number
[normalizing]: built-in-modules/color.md#normalizing-color-channels
[known color space]: types/color.md#known-color-space
[special variable string]: #special-variable-string
[parsing]: built-in-modules/color.md#parsing-color-components

### `if()`

```
if($condition, $if-true, $if-false)
```

### `lab()`

* ```
  lab($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `lab` space.

  * If `parsed` is a string, return a plain CSS function string with the name
    "lab" and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `lightness`, `a`, and `b` be the three elements of `channels`.

  * Return a `lab` color with the given `lightness`, `a`, and `b` channels, and
    `alpha` value.

### `lch()`

* ```
  lch($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `lch` space.

  * If `parsed` is a string, return a plain CSS function string with the name
    "lch" and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `lightness`, `chroma`, and `hue` be the three elements of `channels`.

  * Return an `lch` color with the given `lightness`, `chroma`, and `hue`
    channels, and `alpha` value.

### `oklab()`

* ```
  oklab($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `oklab` space.

  * If `parsed` is a string, return a plain CSS function string with the name
    "oklab" and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `lightness`, `a`, and `b` be the three elements of `channels`.

  * Return an `oklab` color with the given `lightness`, `a`, and `b` channels,
    and `alpha` value.

### `oklch()`

* ```
  oklch($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `oklch` space.

  * If `parsed` is a string, return a plain CSS function string with the name
    "oklch" and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `lightness`, `chroma`, and `hue` be the three elements of `channels`.

  * Return an `oklch` color with the given `lightness`, `chroma`, and `hue`
    channels, and `alpha` value.

### `rgb()` and `rgba()`

The `rgba()` function is identical to `rgb()`, except that if it would return a
plain CSS function named "rgb" that function is named "rgba" instead.

* ```
  rgb($red, $green, $blue, $alpha: 1)
  ```

  * If any argument is an unquoted string that's case-insensitively equal to
    "none", throw an error.

    > Missing channels are not allowed in legacy syntax.

  * If any argument is a [special number], return a plain CSS function
    string with the name "rgb" and the arguments `$red`, `$green`, `$blue`,
    and `$alpha`.

  * If `$alpha` is not a number, throw an error.

  * Let `alpha` be the result of [percent-converting] `alpha` with a max of 1,
    and then clamping the value between 0 and 1, inclusive.

  * Let `red`, `green`, and `blue` be the three elements returned by
    [normalizing] `($red, $green, $blue)` in `rgb` space.

  * Return an `rgb` color with the given `red`, `green`, and `blue` channels,
    and `alpha` value.

* ```
  rgb($red, $green, $blue)
  ```

  * If any argument is a [special number], return a plain CSS function string
    with the name "rgb" and the arguments `$red`, `$green`, and `$blue`.

  * Otherwise, return the result of calling `rgb($red, $green, $blue, 1)`.

* ```
  rgb($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `rgb` space.

  * If `parsed` is a string, return a plain CSS function string with the name
    "rgb" and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `red`, `green`, and `blue` be the three elements of `channels`.

  * Return the result of calling `rgb(red, green, blue, alpha)`.

* ```
  rgb($color, $alpha)
  ```

  * If either argument is a [special variable string], return a plain CSS
    function string with the name "rgb" and the same arguments.

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `rgb()` with `$color`'s red, green, and blue
    channels as unitless number arguments, and `$alpha` as the final argument.

[legacy color]: types/color.md#legacy-color
