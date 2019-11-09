# Functions

## Table of Contents

* [Definitions](#definitions)
  * [Special Number String](#special-number-string)
  * [Special Variable String](#special-variable-string)
* [Syntax](#syntax)
* [Semantics](#semantics)
* [Global Functions](#global-functions)
  * [`rgb()` and `rgba()`](#rgb-and-rgba)
  * [`hsl()` and `hsla()`](#hsl-and-hsla)
  * [`if()`](#if)

## Definitions

### Special Number String

A *special number string* is an unquoted string that CSS will recognize as a
function that may return a number. For the purposes of Sass, this is any
unquoted string that begins with `calc(`, `var(`, `env(`, `min(`, or `max(`.
This matching is case-insensitive.

> Sass functions that shadow CSS functions must work with any invocation that
> CSS allows, which includes allowing special number strings anywhere a number
> would be allowed.

### Special Variable String

A *special variable string* is [special number string][] that begins with
`var(`. This matching is case-insensitive.

[special number string]: #special-number-string

> Unlike other special number strings, variables can expand into multiple
> arguments to a single function.

## Syntax

<x><pre>
**FunctionCall** ::= [NamespacedIdentifier][] ArgumentInvocation
</pre></x>

[NamespacedIdentifier]: modules.md#syntax

No whitespace is allowed between the `NamespacedIdentifier` and the
`ArgumentInvocation` in `FunctionCall`.

## Semantics

To evaluate a `FunctionCall` `call`:

* Let `name` be `call`'s `NamespacedIdentifier`.

* Let `function` be the result of [resolving a function][] named `name`.

  [resolving a function]: ../modules.md#resolving-a-member

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
> [built-in modules]: modules.md#built-in-modules
>
> In addition, many functions that *are* defined in built-in modules have global
> aliases for backwards-compatibility with stylesheets written before `@use` was
> introduced. These global aliases should be avoided by stylesheet authors if
> possible.

### `rgb()` and `rgba()`

The `rgba()` function is identical to `rgb()`, except that if it would return a
plain CSS function named `"rgb"` that function is named `"rgba"` instead.

* ```
  rgb($red, $green, $blue, $alpha)
  ```

  * If any argument is a [special number string][], return a plain CSS function
    string with the name `"rgb"` and the arguments `$red`, `$green`, `$blue`,
    and `$alpha`.

  * If any of `$red`, `$green`, `$blue`, or `$alpha` aren't numbers, throw an
    error.

  * Let `red`, `green`, and `blue` be the result of [percent-converting][]
    `$red`, `$green`, and `$blue`, respectively, with a `max` of 255.

    [percent-converting]: built_in_modules/color.md#percent-converting-a-number

  * Let `alpha` be the result of percent-converting `$alpha` with a `max` of 1.

  * Return a color with the given `red`, `green`, `blue`, and `alpha` channels.

* ```
  rgb($red, $green, $blue)
  ```

  * If any argument is a [special number string][], return a plain CSS function
    string with the name `"rgb"` and the arguments `$red`, `$green`, and
    `$blue`.

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

    [special variable string]: #special-variable-string

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

### `hsl()` and `hsla()`

The `hsla()` function is identical to `hsl()`, except that if it would return a
plain CSS function named `"hsl"` that function is named `"hsla"` instead.

* ```
  hsl($hue, $saturation, $lightness, $alpha: 1)
  ```

  * If any argument is a [special number string][], return a plain CSS function
    string with the name `"hsl"` and the arguments `$hue`, `$saturation`,
    `$lightness`, and `$alpha`.

  * If any of `$hue`, `$saturation`, `$lightness`, or `$alpha` aren't numbers,
    throw an error.

  * Let `hue` be `($hue % 360) / 6`.

  * Let `saturation` and `lightness` be the result of clamping `$saturation` and
    `$lightness`, respectively, between 0 and 100 and dividing by 100.
  
  * Let `red`, `green`, and `blue` be the result of converting `hue`,
    `saturation`, and `lightness` [to RGB][].

    [to RGB]: https://www.w3.org/TR/css-color-4/#hsl-to-rgb

  * Let `alpha` be the result of percent-converting `$alpha` with a `max` of 1.

  * Return a color with the given `red`, `green`, `blue`, and `alpha` channels.

* ```
  hsl($hue, $saturation, $lightness)
  ```

  * If any argument is a [special number strings][], return a plain CSS function
    string with the name `"hsl"` and the arguments `$hue`, `$saturation`, and
    `$lightness`.

    [special number strings]: #special-number-string

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

    [special variable string]: #special-variable-string

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

### `if()`

```
if($condition, $if-true, $if-false)
```
