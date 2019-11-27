# Color Level 4 `rgb()` and `hsl()` Functions: Draft 1.3

*([Issue](https://github.com/sass/sass/issues/2564), [Changelog](color-4-rgb-hsl.changes.md))*

This proposal expands Sass's built-in `rgb()`, `rgba()`, `hsl()`, and `hsla()`
functions to support the new terse syntax defined in [CSS Color Level 4][].

[CSS Color Level 4]: https://drafts.csswg.org/css-color/

## Table of Contents

* [Summary](#summary)
* [Definitions](#definitions)
  * [Special Number String](#special-number-string)
  * [Special Variable String](#special-variable-string)
* [Semantics](#semantics)

## Summary

> This section is non-normative.

Sass's `rgb()` and `hsl()` functions will add support for the Color Level 4
syntax, so `rgb(179 115 153)`, `rgb(70% 45% 60%)`, and `hsl(324deg 29% 57%)`
will all return the same color. They'll also support the slash-separated alpha
syntax using [the normal rules][] for distinguishing between division and `/`,
so `rgb(179 115 153 / 50%)` will return a 50%-opacity color but
`rgb(179 115 $blue / 50%)` will not.

[the normal rules]: https://sass-lang.com/documentation/file.SASS_REFERENCE.html#division-and-slash

Color Level 4 redefines `rgba()` and `hsla()` as pure aliases for `rgb()` and
`hsl()`, which also means that `rgb()` and `hsl()` support the old syntax for
defining colors with alpha channels. In keeping with this, Sass will redefine
`rgba()` and `hsla()` as aliases for `rgb()` and `hsl()` as well, and add
support to the latter for the former's syntax. This includes the Sass-specific
`rgba($color, $alpha)` syntax, which will now work with `rgb()` and `hsl()` as
well.

Sass will continue generating colors with alpha channels as `rgba()` calls, for
backwards-compatibility with older browsers that don't yet support Color Level
4.

## Definitions

### Special Number String

A *special number string* is an unquoted string that CSS will recognize as a
function that may return a number. For the purposes of Sass, this is any
unquoted string that begins with `calc(` or `var(`. This matching is
case-insensitive.

> Sass functions that shadow CSS functions must work with any invocation that
> CSS allows, which includes allowing special number strings anywhere a number
> would be allowed.
>
> This is intended to match the existing behavior for determining special number
> strings for `rgb()`, `hsl()`, `rgba()`, and `hsla()`.

### Special Variable String

A *special variable string* is [special number string][] that begins with
`var(`. This matching is case-insensitive.

[special number string]: #special-number-string

> Unlike other special number strings, variables can expand into multiple
> arguments to a single function.
>
> This is intended to match the existing behavior for determining special
> variable strings for `rgb()`, `hsl()`, `rgba()`, and `hsla()`.

## Semantics

This proposal adds new overloads to the `rgb()`, `hsl()`, `rgba()`, and `hsla()`
functions:

* ```
  rgb($channels)
  ```
  
  * If `$channels` is a [special variable string][], return a plain CSS function
    string with the name `"rgb"` and the argument `$channels`.

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

* ```
  rgb($red, $green, $blue, $alpha)
  ```

  * Call `rgba()` with the same argument and return the result, except that if
    it would return a plain CSS function named `"rgb"` that function is named
    `"rgba"` instead.

* ```
  rgb($color, $alpha)
  ```

  * If either argument is a [special variable string][], return a plain CSS
    function string with the name `"rgb"` and the same arguments.

  * Call `rgba()` with the same arguments and return the result.

* ```
  hsl($channels)
  ```

  * If `$channels` is a [special variable string][], return a plain CSS function
    string with the name `"hsl"` and the argument `$channels`.

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

* ```
  hsl($hue, $saturation, $lightness, $alpha)
  ```

  * Call `hsla()` with the same arguments and return the result, except that if
    it would return a plain CSS function named `"hsla"` that function is named
    `"hsl"` instead.

* ```
  rgba($channels)
  ```

  * Call `rgb()` with the same argument and return the result, except that if
    it would return a plain CSS function named `"rgb"` that function is named
    `"rgba"` instead.

* ```
  rgba($red, $green, $blue)
  ```

  * Call `rgb()` with the same argument and return the result, except that if
    it would return a plain CSS function named `"rgb"` that function is named
    `"rgba"` instead.

* ```
  hsla($channels)
  ```

  * Call `hsl()` with the same arguments and return the result, except that if
    it would return a plain CSS function named `"hsl"` that function is named
    `"hsla"` instead.

* ```
  hsla($hue, $saturation, $lightness)
  ```

  * Call `hsl()` with the same arguments and return the result, except that if
    it would return a plain CSS function named `"hsl"` that function is named
    `"hsla"` instead.
