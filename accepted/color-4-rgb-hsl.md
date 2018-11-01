# Color Level 4 `rgb()` and `hsl()` Functions: Draft 1

*([Issue](https://github.com/sass/sass/issues/2564))*

This proposal expands Sass's built-in `rgb()`, `rgba()`, `hsl()`, and `hsla()`
functions to support the new terse syntax defined in [CSS Color Level 4][].

[CSS Color Level 4]: https://drafts.csswg.org/css-color/

## Table of Contents

* [Summary](#summary)
* [Semantics](#semantics)

## Summary

> This section is non-normative.

Sass's `rgb()` and `hsl()` functions will add support for the Color Level 4
syntax, so `rgb(179 115 153)`, `rgb(70% 45% 60%)`, and `hsl(324deg 29% 57%)`
will all return the same color. They'll also support the slash-separated alpha
syntax using [the normal rules][] for distinguishing between division and `/`,
so `rgb(179 115 153 / 50%)` will return a 50%-opacity color but
`rgb(179 115 $blue / 50%)` will not.

[the normal rules]: http://sass-lang.com/documentation/file.SASS_REFERENCE.html#division-and-slash

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

## Semantics

This proposal adds new overloads to the `rgb()`, `hsl()`, `rgba()`, and `hsla()`
functions:

* ```
  rgb($channels)
  ```

  * If `$channels` is not an unbracketed space-separated list, throw an error.

  * If `$channels` doesn't have exactly three elements, throw an error.

  * Let `red` and `green` be the first two elements of `$channels`.

  * If the third element of `$channels` has preserved its status as two
    slash-separated numbers:

    * Let `blue` be the number before the slash and `alpha` the number after the
      slash.

    * Call `rgba()` with `red`, `green`, `blue`, and `alpha` as arguments and
      return the result.

  * Otherwise:

    * Let `blue` be the third element of `$channels`.

    * Call `rgb()` with `red`, `green`, and `blue` as arguments and return the
      result.

* ```
  rgb($red, $green, $blue, $alpha)
  ```

  * Call `rgba()` with the same arguments and return the result.

* ```
  rgb($color, $alpha)
  ```

  * Call `rgba()` with the same arguments and return the result.

* ```
  hsl($channels)
  ```

  * If `$channels` is not an unbracketed space-separated list, throw an error.

  * If `$channels` doesn't have exactly three elements, throw an error.

  * Let `hue` and `saturation` be the first two elements of `$channels`.

  * If the third element of `$channels` has preserved its status as two
    slash-separated numbers:

    * Let `lightness` be the number before the slash and `alpha` the number
      after the slash.

    * Call `hsla()` with `hue`, `saturation`, `lightness`, and `alpha` as
      arguments and return the result.

  * Otherwise:

    * Let `lightness` be the third element of `$channels`.

    * Call `hsl()` with `hue`, `saturation`, and `lightness` as arguments and
      return the result.

* ```
  hsl($hue, $saturation, $lightness, $alpha)
  ```

  * Call `hsla()` with the same arguments and return the result.

* ```
  hsl($color, $alpha)
  ```

  * Call `hsla()` with the same arguments and return the result.

* ```
  rgba($channels)
  ```

  * Call `rgb()` with the same argument and return the result.

* ```
  rgba($red, $green, $blue)
  ```

  * Call `rgb()` with the same arguments and return the result.

* ```
  hsla($channels)
  ```

  * Call `hsl()` with the same argument and return the result.

* ```
  hsla($hue, $saturation, $lightness)
  ```

  * Call `hsl()` with the same arguments and return the result.
