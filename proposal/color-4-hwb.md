# HWB Color Functions: Draft 1

*([Issue](https://github.com/sass/sass/issues/2831))*

This proposal adds a new `hwb()` color format to the `sass:color` module, along
with inspection and adjustment options for _whiteness_ and _blackness_.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Semantics](#semantics)
  * [`hwb()`](#hwb)
  * [`whiteness()`](#whiteness)
  * [`blackness()`](#blackness)
  * [`adjust()`](#adjust)
  * [`change()`](#change)
  * [`scale()`](#scale)

## Background

> This section is non-normative.

The [CSS Color Module level 4][color-4], provides several new CSS formats for
describing color, but `hwb()` stands out as part of the same `sRGB` color
system that Sass already uses internally.

[color-4]: https://www.w3.org/TR/css-color-4/

## Summary

> This section is non-normative.

This proposal defines  aSassified versions of the `hwb()` color function
added to [CSS Color Level 4][color-4] – along with relevant inspection and
adjustment options. This function will only be available inside the `sass:color`
module to avoid conflicts with the CSS syntax, and will be converted to more
common color-name, hex, or `rgba()` syntax for output -- following the same
logic as our current color functions.

- New `color.hwb()` describes colors in the sRGB colorspace using `$hue` (defined
  identically to the `hsl()` "hue" value), along with `$whiteness`, `$blackness`,
  and optional `$alpha` transparency.
- New `color.whiteness()` and `color.blackness()` functions return the respective
  values of `w` or `b` for a given color.
- Existing `color.adjust()`, `color.scale()`, and `color.change()` functions will
  accept additional `$whiteness` and `$blackness` parameters before the final
  `$alpha` parameter.

### Design Decisions

For backwards-compatibility reasons, both `rgb/a()` and `hsl/a()` are processed
as Sass function in the global namespace, and output is converted to the format
with best browser-support. The primary purpose of Sass color-function support
is our ability to provide those conversions, but the module syntax now allows
us to namespace new formats, so authors can opt-into the feature. With that
in mind:

- The new functions **will not** be available on the global namespace.
- The new functions **will not** accept *special number string* or
  *special variable string* values that can only be resolved in CSS.

Unknown CSS functions are already passed through implicitly. As browser support
grows for the native-CSS functions, authors will be able to migrate on their own
schedule – or use both Sass & CSS functions together to progressively enhance
their designs.

Since all `color.hwb()` output will be processed for rgb output, there is no
need to support shortcuts like `hwb($color, $alpha)`. However, it is reasonable
to support both the space-delimited CSS syntax, as well as the comma-delimited
Sass named-argument syntax.

## Semantics

### `hwb()`

* ```
  hwb($hue, $whiteness, $blackness, $alpha: 1)
  ```

  * If any of `$hue`, `$whiteness`, `$blackness`, or `$alpha` aren't numbers,
    throw an error.

  * If either of `$whiteness` or `$blackness` is not a percentage between 0% and
    100%, throw an error.

  * Let `hue` be `($hue % 360) / 6`.

  * Let `whiteness` and `$blackness` be the respective values of `$whiteness`
    and `$blackness`

  * If the sum of `whiteness` and `blackness` is greater than 100%, normalize
    both to maintain the same relative ratio, adding up to 100% combined.

  * Let `red`, `green`, and `blue` be the result of converting `hue`,
    `whiteness`, and `blackness` [to RGB][].

  [to RGB]: https://www.w3.org/TR/css-color-4/#hwb-to-rgb

* ```
  hwb($channels)
  ```

  * If `$channels` is not an unbracketed space-separated list, throw an error.

  * If `$channels` does not includes exactly three elements, throw an error.

  * Let `hue` and `whiteness` be the first two elements of `$channels`

  * If the third element of `$channels` has preserved its status as
    two slash-separated numbers:

    * Let `blackness` be the number before the slash and `alpha` the number
      after the slash.

  * Otherwise:

    * Let `blackness` be the third element of `$channels`.

  * Call `hwb()` with `hue`, `whiteness`, `blackness`, and `alpha` (if it's
    defined) as arguments and return the result.

### `whiteness()`

```
whiteness($color)
```

* If `$color` is not a color, throw an error.
* Return the `hwb()` whiteness value of `$color`.

### `blackness()`

```
blackness($color)
```

* If `$color` is not a color, throw an error.
* Return the `hwb()` blackness value of `$color`.

### `adjust()`

This proposal adds new `$whiteness` and `$blackness` parameters to the `adjust()`
function, and its global `adjust-color()` alias.

```
adjust($color,
  $red: null, $green: null, $blue: null,
  $hue: null, $saturation: null, $lightness: null,
  $whiteness: null, $blackness: null,
  $alpha: null)
```

### `change()`

This proposal adds new `$whiteness` and `$blackness` parameters to the `change()`
function, and its global `change-color()` alias.

```
change($color,
  $red: null, $green: null, $blue: null,
  $hue: null, $saturation: null, $lightness: null,
  $whiteness: null, $blackness: null,
  $alpha: null)
```

### `scale()`

This proposal adds new `$whiteness` and `$blackness` parameters to the `scale()`
function, and its global `scale-color()` alias.

```
scale($color,
  $red: null, $green: null, $blue: null,
  $saturation: null, $lightness: null,
  $whiteness: null, $blackness: null,
  $alpha: null)
```
