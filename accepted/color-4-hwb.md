# HWB Color Functions: Draft 1

*([Issue](https://github.com/sass/sass/issues/2834))*

This proposal adds a new `hwb()` color format to the `sass:color` module, along
with inspection and adjustment options for _whiteness_ and _blackness_.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
  * [Future Designs](#future-designs)
* [Procedures](#procedures)
  * [Scaling a Number](#scaling-a-number)
* [Functions](#functions)
  * [`hwb()`](#hwb)
  * [`whiteness()`](#whiteness)
  * [`blackness()`](#blackness)
  * [`adjust()`](#adjust)
  * [`change()`](#change)
  * [`scale()`](#scale)

## Background

> This section is non-normative.

The [CSS Color Module level 4][color-4] provides several new CSS formats for
describing color, but `hwb()` stands out as part of the same `sRGB` color
system that Sass already uses internally.

[color-4]: https://www.w3.org/TR/css-color-4/

## Summary

> This section is non-normative.

This proposal defines a Sassified version of the `hwb()` color function
added to [CSS Color Level 4][color-4] – along with relevant inspection and
adjustment options. This function will only be available inside the `sass:color`
module to avoid conflicts with the CSS syntax, and will be converted to more
common color-name, hex, or `rgba()` syntax for output -- following the same
logic as our current color functions.

- New `color.hwb()` function describes colors in the sRGB colorspace using
  `$hue` (defined identically to the `hsl()` "hue" value), along with
  `$whiteness`, `$blackness`, and optional `$alpha` transparency.
- New `color.whiteness()` and `color.blackness()` functions return the respective
  values of `w` or `b` for a given color.
- Existing `color.adjust()`, `color.scale()`, and `color.change()` functions will
  accept additional `$whiteness` and `$blackness` parameters before the final
  `$alpha` parameter.

### Design Decisions

Both `rgb/a()` and `hsl/a()` are available in the global namespace because both
of these formats are part of a stable CSS spec, and we want to make any standard
CSS representation of a color parse as a Sass color. However, although `hwb()`
is defined in Color Level 4, it's not yet implemented by any browser. Sass
policy is to avoid supporting any new CSS syntax until it's shipped in a real
browser, so `hwb()` **will not** be available in the global namespace initially.
Instead, it will appear in the `sass:color` namespace which is guaranteed to be
forwards-compatible with future CSS changes.

Because the `color.hwb()` function isn't currently intended to directly
implement CSS's native `hwb()` function, it **will not** accept *special number
string* or *special variable string* values that can only be resolved in CSS.
However, for consistency with Sass's `rgb()` and `hsl()` functions it will
support both space-delimited and comma-delimited arguments.

### Future Designs

It's likely that as CSS Color Level 4 matures, `hwb()` will be stabilized and
supported in browsers in one form or another. At this point, Sass will likely
add support for a global `hwb()` function that's compatible with its CSS usage,
including supporting special number and variable strings. The details of this
are left to a future proposal.

## Procedures

### Scaling a Number

This algorithm takes a number `number`, a value `factor`, and a number `max`.
It's written "scale `<number>` by `<factor>` with a `max` of `<max>`". It
returns a number with a value between 0 and `max` and the same units as
`number`.

> Note: in practice, this is only ever called with `number <= max`.

* If `factor` isn't a number with unit `%` between `-100%` and `100%`
  (inclusive), throw an error.

* If `factor > 0%`, return `number + (max - number) * factor / 100%`.

* Otherwise, return `number + number * factor / 100%`.

## Functions

All new functions are part of the `sass:color` built-in module.

### `hwb()`

* ```
  hwb($hue, $whiteness, $blackness, $alpha: 1)
  ```

  * If any of `$hue`, `$whiteness`, `$blackness`, or `$alpha` aren't numbers,
    throw an error.

  * If `$hue` has any units other than `deg`, throw an error.

  * If either of `$whiteness` or `$blackness` don't have unit `%` or aren't
    between `0%` and `100%` (inclusive), throw an error.

  * Let `hue` be `$hue` without units.

  * Let `whiteness` be `$whiteness / 100%`.

  * Let `blackness` be `$blackness / 100%`.

  * If `whiteness + blackness > 1`:

    * Set `whiteness` to `whiteness / (whiteness + blackness)`.

    * Set `blackness` to `blackness / (whiteness + blackness)`.

  * Let `red`, `green`, and `blue` be the result of converting `hue`,
    `whiteness`, and `blackness` [to RGB][].

  * Set `red`, `green`, and `blue` to their existing values multiplied by 255
    and rounded to the nearest integers.

  * Let `alpha` be the result of [percent-converting][] `$alpha` with a `max` of 1.

  * Return a color with the given `red`, `green`, `blue`, and `alpha` channels.

  [percent-converting]: ../spec/built-in-modules/color.md#percent-converting-a-number
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

* Return a number with unit `%` between `0%` and `100%` (inclusive) such that:

  * `hwb(hue($color), whiteness($color), blackness($color))` returns a color with
    the same red, green, and blue channels as `$color`.

  * `whiteness($color) + blackness($color) <= 100%`.

  > The specific number returned here is left purposefully open-ended to allow
  > implementations to pursue different strategies for representing color
  > values. For example, one implementation may eagerly convert all colors to
  > RGB channels and convert back when `whiteness()` or `blackness()` is called,
  > where another may keep around their original HWB values and return those
  > as-is.

### `blackness()`

```
blackness($color)
```

* If `$color` is not a color, throw an error.

* Return a number with unit `%` between `0%` and `100%` (inclusive) such that:

  * `hwb(hue($color), whiteness($color), blackness($color))` returns a color with
    the same red, green, and blue channels as `$color`.

  * `whiteness($color) + blackness($color) <= 100%`.

  > The specific number returned here is left purposefully open-ended to allow
  > implementations to pursue different strategies for representing color
  > values. For example, one implementation may eagerly convert all colors to
  > RGB channels and convert back when `whiteness()` or `blackness()` is called,
  > where another may keep around their original HWB values and return those
  > as-is.

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

This function's new definition is as follows:

* If `$color` isn't a color, throw an error.

* Let `alpha` be `$color`'s alpha channel.

* If `$alpha` isn't null:

  * If `$alpha` isn't a number between -1 and 1 (inclusive), throw an error.

  * Set `alpha` to `alpha + $alpha` clamped between 0 and 1.

* If `$hue` isn't a number or null, throw an error.

* If any of `$red`, `$green`, or `$blue` aren't null:

  * If any of `$hue`, `$saturation`, `$lightness`, `$whiteness`, or `$blackness`
    aren't null, throw an error.

  * If any of `$red`, `$green`, or `$blue` aren't either null or numbers between
    -255 and 255 (inclusive), throw an error.

  * Let `red`, `green`, and `blue` be `$color`'s red, green, and blue channels.

  * If `$red` isn't null, set `red` to `red + $red` clamped between 0 and 255.

  * If `$green` isn't null, set `green` to `green + $green` clamped between 0 and 255.

  * If `$blue` isn't null, set `blue` to `blue + $blue` clamped between 0 and 255.

  * Return a color with `red`, `green`, `blue`, and `alpha` as the red, green,
    blue, and alpha channels, respectively.

* Otherwise, if either `$saturation` or `$lightness` aren't null:

  * If either `$whiteness` or `$blackness` aren't null, throw an error.

  * If either `$saturation` or `$lightness` aren't either null or numbers
    between -100 and 100 (inclusive), throw an error.

  * Let `hue`, `saturation`, and `lightness` be the result of calling
    `hue($color)`, `saturation($color)`, and `lightness($color)` respectively.

  * If `$hue` isn't null, set `hue` to `hue + $hue`.

  * If `$saturation` isn't null, set `saturation` to `saturation + $saturation`
    clamped between 0 and 100.

  * If `$lightness` isn't null, set `lightness` to `lightness + $lightness`
    clamped between 0 and 100.

  * Return the result of calling [`hsl()`][] with `hue`, `saturation`,
    `lightness`, and `alpha`.

* Otherwise, if either `$hue`, `$whiteness`, or `$blackness` aren't null:

  * If either `$whiteness` or `$blackness` aren't either null or numbers with
    unit `%` between `-100%` and `100%` (inclusive), throw an error.

  * Let `hue`, `whiteness`, and `blackness` be the result of calling
    `hue($color)`, `whiteness($color)`, and `blackness($color)` respectively.

  * If `$hue` isn't null, set `hue` to `hue + $hue`.

  * If `$whiteness` isn't null, set `whiteness` to `whiteness + $whiteness`
    clamped between `0%` and `100%`.

  * If `$blackness` isn't null, set `blackness` to `blackness + $blackness`
    clamped between `0%` and `100%`.

  * Return the result of calling `hwb()` with `hue`, `whiteness`, `blackness`,
    and `alpha`.

* Otherwise, return a color with the same red, green, and blue channels as
  `$color` and `alpha` as its alpha channel.

[`hsl()`]: ../spec/functions.md#hsl-and-hsla

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

This function's new definition is as follows:

* If `$color` isn't a color, throw an error.

* If `$alpha` isn't either null or a number between 0 and 1 (inclusive), throw
  an error.

* Let `alpha` be `$color`'s alpha channel if `$alpha` is null or `$alpha`
  without units otherwise.

* If `$hue` isn't a number or null, throw an error.

* If any of `$red`, `$green`, or `$blue` aren't null:

  * If any of `$hue`, `$saturation`, `$lightness`, `$whiteness`, or `$blackness`
    aren't null, throw an error.

  * If any of `$red`, `$green`, or `$blue` aren't either null or numbers
    between 0 and 255 (inclusive), throw an error.

  * Let `red` be `$color`'s red channel if `$red` is null or `$red` without
    units otherwise.

  * Let `green` be `$color`'s green channel if `$green` is null or `$green`
    without units otherwise.

  * Let `blue` be `$color`'s blue channel if `$blue` is null or `$blue` without
    units otherwise.

  * Return a color with `red`, `green`, `blue`, and `alpha` as the red, green,
    blue, and alpha channels, respectively.

* Otherwise, if either `$saturation` or `$lightness` aren't null:

  * If either `$whiteness` or `$blackness` aren't null, throw an error.

  * If either `$saturation` or `$lightness` aren't either null or numbers
    between 0 and 100 (inclusive), throw an error.

  * Let `hue` be the result of calling `hue($color)` if `$hue` is null, or
    `$hue` otherwise.

  * Let `saturation` be the result of calling `saturation($color)` if
    `$saturation` is null, or `$saturation` otherwise.

  * Let `lightness` be the result of calling `lightness($color)` if
    `$lightness` is null, or `$lightness` otherwise.

  * Return the result of calling [`hsl()`][] with `hue`, `saturation`,
    `lightness`, and `alpha`.

* Otherwise, if either `$hue`, `$whiteness`, or `$blackness` aren't null:

  * If either `$whiteness` or `$blackness` aren't either null or numbers with
    unit `%` between `0%` and `100%` (inclusive), throw an error.

  * Let `hue` be the result of calling `hue($color)` if `$hue` is null, or
    `$hue` otherwise.

  * Let `whiteness` be the result of calling `whiteness($color)` if `$whiteness`
    is null, or `$whiteness` otherwise.

  * Let `blackness` be the result of calling `blackness($color)` if `$blackness`
    is null, or `$blackness` otherwise.

  * Return the result of calling `hwb()` with `hue`, `whiteness`, `blackness`,
    and `alpha`.

* Otherwise, return a color with the same red, green, and blue channels as
  `$color` and `alpha` as its alpha channel.

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

This function's new definition is as follows:

* If `$color` isn't a color, throw an error.

* Let `alpha` be `$color`'s alpha channel.

* If `$alpha` isn't null, set `alpha` to the result of [scaling][] `alpha` by
  `$alpha` with `max` 1.

  [scaling]: #scaling-a-number

* If any of `$red`, `$green`, or `$blue` aren't null:

  * If any of `$saturation`, `$lightness`, `$whiteness`, or `$blackness` aren't
    null, throw an error.

  * Let `red`, `green`, and `blue` be `$color`'s red, green, and blue channels.

  * If `$red` isn't null, set `red` to the result of [scaling][] `red` by `$red`
    with `max` 255.

  * If `$green` isn't null, set `green` to the result of [scaling][] `green` by
    `$green` with `max` 255.

  * If `$blue` isn't null, set `blue` to the result of [scaling][] `blue` by `$blue`
    with `max` 255.

  * Return a color with `red`, `green`, `blue`, and `alpha` as the red, green,
    blue, and alpha channels, respectively.

* Otherwise, if either `$saturation` or `$lightness` aren't null:

  * If either `$whiteness` or `$blackness` aren't null, throw an error.

  * Let `hue`, `saturation`, and `lightness` be the result of calling
    `hue($color)`, `saturation($color)`, and `lightness($color)` respectively.

  * If `$saturation` isn't null, set `saturation` to the result of [scaling][]
    `saturation` by `$saturation` with `max` `100%`.

  * If `$lightness` isn't null, set `lightness` to the result of [scaling][]
    `lightness` by `$lightness` with `max` `100%`.

  * Return the result of calling [`hsl()`][] with `hue`, `saturation`,
    `lightness`, and `alpha`.

* Otherwise, if either `$hue`, `$whiteness`, or `$blackness` aren't null:

  * Let `hue`, `whiteness`, and `blackness` be the result of calling
    `hue($color)`, `whiteness($color)`, and `blackness($color)` respectively.

  * If `$whiteness` isn't null, set `whiteness` to the result of [scaling][]
    `whiteness` by `$whiteness` with `max` `100%`.

  * If `$blackness` isn't null, set `blackness` to the result of [scaling][]
    `blackness` by `$blackness` with `max` `100%`.

  * Return the result of calling `hwb()` with `hue`, `whiteness`, `blackness`,
    and `alpha`.

* Otherwise, return a color with the same red, green, and blue channels as
  `$color` and `alpha` as its alpha channel.
