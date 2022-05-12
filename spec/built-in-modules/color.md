# Color Module

This built-in module is available from the URL `sass:color`.

## Table of Contents

* [Procedures](#procedures)
  * [Percent-Converting a Number](#percent-converting-a-number)
  * [Scaling a Number](#scaling-a-number)
* [Functions](#functions)
  * [`adjust()`](#adjust)
  * [`adjust-hue()`](#adjust-hue)
  * [`alpha()`](#alpha)
  * [`blackness()`](#blackness)
  * [`blue()`](#blue)
  * [`change()`](#change)
  * [`complement()`](#complement)
  * [`darken()`](#darken)
  * [`desaturate()`](#desaturate)
  * [`fade-in()`](#fade-in)
  * [`fade-out()`](#fade-out)
  * [`grayscale()`](#grayscale)
  * [`green()`](#green)
  * [`hue()`](#hue)
  * [`hwb()`](#hwb)
  * [`ie-hex-str()`](#ie-hex-str)
  * [`invert()`](#invert)
  * [`lighten()`](#lighten)
  * [`lightness()`](#lightness)
  * [`mix()`](#mix)
  * [`opacify()`](#opacify)
  * [`red()`](#red)
  * [`saturate()`](#saturate)
  * [`saturation()`](#saturation)
  * [`scale()`](#scale)
  * [`transparentize()`](#transparentize)
  * [`whiteness()`](#whiteness)

## Procedures

### Percent-Converting a Number

This algorithm takes a SassScript number `number` and a number `max`. It returns
a number between 0 and `max`.

* If `number` has units other than `%`, throw an error.

* If `number` has the unit `%`, set `number` to `number * max / 100`, without
  units.

* Return `number`, clamped between 0 and `max`.

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

### `adjust()`

```
adjust($color,
  $red: null, $green: null, $blue: null,
  $hue: null, $saturation: null, $lightness: null,
  $whiteness: null, $blackness: null,
  $alpha: null)
```

This function is also available as a global function named `adjust-color()`.

* If `$color` isn't a color, throw an error.

* Let `alpha` be `$color`'s alpha channel.

* If `$alpha` isn't null:

  * If `$alpha` isn't a number between -1 and 1 (inclusive), throw an error.

  * Set `alpha` to `alpha + $alpha` clamped between 0 and 1.

* If `$hue` isn't a number or null, throw an error.

* If `$hue` is a number and it has units that aren't [compatible] with `deg`,
  throw an error.

  > Unitless numbers are allowed.

  [compatible]: ../types/number.md#compatible-units

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

  * If either `$saturation` or `$lightness` aren't either null or numbers with
    unit `%` between -100% and 100% (inclusive), throw an error.

  * Let `hue`, `saturation`, and `lightness` be the result of calling
    `hue($color)`, `saturation($color)`, and `lightness($color)` respectively.

  * If `$hue` isn't null, set `hue` to `hue + $hue`.

  * If `$saturation` isn't null, set `saturation` to `saturation + $saturation`
    clamped between 0% and 100%.

  * If `$lightness` isn't null, set `lightness` to `lightness + $lightness`
    clamped between 0% and 100%.

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

[`hsl()`]: ../functions.md#hsl-and-hsla

### `adjust-hue()`

```
adjust-hue($color, $degrees)
```

* Throw an error.

  > This error should indicate that the user should use the [`adjust()`
  > function](#adjust) instead.

### `alpha()`

```
alpha($color)
```

This function is also available as a global function named `opacity()`.

> There is also a global function named `alpha()`, but it supports an additional
> overload with different behavior that's defined with the [global functions][].
>
> [global functions]: ../functions.md#alpha

* If `$color` is a number and this function is called as the global `opacity()`
  function, return a plain CSS function string with the name `"opacity"` and the
  argument `$color`.

* Otherwise, if `$color` is not a color, throw an error.

* Return the alpha channel of `$color` as a unitless number.

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

### `blue()`

```
blue($color)
```

This function is also available as a global function named `blue()`.

### `change()`

```
change($color,
  $red: null, $green: null, $blue: null,
  $hue: null, $saturation: null, $lightness: null,
  $whiteness: null, $blackness: null,
  $alpha: null)
```

This function is also available as a global function named `change-color()`.

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

### `complement()`

```
complement($color)
```

This function is also available as a global function named `complement()`.

### `darken()`

```
darken($color, $amount)
```

* Throw an error.

  > This error should indicate that the user should use the [`adjust()`
  > function](#adjust) instead.

### `desaturate()`

```
desaturate($color, $amount)
```

* Throw an error.

  > This error should indicate that the user should use the [`adjust()`
  > function](#adjust) instead.

### `fade-in()`

```
fade-in($color, $amount)
```

* Throw an error.

  > This error should indicate that the user should use the [`adjust()`
  > function](#adjust) instead.

### `fade-out()`

```
fade-out($color, $amount)
```

* Throw an error.

  > This error should indicate that the user should use the [`adjust()`
  > function](#adjust) instead.

### `grayscale()`

```
grayscale($color)
```

This function is also available as a global function named `grayscale()`.

* If `$color` is a number and this function is called as a global function,
  return a plain CSS function string with the name `"grayscale"` and the
  argument `$color`.

* Otherwise, if `$color` is not a color, throw an error.

* Return a color with the same hue and lightness as `$color` but with saturation
  0.

### `green()`

```
green($color)
```

This function is also available as a global function named `green()`.

### `hue()`

```
hue($color)
```

This function is also available as a global function named `hue()`.

### `hwb()`

* ```
  hwb($hue, $whiteness, $blackness, $alpha: 1)
  ```

  * If any of `$hue`, `$whiteness`, `$blackness`, or `$alpha` aren't numbers,
    throw an error.

  * Let `hue` be the result of [converting] `$hue` to `deg` allowing unitless.

  * If either of `$whiteness` or `$blackness` don't have unit `%` or aren't
    between `0%` and `100%` (inclusive), throw an error.

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

  [converting]: ../types/number.md#converting-a-number-to-a-unit
  [percent-converting]: #percent-converting-a-number
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

### `ie-hex-str()`

```
ie-hex-str($color)
```

This function is also available as a global function named `ie-hex-str()`.

### `invert()`

```
invert($color, $weight: 100%)
```

This function is also available as a global function named `invert()`.

* If `$color` is a number and this function is called as a global function:

  * If `$weight` is not `100%`, throw an error.

  * Return a plain CSS function string with the name `"invert"` and the argument
    `$color`.

* If `$color` is not a color, throw an error.

* Let `inverse` be a color with each RGB channel equal to 255 minus `$color`'s
  corresponding channel.

* Call [`mix()`](#mix) with `$color`, `inverse`, and `$weight` and return the
  result.

### `lighten()`

```
lighten($color, $amount)
```

* Throw an error.

  > This error should indicate that the user should use the [`adjust()`
  > function](#adjust) instead.

### `lightness()`

```
lightness($color)
```

This function is also available as a global function named `lightness()`.

### `mix()`

```
mix($color1, $color2, $weight: 50%)
```

### `opacify()`

```
opacify($color, $amount)
```

* Throw an error.

  > This error should indicate that the user should use the [`adjust()`
  > function](#adjust) instead.

### `red()`

This function is also available as a global function named `red()`.

```
red($color)
```

This function is also available as a global function named `mix()`.

### `saturate()`

```
saturate($color, $amount)
```

* Throw an error.

  > This error should indicate that the user should use the [`adjust()`
  > function](#adjust) instead.

### `saturation()`

```
saturation($color)
```

This function is also available as a global function named `saturation()`.

### `scale()`

```
scale($color,
  $red: null, $green: null, $blue: null,
  $saturation: null, $lightness: null,
  $whiteness: null, $blackness: null,
  $alpha: null)
```

This function is also available as a global function named `scale-color()`.

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

### `transparentize()`

```
transparentize($color, $amount)
```

* Throw an error.

  > This error should indicate that the user should use the [`adjust()`
  > function](#adjust) instead.

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
