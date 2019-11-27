# Color Module

This built-in module is available from the URL `sass:color`.

## Table of Contents

* [Procedures](#procedures)
  * [Percent-Converting a Number](#percent-converting-a-number)
* [Functions](#functions)
  * [`adjust()`](#adjust)
  * [`adjust-hue()`](#adjust-hue)
  * [`alpha()`](#alpha)
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

## Procedures

### Percent-Converting a Number 

This algorithm takes a SassScript number `number` and a number `max`. It returns
a number between 0 and `max`.

* If `number` has units other than `%`, throw an error.

* If `number` has the unit `%`, set `number` to `number * max / 100`, without
  units.

* Return `number`, clamped between 0 and `max`.

## Functions

### `adjust()`

```
adjust($color,
  $red: null, $green: null, $blue: null,
  $hue: null, $saturation: null, $lightness: null,
  $alpha: null)
```

This function is also available as a global function named `adjust-color()`.

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
  $alpha: null)
```

This function is also available as a global function named `change-color()`.

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
  $alpha: null)
```

This function is also available as a global function named `scale-color()`.

### `transparentize()`

```
transparentize($color, $amount)
```

* Throw an error.

  > This error should indicate that the user should use the [`adjust()`
  > function](#adjust) instead.
