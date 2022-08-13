# Function Units: Draft 1

*([Issue](https://github.com/sass/sass/issues/3374))*

This proposal restricts the use of invalid units in built-in Sass functions.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Decision Decisions](#decision-decisions)
    * [Alpha Units](#alpha-units)
* [Functions](#functions)
  * [`color.adjust()`](#coloradjust)
  * [`color.scale()`](#colorscale)
  * [`color.mix()`](#colormix)
  * [`list.nth()` and `list.set-nth()`](#listnth-and-listset-nth)
* [Deprecation Process](#deprecation-process)
  * [`color.adjust()`](#coloradjust-1)
  * [`color.scale()`](#colorscale-1)
  * [`color.mix()`](#colormix-1)
  * [`list.nth()` and `list.set-nth()`](#listnth-and-listset-nth-1)

## Background

> This section is non-normative.

Sass functions added early during Sass's lifetime were generally quite
permissive, allowing numbers with units to be passed even if the units were
ignored. However, this is often quite confusing in practice; for example, a user
might expect that `color.adjust($color, $alpha: -1%)` would return a color with
`0.01` less alpha. In fact it returns a transparent color, because the unit is
ignored entirely.

In some cases, CSS has even added support for units to functions after the
fact—including support for [percentages in alpha values]. In that case, Sass has
been forced to deprecate passing incorrect units to those functions before
adding support for new units.

[percentages in alpha values]: https://www.w3.org/TR/css-color-4/#typedef-alpha-value

## Summary

> This section is non-normative.

This proposal adds the following restrictions:

* The `$alpha` parameter to `color.adjust()`, `color.change()`, `opacify()`,
  `fade-in()`, `transparentize()`, and `fade-out()` may be either unitless or
  use the `%` unit. If it uses `%`, it's divided by `100%` before using it.

* The `$weight` parameter to `color.mix()` must have unit `%`.

* The `$n` parameter to `list.nth()` and `list.set-nth()` may not have units.

### Decision Decisions

#### Alpha Units

One alternative would be to forbid units in `$alpha` parameters entirely, as
we're doing for `$n` parameters. However, since [Colors Level 4] supports
percentage-style alphas (as do Sass's `hsl()`, `rgb()`, and `hwb()` functions),
it's much more friendly and consistent to allow them.

[Colors Level 4]: https://www.w3.org/TR/css-color-4/#typedef-alpha-value

There is some risk that users are already passing `%` units to alpha values and
will have their colors change unexpectedly. However, deprecation warnings should
alert these users in time to change their code, and even if they miss the
warnings it's likely they expected the new behavior in the first place so in a
sense the change will be a bug fix for them.

## Functions

> `opacify()`, `fade-in()`, `transparentize()`, and `fade-out()` don't need to
> be modified explicitly because they're defined as calling `color.adjust()`
> internally.

### `color.adjust()`

Replace the "If `$alpha` isn't null" block with the following:

* If `$alpha` isn't null:

  * If `$alpha` isn't a number, throw an error.

  * If `$alpha` has units other than `%`, throw an error.

  * If `$alpha` has unit `%`, set it to `math.div($alpha, 100%)`.

  * If `$alpha` isn't a number between -1 and 1 (inclusive), throw an error.

  * Set `alpha` to `alpha + $alpha` clamped between 0 and 1.

### `color.scale()`

Replace

* If `$alpha` isn't either null or a number between 0 and 1 (inclusive), throw
  an error.

* Let `alpha` be `$color`'s alpha channel if `$alpha` is null or `$alpha`
  without units otherwise.

with

* If `$alpha` is null, let `alpha` be `$color`'s alpha channel. Otherwise:

  * If `$alpha` isn't a number, throw an error.

  * If `$alpha` has units other than `%`, throw an error.

  * If `$alpha` has unit `%`, set it to `math.div($alpha, 100%)`.

  * If `$alpha` isn't a number between 0 and 1 (inclusive), throw an error.

  * Let `alpha` be `$alpha`.

### `color.mix()`

Add the following to the beginning of the function's definition:

* If `$weight` isn't a number with unit `%`, throw an error.

### `list.nth()` and `list.set-nth()`

Add the following to the beginning of these functions' definitions:

* If `$n` isn't a unitless integer, throw an error.

## Deprecation Process

Before an implementation releases its next major version, it should make the
following changes instead of those listed above:

### `color.adjust()`

Add the following to the beginning of the "If `$alpha` isn't null" block:

* If `$alpha` has any units, emit a deprecation warning.

### `color.scale()`

Replace

* If `$alpha` isn't either null or a number between 0 and 1 (inclusive), throw
  an error.

* Let `alpha` be `$color`'s alpha channel if `$alpha` is null or `$alpha`
  without units otherwise.

with

* If `$alpha` is null, let `alpha` be `$color`'s alpha channel. Otherwise:

  * If `$alpha` isn't a number, throw an error.

  * If `$alpha` has any units, emit a deprecation warning.

  * If `$alpha` isn't a number between 0 and 1 (inclusive), throw an error.

  * Let `alpha` be `$alpha`.

### `color.mix()`

Add the following to the beginning of the function's definition:

* If `$weight` is a unitless number or a number with units other than `%`, emit
  a deprecation warning.

### `list.nth()` and `list.set-nth()`

Add the following to the beginning of these functions' definitions:

* If `$n` is a number with units, emit a deprecation warning.
