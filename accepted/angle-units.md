# Angle Units: Draft 1

*([Issue](https://github.com/sass/sass/issues/2904))*

This proposal adds support for units other than `deg` to HSL and HWB functions.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Scope of Phase 2](#scope-of-phase-2)
    * [Global Saturation and Lightness Functions](#global-saturation-and-lightness-functions)
* [Functions](#functions)
  * [`hsl()` and `hsla()`](#hsl-and-hsla)
  * [`color.hwb()`](#colorhwb)
  * [`adjust-hue()`](#adjust-hue)
  * [`color.adjust()`](#coloradjust)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)
  * [Phase 3](#phase-3)

## Background

> This section is non-normative.

CSS Values and Units 3 defines [a number of different units] that can be used to
represent angles, including the hue angle that's passed to the [`hsl()`][css
hsl] and [`hwb()`] functions. However, Sass has historically ignored
units for arguments passed to `hsl()` and related functions, choosing instead to
always interpret the hue as degrees. For example, `hsl(1rad 50% 50%)` is
incorrectly interpreted as `hsl(1deg 50% 50%) = #bf4240` rather than
`hsl(57.3deg 50% 50%) = #bfba40`.

[a number of different units]: https://www.w3.org/TR/css-values-3/#angles
[css hsl]: https://www.w3.org/TR/css-color-4/#the-hsl-notation
[`hwb()`]: https://www.w3.org/TR/css-color-4/#the-hwb-notation

Relatedly, `hsl()` and related functions don't enforce that the saturation and
lightness values are percentages. This is a less pressing issue, because it
doesn't mean that Sass is misinterpreting *valid* CSS, but it does mean that
invalid CSS like `hsl(0 50 50)` or even `hsl(0 50px 50px)` is being incorrectly
accepted.

## Summary

> This section is non-normative.

This proposal makes `hsl()`, `hsla()`, `hwb()`, `adjust-hue()`,
`color.adjust()`, and `color.change()` convert all hue angles into degrees and
reject all non-angle non-empty units for hue and all non-`%` units for
saturation and lightness. Because these are breaking changes, they're split
across three distinct phases:

1. Initially, passing any non-`deg` non-empty units to hue or non-`%` units to
   saturation or lightness will produce a deprecation warning. Passing unitless
   numbers for hue will still be allowed because the CSS spec allows it, but not
   for saturation or lightness.

2. After at least three months (per the [Dart Sass compatibility policy]), all
   functions will start converting angle units to degrees. Because this brings
   Sass into compatibility with the CSS spec, Dart Sass will make it outside of
   a major version change despite it being a breaking change. All other
   deprecations will remain in place without breaking changes.

3. As part of the release of the next major version of Dart Sass, these
   functions will start throwing errors for unknown units rather than
   interpreting them as `deg` or `%`. Passing unitless numbers for hue will
   still be supported.

[Dart Sass compatibility policy]: https://github.com/sass/dart-sass#compatibility-policy

### Design Decisions

#### Scope of Phase 2

It would be possible to further minimize the scope of the breaking change in
phase 2 by *only* changing how `hsl()` and `hsla()` interpret hue arguments with
other angle arguments, and leaving `adjust-hue()`, `color.adjust()`, and
`color.change()` alone. However, allowing the same hue argument to be accepted
in multiple functions but interpreted different ways is likely to cause
substantial confusion. In addition, it's unlikely that users are specifically
passing `deg`, `rad`, or `turn` units today and relying on their broken
behavior.

In other words, the cost of not slightly expanding this breaking change is
likely to be high, and the benefit in terms of causing friction for existing
users is likely to be very low.

#### Global Saturation and Lightness Functions

Since we're requiring `%` for saturation and lightness in most functions, it
would make some sense to add the same requirement to the `saturate()`,
`desaturate()`, `lighten()`, and `darken()` functions as well. This proposal
chooses instead to leave them as-is because they're intended to be removed in
the next breaking release anyway, and until that release saturation and
lightness will only have deprecation warnings. This would put them in a
situation where they would *only ever* emit deprecation warnings without ever
actually rejecting other units, which is unlikely to be worth the effort.

## Functions

> Note that although the behavior of the `color.change()` function is changing,
> no explicit changes are needed because per the spec it passes its `$hue`,
> `$saturation`, and `$lightness` parameters directly to `hsl()`.

### `hsl()` and `hsla()`

In the four-argument overload of the global [`hsl()`] function, replace

[`hsl()`]: ../spec/functions.md#hsl-and-hsla

* Let `hue` be `($hue % 360) / 60` without units.

* Let `saturation` and `lightness` be the result of clamping `$saturation` and
  `$lightness`, respectively, between 0 and 100 and dividing by 100.

with

* Let `hue` be the result of [converting] `$hue` to `deg` allowing unitless.

* Set `hue` to `(hue % 360deg) / 60deg`.

* If `$saturation` and `$lightness` don't have unit `%`, throw an error.

* Let `saturation` and `lightness` be the result of clamping `$saturation` and
  `$lightness`, respectively, between `0%` and `100%` and dividing by `100%`.

[converting]: ../spec/types/number.md#converting-a-number-to-a-unit

Because `hsla()` is identical to `hsl()`, it's updated identically.

### `color.hwb()`

In the four-argument overload of [`color.hwb()`], replace

[`color.hwb()`]: ../spec/built-in-modules/color.md#hwb

* If `$hue` has any units other than `deg`, throw an error.

* If either of `$whiteness` or `$blackness` don't have unit `%` or aren't
  between `0%` and `100%` (inclusive), throw an error.

* Let `hue` be `($hue % 360) / 60` without units.

with

* Let `hue` be the result of [converting] `$hue` to `deg` allowing unitless.

* Set `hue` to `(hue % 360deg) / 60deg`.

* If either of `$whiteness` or `$blackness` don't have unit `%` or aren't
  between `0%` and `100%` (inclusive), throw an error.

### `adjust-hue()`

The global `adjust-hue()` function will now behave as follows:

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
  
[`color.saturation($color)`]: ../spec/built-in-modules/color.md#saturation
[`color.lightness($color)`]: ../spec/built-in-modules/color.md#lightness

### `color.adjust()`

In the definition of [`color.adjust()`], after

[`color.adjust()`]: ../spec/built-in-modules/color.md#adjust

* If `$hue` isn't a number or null, throw an error.

add

* If `$hue` is a number and it has units that aren't [compatible] with `deg`,
  throw an error.

  > Unitless numbers are allowed.

[compatible]: ../spec/types/number.md#compatible-units

> The existing definition of `color.adjust()` includes the line "set `hue` to
> `hue + $hue`" which *should* throw an error if `$hue` has units that aren't
> compatible with `deg` and otherwise should convert `$hue` to `deg`. However,
> no implementation currently follows that behavior, so this spec change
> effectively serves to make the already-specified behavior more explicit.

In addition, replace

* If either `$saturation` or `$lightness` aren't either null or numbers
  between -100 and 100 (inclusive), throw an error.

* Let `hue`, `saturation`, and `lightness` be the result of calling
  `hue($color)`, `saturation($color)`, and `lightness($color)` respectively.

* If `$hue` isn't null, set `hue` to `hue + $hue`.

* If `$saturation` isn't null, set `saturation` to `saturation + $saturation`
  clamped between 0 and 100.

* If `$lightness` isn't null, set `lightness` to `lightness + $lightness`
  clamped between 0 and 100.

with

* If either `$saturation` or `$lightness` aren't either null or numbers **with
  unit `%` between -100% and 100%** (inclusive), throw an error.

* Let `hue`, `saturation`, and `lightness` be the result of calling
  `hue($color)`, `saturation($color)`, and `lightness($color)` respectively.

* If `$hue` isn't null, set `hue` to `hue + $hue`.

* If `$saturation` isn't null, set `saturation` to `saturation + $saturation`
  clamped between **0% and 100%**.

* If `$lightness` isn't null, set `lightness` to `lightness + $lightness`
  clamped between **0% and 100%**.

## Deprecation Process

The deprecation process will be divided into three phases:

### Phase 1

> This phase adds no breaking changes. Its purpose is to notify users of the
> upcoming changes to behavior and give them a chance to move towards passing
> future-proof units.

Phase 1 implements none of the function changes described [above](#functions).
Instead, if the `$hue` parameter to [`hsl()`], [`hsla()`], `adjust-hue()`,
[`color.adjust()`], or [`color.change()`] is passed a number with a unit other
than `deg`, emit a deprecation warning. In addition, if either the `$saturation`
or `$lightness` parameters to `hsl()`, `hsla()`, `color.adjust()`, or
`color.change()` are passed a number without the unit `%`, emit a deprecation
warning.

[`hsla()`]: ../spec/functions.md#hsl-and-hsla
[`color.change()`]: ../spec/built-in-modules/color.md#change

> Unitless hues should not cause deprecation warnings, but unitless saturations
> and lightnesses should.

### Phase 2

> This phase only breaks the behavior of passing `deg`-compatible units as hues,
> and otherwise leaves existing behavior intact.

Phase 2 implements a subset of the function changes described
[above](#functions). In particular:

* The `color.hwb()` function is updated [as described above](#colorhwb).

* If the `$hue` parameter to [`hsl()`], [`hsla()`], `adjust-hue()`,
  [`color.adjust()`], or [`color.change()`] is passed a number with unit `rad`,
  `grad`, or `turn`, [convert] it to `deg` before running the original function.

* As in phase 1, if the `$hue` parameter to [`hsl()`], [`hsla()`],
  `adjust-hue()`, [`color.adjust()`], or [`color.change()`] is passed a number
  with a unit other than `deg`, `rad`, `grad`, or `turn`, emit a deprecation
  warning.

* As in phase 1, if either the `$saturation` or `$lightness` parameters to
  `hsl()`, `hsla()`, `color.adjust()`, or `color.change()` are passed a number
  without the unit `%`, emit a deprecation warning.

[convert]: ../spec/types/number.md#converting-a-number-to-a-unit

### Phase 3

Phase 3 implements the full function changes described [above](#functions).

> It's recommended that implementations increment their major version numbers
> with the release of phase 3.
