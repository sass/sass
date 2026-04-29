# Analogous Sets: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4217))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Which Conversions?](#which-conversions)
    * [No Deprecation](#no-deprecation)
* [Definitions](#definitions)
  * [Missing Components](#missing-components)
  * [Analogous Mappings](#analogous-mappings)
* [Procedures](#procedures)
  * [Converting a Color](#converting-a-color)

## Background

> This section is non-normative.

When Sass originally implemented support for the expanded color spaces
introduced by [CSS Color 4], it contained the concept of "[analogous
components]" across two color spaces whose missing status would be maintained
during (some) conversions. At the time, analogous components were limited to
individual component pairs: `red`/`green`/`blue` across the various RGB-style
spaces, `lightness` between HSL/Lab/LCH, and so on. Sass implemented these by
retaining missing channels during all color conversions.

[analogous components]: https://drafts.csswg.org/css-color-4/#analogous-components

Since then, the CSS spec has expanded the notion of analogous components to
include "analogous sets" as well, defined as the complement of any subset of
analogous components. If all channels in an analogous set are missing in the
source color, the corresponding channels should be marked as missing in the
target color as well. This proposal implements this change.

[CSS Color 4]: https://drafts.csswg.org/css-color-4/

## Summary

> This section is non-normative.

This proposal changes the color space conversion procedure to mark as missing
all channels in the target color that correspond to a missing analogous set in
the source color.

### Design Decisions

#### Which Conversions?

According to the text of [CSS Color 4] at time of writing, analogous channels
and sets are *only* relevant to conversions that happen as part of [color
interpolation], and should not be applied to other color conversions. In spite
of this, Sass already preserves individual analogous missing components in *all*
color space conversions. This proposal continues this pattern by applying
analogous components to all conversions as well.

[color interpolation]: https://drafts.csswg.org/css-color-4/#interpolation

CSS working group members have (so far informally) indicated that this is the
intended meaning of the spec, even if it's not the meaning as written, in [this
issue]. In addition, Sass's internal color conversion logic isn't *necessarily*
tied closely to the CSS spec; although each individual pair of color spaces
follows CSS-specified conversion logic, the surrounding spec text is separate in
Sass and can have some differences without being a *de jure* violation of CSS
compatibility.

[this issue]: https://github.com/w3c/csswg-drafts/issues/10210#issuecomment-4298678883

#### No Deprecation

Despite being a user-visible breaking change, this proposal does not include a
deprecation process. Such a process would be high friction with relatively low
value: we would have to emit a deprecation warning every time a user converted a
color with a set of analogous missing components, and that user would have no
clear way of disabling this warning or opting into the new behavior early.

There are several other concerns that point towards avoiding a deprecation
process here. Working with missing components is a narrow use-case to begin
with, and something we don't expect a wide range of users to be concerned with
at all. For those who are, the new behavior is a clear and immediate
improvement. It more accurately represents the intended semantics of missing
channels as having absent or templatable values, and so any behavioral
differences are likely to be improvements rather than regressions.

## Definitions

### Missing Components

Change the definition of [missing components] by removing the section that
defines "analogous components". Any references to "analogous components"
elsewhere in the specification should now refer to the definition of that term
in CSS Color 4.

[missing components]: ../spec/types/color.md#missing-components

### Analogous Mappings

An *analogous mapping* is a pair that includes a set of channels from one color
space and a set of channels from another color space, where those two sets are
either two [analogous sets] or two single-element sets representing two
[analogous components].

[analogous sets]: https://drafts.csswg.org/css-color-4/#analogous-set

> Each pair of color spaces has a finite, well-defined set of analogous
> mappings. Per the CSS spec, the set of all components of each space always
> represents an analogous mapping.
>
> For example, the mappings between Lab and LCH are:
>
> * `({lightness}, {lightness})`
> * `({a, b}, {chroma, hue})`
> * `({lightness, a, b}, {lightness, chroma, hue})`

## Procedures

### Converting a Color

Remove the assignment of the `missing` variable. Replace the iteration of this variable with:

* For each [analogous mapping] `(origin-channels, target-channels)` between
  `origin-color`'s space and `target-space`:

  * If each channel in `origin-channels` is missing in `origin-color`, mark each
    channel in `target-channels` as missing in `color`.

[analogous mapping]: #analogous-mappings
