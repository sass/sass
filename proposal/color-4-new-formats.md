# CSS Color Level 4, New Color Formats: Draft 1

*([Issue](https://github.com/sass/sass/issues/2831))*

This proposal adds several new color formats to Sass -- including `hwb()`,
`lab()`, and `lch()` as defined in [CSS Color Level 4][color-4].

[color-4]: https://www.w3.org/TR/css-color-4/

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Semantics](#semantics)

## Background

> This section is non-normative.

The new modular syntax and name-spaced `sass:color` module allows us to provide
new CSS color formats before any browser implementations. The explicit purpose
of these Sass-module function will be providing pre-processed conversion to an
existing, well-supported color format.

While `hwb()` is defined in the sRGB space, with simple conversion, `lab()` &
`lch()` colors are defined in a device-indipendant and *perceptually uniform*
CIE space, much larger than the avialable sRGB gamut. That uniformity is useful
for programmatic color manipulation and consistent contrast ratios, but careful
conversions between CIElab and sRGB will be required.

The Sass functions will use a *relative-colorimetric* approach to converting
out-of-gamut colors, so that they resolve to the nearest sRGB equivalent.
Colors that convert cleanly in-gamut will be un-affected. Since we are eagerly
resolving colors into sRGB space, while browsers can take a more nuanced
approach, it's important that we don't override users ability to use the CSS
functions once available.

With that in mind:
- None of these functions will be available on the global namespace.
- None of these functions will accept *special number string* or
  *special variable string* values that can only be resolved in CSS.

## Summary

> This section is non-normative.

This proposal defines Sassified versions of all the color functions in
[CSS Color Level 4][color-4]. Since the CIE color space defines the entire
gamut of visible color, much larger than the target sRGB gamut, out-of-range
color definitions will be clipped using a *relative-colorimetric* approach that
leaves in-gamut colors un-affected.

### Design Decisions

Conversions between color formats and color-spaces are well defined, and the
CSS specification provides code samples to help with implementation, but
handling out-of-gamut colors can be much more complex. There is no single
"rendering intent" that works in every instance, and no well-defines algoythms
even once a rendering intent is determined.

Our solution attempts to balance several concerns:

- With programmatic color manipulation, it is useful to allow "overshooting"
  the gamut: such as allowing an `hsl()` color to have saturation and lightness
  higher than `100%` or lower than `0%` -- also colors that would be considered
  outside of the sRGB color space, but resolve to their nearest in-gamut proxy
  without changing hue. This requires silent failure for out-of-gamut conversion.
- Authors should have as much clarity and control as possible, meaning in-gamut
  colors should not be altered in unexpected ways. That eliminates *perceptual*
  rendering as an option. Similarly, clipping individual RGB color channels to
  at `0` and `255` can cause unwanted hue-shift.

The best way to match existing behavior and designer intent is to lock hue and
lightness in place, and reduce chroma/saturation until we either find a proxy or resolve to black/white.

## Semantics
