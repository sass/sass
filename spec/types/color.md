# Colors

## Table of Contents

* [Definitions](#definitions)
  * [Color](#color)
  * [Legacy Color](#legacy-color)
  * [Known Color Space](#known-color-space)
  * [Predefined Color Spaces](#predefined-color-spaces)
  * [Missing Components](#missing-components)
  * [Powerless Components](#powerless-components)
* [Types](#types)
  * [Equality](#equality)
  * [Serialization](#serialization)
    * [Serialization of Non-Legacy Colors](#serialization-of-non-legacy-colors)
    * [Serialization of Out-of-Gamut RGB Colors](#serialization-of-out-of-gamut-rgb-colors)

## Definitions

### Color

### Legacy Color

[legacy color]: #legacy-color

> Both Sass and CSS have similar legacy behavior that relies on all colors
> being interchangeable as part of a shared `srgb` color space. While the new
> color spaces will opt users into new default behavior, some legacy color
> spaces behave differently for the sake of backwards-compatibility.

Colors in the `rgb`, `hsl`, or `hwb` [color spaces](#known-color-space) are
considered *legacy colors*. The output of a legacy color is not required to
match the input color space, and several color functions maintain legacy
behavior when manipulating legacy colors.

Legacy colors that have [missing] components are
[serialized as non-legacy colors](#serialization-of-non-legacy-colors).

> This includes colors defined using the CSS color names, hex syntax, `rgb()`,
> `rgba()`, `hsl()`, `hsla()`, or `hwb()` -- along with colors that are
> manually converted into legacy color spaces.

### Known Color Space

Each known color space has a name and an ordered list of associated channels.
Each channel has a name, and an optional associated unit. Color space names are
matched case-insensitively and referred to using unquoted strings, and are
always emitted as unquoted lowercase strings by inspection functions. Color
channel names are matched case-sensitively and referred to using quoted strings.

Values outside a *bounded gamut* range (including infinity or negative infinity)
are valid but are considered *out of gamut* for the given color space. They
remain un-clamped unless the gamut is specifically marked as "clamped", in which
case they're clamped *only* when constructing the color from its global
constructor function or `color.adjust()`.

Some color spaces include a *polar angle* channel (so far exclusively called
`hue`). Polar-angle hues represent an angle position around a given hue wheel,
using a CSS `<angle>` dimension or number (interpreted as a `deg` value when no
units are passed), and are serialized with `deg` units. Polar angle channels are
normalized using modulo to fall within their conventional range. Any channel
that is not polar-angle is considered *scalable*.

All channels have a conventional range of values, whose bounds are referred to
as the channel's *lower bound* and *upper bound* (although for unbounded
channels these bounds aren't enforced anywhere). For scalable channels,
percentages provided by the user are converted to match these bounds. `0%`
always refers to the value 0, `100%` always refers to the upper bound of the
range, and `-100%` always refers to the negative value of the upper bound of the
range. (In many cases this is considered outside the conventional scope of the
color space, but the `a` and `b` channels do conventionally cover negative
values as well.)

Colors specified using a CSS color keyword or the hex notation are treated as
`rgb` colors.

The known color spaces and their channels are:

* `rgb` (RGB, legacy):
  * `red`, `green`, `blue`:
    * gamut: bounded, clamped
    * range: `[0,255]`

      > Percentages `[0%,100%]` map to the `[0,255]` range.

* `hwb` (RGB, legacy):
  * `hue` (polar angle):
    * associated unit: `deg`
    * range: `[0,360]`
  * `whiteness`, `blackness`:
    * associated unit: `%`
    * gamut: bounded
    * range: `[0,100]`

* `hsl` (RGB, legacy):
  * `hue` (polar angle):
    * associated unit: `deg`
    * range: `[0,360]`
  * `saturation`:
    * gamut: bounded, clamped (lower bound only)
    * associated unit: `%`
    * range: `[0,100]`
  * `lightness`:
    * gamut: bounded
    * associated unit: `%`
    * range: `[0,100]`

* `srgb`, `srgb-linear`, `display-p3`, `a98-rgb`, `prophoto-rgb`,
  `rec2020` (RGB):
  * `red`, `green`, `blue`:
    * gamut: bounded
    * range: `[0,1]`

* `xyz`, `xyz-d50`, `xyz-d65`:
  * `x`, `y`, `z`:
    * gamut: un-bounded
    * range: `[0,1]`

* `lab`:
  * `lightness`:
    * gamut: un-bounded, clamped
    * associated unit: `%`
    * range: `[0,100]`

  * `a`, `b`:
    * gamut: un-bounded
    * range: `[-125,125]`

* `lch`:
  * `lightness`:
    * gamut: un-bounded, clamped
    * associated unit: `%`
    * range: `[0,100]`

  * `chroma`:
    * gamut: un-bounded, clamped (lower bound only)
    * range: `[0,150]`

  * `hue` (polar angle):
    * associated unit: `deg`
    * range: `[0,360]`

* `oklab`:
  * `lightness`:
    * gamut: un-bounded, clamped
    * associated unit: `%`
    * range: `[0,1]`

  * `a`, `b`:
    * gamut: un-bounded
    * range: `[-0.4,0.4]`

* `oklch`:
  * `lightness`:
    * gamut: un-bounded, clamped
    * associated unit: `%`
    * range: `[0,1]`

  * `chroma`:
    * gamut: un-bounded, clamped (lower bound only)
    * range: `[0,0.4]`

  * `hue` (polar angle):
    * associated unit: `deg`
    * range: `[0,360]`

If a color with negative saturation or chroma would be created in the `hsl`,
`lch`, or `oklch` spaces by any means, instead create a color with the absolute
value of that saturation or chroma and the hue rotated by 180deg.

> This is an equivalent color, and in fact passing channels with negative
> saturation/chroma through conversion to other color spaces and back will
> produce this same result. This helps ensure that identical colors are
> represented uniformly.
>
> Note that this conversion happens *after* any clipping, so `hsl(0deg -50%
> 50%)` returns `hsl(0deg 0% 50%)`, not `hsl(180deg 50% 50%)`.

The `xyz-d65` space is an alias for `xyz`, and implementations should report the
space name as `xyz` even if it was originally written as `xyz-d65`.

### Predefined Color Spaces

> "Predefined color spaces" can be described using the `color()` function.

The *predefined RGB spaces* are:

* `srgb`
* `srgb-linear`
* `display-p3`
* `a98-rgb`
* `prophoto-rgb`
* `rec2020`

The *predefined XYZ spaces* are:

* `xyz`
* `xyz-d50`
* `xyz-d65` (an alias for `xyz`)

### Missing Components

In some cases, a color can have one or more missing components (channel or alpha
values). Unless explicitly specified otherwise, missing components are treated
as though they had the value `0`.

> Some situations where missing components are treated specially include
> interpolation, the `==` operator, and color space conversion.

For the sake of [interpolating] between colors with missing components, the
following *analogous components* are defined by [CSS Color Level 4][color-4]:

| Category      | Components          |
| ------------- | ------------------- |
| Reds          | red, x              |
| Greens        | green, y            |
| Blues         | blue, z             |
| Lightness     | lightness           |
| Colorfulness  | chroma, saturation  |
| Hue           | hue                 |

[interpolating]: ../built-in-modules/color.md#interpolating-colors

### Powerless Components

In some color spaces, it is possible for a channel value to be considered
*powerless* in certain circumstances.

* `hsl`:

  * If the `saturation` value is `0%`, then the `hue` channel is powerless.

* `hwb`:

  * If `whiteness + blackness` is greater than or equal to `100%`, then the
    `hue` channel is powerless.

* `lch`/`oklch`:

  * If the `chroma` value is 0%, then the `hue` channel is powerless.

> In some circumstances, conversion between color spaces will mark powerless
> components in the output as missing.

## Types

> Note that channel values are stored as specified, maintaining precision where
> possible, even when the values are out-of-gamut for the [known color space].

The value type known as a *color* has three components

* A *color space* that is a [known color space].

* An ordered list of *channel*s, each one containing a [double] or the special
  value `none`.

* An *alpha* that is either the special value `none` or a [double] between
  `0-1` (inclusive).

  > While it's valid to specify numbers outside this range, they are
  > meaningless, and can be clamped by input functions when generating a color.

[known color space]: #known-color-space
[double]: ../types/number.md#double

### Equality

For determining *equality* between two colors `color1` and `color2`:

* If both colors are [legacy colors] in different spaces, return
  `color.to-space(color1, rgb) == color.to-space(color2, rgb)`.

* If the colors are not in the same color space, return false.

* If either color has a channel (including alpha) marked as [missing] that the
  other does not, return false.

* Return whether each matching pair of non-missing channel values (including
  alpha) is fuzzy-equal.

  > Since this definition no longer involves rounding channels for the legacy
  > RGB space, it is potentially a breaking change. Moving forward, `rgb(0 0
  > 0.6) != rgb(0 0 1)`.

[legacy colors]: #legacy-color

### Serialization

#### Serialization of Non-Legacy Colors

To serialize a non-legacy color `color`:

* If `color` has a clamped channel whose value is out-of-bounds, emit a CSS
  [`<color>`] expression that evaluates to `color`'s value, then return.

  > The specific syntax here is left up to implementations, based on the
  > specifics of the color in question and the realities of browser support.
  > Two options include:
  >
  > * `color-mix()`. For example, `color-mix(in lab, color(xyz 1 1 1) 100%,
  >   black)` will losslessly convert `color(xyz 1 1 1)` into `lab` where the
  >   native `lab` syntax would clamp the lightness at `100%`.
  >
  > * [Relative color syntax], which per spec is never clamped. For example,
  >   while the lightness in `lab(200 50 50)` is clamped, the lightness in
  >   `lab(from black 200 50 50)` is not.
  >
  > At the time of writing, browser support is patchy for these syntaxes and no
  > browser correctly avoids clipping in all the cases we're relying on.
  > Although we have no way of ensuring that all color values representable in
  > Sass can be correctly loaded by browsers, this spec aims to ensure that Sass
  > generates the correct value according to the CSS spec and that
  > implementations have enough flexibility within that to target the shifting
  > landscape of what browsers actually support.

* Let `components` be an empty string.

* For each `channel` in `color`'s channels:

  * If `channel` contains the special value `none`, append the unquoted string
    "none" to `components`.

  * Otherwise:

    * Let `channel-number` be a number with the same value as `channel` and unit
      given by `channel`'s associated unit, if it has one.

    * Append the result of serializing `channel-number` to `components`.

  * If `channel` isn't the last channel, append a space to `components`.

* If `color`'s alpha value isn't `1`, append the unquoted string " / " followed
  by the result of serializing a unitless number whose value is `color`'s alpha
  value to `components`.

* Let `space-name` be `color`'s space name as an unquoted lowercase string.

* If `color`'s space is not a [predefined color space], emit `space-name`
  followed by "(", `components`, and then ")".

  > Since a [predefined color space] is defined as a [known color space] that
  > uses the `color()` syntax, this is a reliable way to get the remaining
  > known color spaces that provide their own function syntax.

* Otherwise, emit "color(", followed by `space-name`, " ", `components`, and
  then ")".

[`<color>`]: https://drafts.csswg.org/css-color-5/#typedef-color
[predefined color space]: #predefined-color-spaces

#### Serialization of Out-of-Gamut RGB Colors

To serialize an out-of-gamut color `color` in the `rgb` space:

* Let `hsl` be the result of [converting] `color` into the `hsl` space.

* Return the result of serializing `hsl`.

