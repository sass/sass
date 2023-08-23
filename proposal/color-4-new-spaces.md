# CSS Color Level 4, New Color Spaces: Draft 1.7

*([Issue](https://github.com/sass/sass/issues/2831))*

This proposal adds Sass support for several new CSS color spaces defined in
[CSS Color Level 4][color-4], including access to non-RGB color models and
colors outside the sRGB gamut.

[color-4]: https://www.w3.org/TR/css-color-4/

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Rules of Thumb](#rules-of-thumb)
  * [Standard CSS Color Functions](#standard-css-color-functions)
    * [`oklab()` and `oklch()`](#oklab-and-oklch)
    * [`lab()` and `lch()`](#lab-and-lch)
    * [`hwb()`](#hwb)
    * [`color()`](#color)
  * [New Sass Color Functions](#new-sass-color-functions)
    * [`color.channel()`](#colorchannel)
    * [`color.is-missing()`](#coloris-missing)
    * [`color.space()`](#colorspace)
    * [`color.is-in-gamut()`, `color.is-legacy()`](#coloris-in-gamut-coloris-legacy)
    * [`color.to-gamut()`](#colorto-gamut)
    * [`color.is-powerless()`](#coloris-powerless)
    * [`color.same()`](#colorsame)
  * [Existing Sass Color Functions](#existing-sass-color-functions)
    * [`color.scale()`, `color.adjust()`, and `color.change()`](#colorscale-coloradjust-and-colorchange)
    * [`color.mix()`](#colormix)
    * [Deprecations](#deprecations)
  * [Design Decisions](#design-decisions)
* [Definitions](#definitions)
  * [Color](#color)
  * [Legacy Color](#legacy-color)
  * [Color Equality](#color-equality)
  * [Known Color Space](#known-color-space)
  * [Predefined Color Spaces](#predefined-color-spaces)
  * [Missing Components](#missing-components)
  * [Powerless Components](#powerless-components)
  * [Color Interpolation Method](#color-interpolation-method)
* [Serialization](#serialization)
  * [Serialization of Non-Legacy Colors](#serialization-of-non-legacy-colors)
* [Procedures](#procedures)
  * [Looking Up a Known Color Space](#looking-up-a-known-color-space)
  * [Converting a Color](#converting-a-color)
* [CSS-Converting a Color Space](#css-converting-a-color-space)
  * [Gamut Mapping](#gamut-mapping)
  * [Parsing Color Components](#parsing-color-components)
  * [Percent-Converting a Number](#percent-converting-a-number)
  * [Validating a Color Channel](#validating-a-color-channel)
  * [Normalizing Color Channels](#normalizing-color-channels)
  * [Interpolating Legacy Colors](#interpolating-legacy-colors)
  * [Interpolating Colors](#interpolating-colors)
    * [Premultiply Transparent Colors](#premultiply-transparent-colors)
    * [Hue Interpolation](#hue-interpolation)
  * [Scaling a Number](#scaling-a-number)
* [New Color Module Functions](#new-color-module-functions)
  * [`color.space()`](#colorspace-1)
  * [`color.to-space()`](#colorto-space)
  * [`color.is-legacy()`](#coloris-legacy)
  * [`color.is-powerless()`](#coloris-powerless-1)
  * [`color.is-in-gamut()`](#coloris-in-gamut)
  * [`color.to-gamut()`](#colorto-gamut-1)
  * [`color.channel()`](#colorchannel-1)
  * [`color.is-missing()`](#coloris-missing-1)
  * [`color.same()`](#colorsame-1)
* [Modified Color Module Functions](#modified-color-module-functions)
  * [`color.hwb()`](#colorhwb)
  * [`color.mix()`](#colormix-1)
  * [`color.change()`](#colorchange)
  * [`color.adjust()`](#coloradjust)
  * [`color.scale()`](#colorscale)
  * [`color.complement()`](#colorcomplement)
  * [`color.invert()`](#colorinvert)
  * [`color.grayscale()`](#colorgrayscale)
  * [`color.ie-hex-str()`](#colorie-hex-str)
* [New Global Functions](#new-global-functions)
  * [`hwb()`](#hwb-1)
  * [`lab()`](#lab)
  * [`lch()`](#lch)
  * [`oklab()`](#oklab)
  * [`oklch()`](#oklch)
  * [`color()`](#color-1)
* [Modified Global Functions](#modified-global-functions)
  * [`rgb()` and `rgba()`](#rgb-and-rgba)
  * [`hsl()` and `hsla()`](#hsl-and-hsla)
* [Deprecated Functions](#deprecated-functions)
  * [`color.red()`, `red()`](#colorred-red)
  * [`color.green()`, `green()`](#colorgreen-green)
  * [`color.blue()`, `blue()`](#colorblue-blue)
  * [`color.hue()`, `hue()`](#colorhue-hue)
  * [`color.saturation()`, `saturation()`](#colorsaturation-saturation)
  * [`color.lightness()`, `lightness()`](#colorlightness-lightness)
  * [`color.whiteness()`](#colorwhiteness)
  * [`color.blackness()`](#colorblackness)
  * [`color.alpha()`](#coloralpha)
  * [`adjust-hue()`](#adjust-hue)
  * [`saturate()`](#saturate)
  * [`desaturate()`](#desaturate)
  * [`transparentize()`, `fade-out()`](#transparentize-fade-out)
  * [`opacify()`, `fade-in()`](#opacify-fade-in)
  * [`lighten()`](#lighten)
  * [`darken()`](#darken)

## Background

> This section is non-normative.

Historically, CSS has only provided authors with color formats using the RGB
model, limited to the sRGB gamut. As CSS is used for more applications (such as
print) and displays continue to improve, those limitations become more clear.
The [CSS Color Level 4][color-4] specification defines a number of new color
spaces, each with its own syntax, representing both new color models and
wider RGB gamuts.

* A *color model* is a mathematical approach to representing colors and their
  relationships. Historically, RGB has been the dominant color model for both
  computer monitors and web browsers. Lately, CIELab and OKLab models have
  shown significant benefits by providing a more *perceptually uniform*
  distribution of colors, so that similar mathematical adjustments achieve
  visually similar results.

* A *color space* is the result of projecting a color model into a coordinate
  system. In CSS, each color syntax describes a specific (and often unique)
  color space. For example, `rgb()`, `color(srgb)`, and `color(display-p3)` all
  project the RGB color model into cubic coordinate systems, while `hsl()`
  projects the same color model into a cylindrical (polar-angle) space.
  Similarly, `oklab()` and `oklch()` provide different coordinate projections
  of the OKLab model.

* A *color gamut* is the full range of colors that can be described in a color
  space. Historically, all CSS syntaxes have been limited to the sRGB gamut.
  However, modern computer monitors often support wider gamuts. Color spaces
  like `srgb` and `display-p3` describe different gamuts of color, using the
  same underlying RGB color model, and differently-mapped cubic coordinates.

These terms can get a bit confusing, since there is so much overlap. The term
'RGB' can refer to a color model, a color space, a coordinate system, and also
a color function. The 'RGB' color space is identical to the 'sRGB' space, and
both describe the 'sRGB' gamut. But we have both `rgb()` and `color(srgb)`
syntax, in order to distinguish legacy from non-legacy variations. They also
have different coordinate systems, `rgb()` accepts a range from 0-255, while
`color(srgb)` accept values from 0-1.

The result is that authors can generally think of each color syntax as a unique
space, and each space implies an associated gamut and coordinate system. Color
spaces become a simple way to refer to all parts combined. We've used that same
approach in Sass, such that the name of a color space can be used to reference
the associated gamut. For example, `color.to-gamut($color, hsl)` and
`color.to-gamut($color, srgb)` have the same meaning, both mapping a color into
the sRGB gamut.

Since all CSS colors up until this point have been restricted to RGB math in
the sRGB gamut, Sass has historically treated all color formats and spaces as
interchangeable. That has allowed authors to inspect and manipulate colors in
any space, without careful management or gamut mapping. It has also allowed
Sass to output the most browser-compatible CSS format for any given color.

In order to support the color spaces in CSS, Sass will need to start tracking
the space/gamut associated with any given color, and provide author tools for
managing those color spaces/gamuts. In addition to supporting the new color
space functions, we plan to update all functions in the color module, and
provide some additional space and gamut management and inspection functions.

## Summary

> This section is non-normative.

This proposal defines a Sass representation of colors with color spaces,
Sassified versions of all the color functions in [CSS Color Level 4][color-4],
updated definitions of existing Sass functions to accommodate color spaces, and
several new Sass-specific color functions as well.

### Rules of Thumb

There are several rules of thumb for working with color spaces in Sass:

* The `rgb`, `hsl`, and `hwb` spaces are considered "legacy spaces", and will
  often get special handling for the sake of backwards compatibility. Colors
  defined using hex notation or CSS color names are considered part of the `rgb`
  color space. Legacy colors are emitted in the most compatible format. This
  matches CSS's own backwards-compatibility behavior.

* Otherwise, any color defined in a given space will remain in that space, and
  be emitted in that space.

* Authors can explicitly convert a color's space by using `color.to-space()`.
  This can be useful to enforce non-legacy behavior, by converting into a
  non-legacy space, or to ensure the color output is compatible with older
  browsers by converting colors into a legacy space before emitting.

* The `srgb` color space is equivalent to `rgb`, except that one is a legacy
  space, and the other is not. They also use different coordinate systems, with
  `rgb()` accepting a range from 0-255, and `srgb` using a range of 0-1.

* Color functions that allow specifying a color space for manipulation will
  always use the source color space by default. When an explicit space is
  provided for manipulation, the resulting color will still be returned in the
  same space as the origin color. For `color.mix()`, the first color parameter
  is considered the origin color.

* All legacy and RGB-style spaces represent bounded gamuts of color. Since
  mapping colors into gamut is a lossy process, it should generally be left to
  browsers, which can map colors as-needed, based on the capabilities of a
  display. For that reason, out-of-gamut channel values are maintained by Sass
  whenever possible, even when converting into gamut-bounded color spaces. The
  only exception is that `hsl` and `hwb` color spaces are not able to express
  out-of-gamut color, so converting colors into those spaces will gamut-map the
  colors as well. Authors can also perform explicit gamut mapping with the
  `color.to-gamut()` function.

* Legacy browsers require colors in the `srgb` gamut. However, most modern
  displays support the wider `display-p3` gamut.

### Standard CSS Color Functions

#### `oklab()` and `oklch()`

The `oklab()` (cubic) and `oklch()` (cylindrical) functions provide access to an
unbounded gamut of colors in a perceptually uniform space. Authors can use these
functions to define reliably uniform colors. For example, the following colors
are perceptually similar in lightness and saturation:

```scss
$pink: oklch(64% 0.196 353); // hsl(329.8 70.29% 58.75%)
$blue: oklch(64% 0.196 253); // hsl(207.4 99.22% 50.69%)
```

The `oklch()` format uses consistent "lightness" and "chroma" values, while the
`hsl()` format shows dramatic changes in both "lightness" and "saturation". As
such, `oklch` is often the best space for consistent transforms.

#### `lab()` and `lch()`

The `lab()` and `lch()` functions provide access to an unbounded gamut of colors
in a space that's less perpetually-uniform but more widely-adopted than OKLab
and OKLCH.

#### `hwb()`

Sass now supports a top-level `hwb()` function that uses the same syntax as
CSS's built-in `hwb()` syntax.

#### `color()`

The new `color()` function provides access to a number of specialty spaces. Most
notably, `display-p3` is a common space for wide-gamut monitors, making it
likely one of the more popular options for authors who simply want access to a
wider range of colors. For example, P3 greens are significantly 'brighter' and
more saturated than the greens available in sRGB:

```scss
$fallback-green: rgb(0% 100% 0%);
$brighter-green: color(display-p3 0 1 0);
```

Sass will natively support all predefined color spaces declared in the Colors
Level 4 specification.

### New Sass Color Functions

#### `color.channel()`

This function returns the value of a single channel in a color. By default, it
only supports channels that are available in the color's own space, but you can
pass the `$space` parameter to return the value of the channel after converting
to the given space.

```scss
$brand: hsl(0 100% 25.1%);

// result: 25.1%
$hsl-lightness: color.channel($brand, "lightness");

// result: 37.67%
$oklch-lightness: color.channel($brand, "lightness", $space: oklch);
```

#### `color.is-missing()`

This function returns if a given channel value is 'missing' (set to `none`).
This is necessary, since `color.channel` returns `0` for missing channels.
Since color-space conversion can change what channels are missing, this
function only supports inspecting channels that are part of the color's own
space.

```scss
$brand: hsl(none 100% 25.1%);

// result: false
$missing-lightness: color.is-missing($brand, "lightness");

// result: true
$missing-hue: color.is-missing($brand, "hue");
```

#### `color.space()`

This function returns the name of the color's space.

```scss
// result: hsl
$hsl-space: color.space(hsl(0 100% 25.1%));

// result: oklch
$oklch-space: color.space(oklch(37.7% 38.75% 29.23deg));
```

#### `color.is-in-gamut()`, `color.is-legacy()`

These functions return various facts about the color. `color.is-in-gamut()`
returns whether the color is in-gamut for its color space (as opposed to having
one or more of its channels out of bounds, like `rgb(300 0 0)`).
`color.is-legacy()` returns whether the color is a legacy color in the `rgb`,
`hsl`, or `hwb` color space.

#### `color.to-gamut()`

This function returns a color that is in the given gamut, using the recommended
[CSS Gamut Mapping Algorithm][css-mapping] to 'map' out-of-gamut colors into
the desired gamut with as little perceptual change as possible. In many cases
this can be more reliable for generating fallback values, rather than the
'channel clipping' approach used by current browsers.

```scss
$green: oklch(0.8 2 150);

// oklch(0.91 0.14 164)
$rgb: color.to-gamut($green, "srgb");

// oklch(0.91 0.16 163)
$p3: color.to-gamut($green, "display-p3");
```

#### `color.is-powerless()`

This function returns whether a given channel is "powerless" in the given color.
This is a special state that's defined for individual color spaces, which
indicates that a channel's value won't affect how a color is displayed.

```scss
$grey: hsl(0 0% 60%);

// result: true, because saturation is 0
$hue-powerless: color.is-powerless($grey, "hue");

// result: false
$hue-powerless: color.is-powerless($grey, "lightness");
```

#### `color.same()`

This function returns whether two colors will be displayed the same way, even if
this requires converting between spaces. This is unlike the `==` operator, which
always considers colors in different non-legacy spaces to be inequal.

```scss
$orange-rgb: #ff5f00;
$orange-oklch: oklch(68.72% 20.966858279% 41.4189852913deg);

// result: false
$equal: $orange-rgb == $orange-oklch;

// result: true
$same: color.same($orange-rgb, $orange-oklch);
```

### Existing Sass Color Functions

#### `color.scale()`, `color.adjust()`, and `color.change()`

By default, all Sass color transformations are handled and returned in the color
space of the original color parameter. However, all relevant functions now allow
specifying an explicit color space for transformations. For example, lightness &
darkness adjustments are most reliable in `oklch`:

```scss
$brand: hsl(0 100% 25.1%);

// result: hsl(0 100% 43.8%)
$hsl-lightness: color.scale($brand, $lightness: 25%);

// result: hsl(5.76 56% 45.4%)
$oklch-lightness: color.scale($brand, $lightness: 25%, $space: oklch);
```

Note that the returned color is still emitted in the original color space, even
when the adjustment is performed in a different space.

#### `color.mix()`

The `color.mix()` function will retain its existing behavior for legacy color
spaces, but for new color spaces it will match CSS's "color interpolation"
specification. This is how CSS computes which color to use in between two colors
in a gradient or an animation.

#### Deprecations

A number of existing functions only make sense for legacy colors, and so are
being deprecated in favor of color-space-friendly functions like
`color.channel()` and `color.adjust()`:

* `color.red()`
* `color.green()`
* `color.blue()`
* `color.hue()`
* `color.saturation()`
* `color.lightness()`
* `color.whiteness()`
* `color.blackness()`
* `color.alpha()`
* `adjust-hue()`
* `saturate()`
* `desaturate()`
* `transparentize()`/`fade-out()`
* `opacify()`/`fade-in()`
* `lighten()`/`darken()`

### Design Decisions

Most of the design decisions involved in the proposal are based on the
[CSS Color Level 4][color-4] specification, which we have tried to emulate as
closely as possible, while maintaining support for legacy projects. In some
cases, that required major changes to the way Sass handles colors:

1. RGB-style channel values are no longer clamped to the gamut of a color space,
   except for the `hsl` and `hwb` spaces, which are unable to represent
   out-of-gamut colors. By default Sass will output CSS with out-of-gamut
   colors, because browsers can provide better gamut mapping based on the user
   device capabilities. However, authors can use the provided `color.to-gamut()`
   function to enforce mapping a color into a specific gamut.
2. RGB-style channel values are no longer rounded to the nearest integer, since
   the spec now requires maintaining precision wherever possible. This is
   especially important in RGB spaces, where color distribution is inconsistent.

Different color spaces often represent different color-gamuts, which can present
a new set of problems for authors. Some color manipulations are best handled
in a wide-gamut space like `oklch`, but (for now) authors will likely prefer
emitting legacy colors that work in existing and legacy browsers. While that
is likely to change in the long term, we think it's a worthwhile tradeoff to
prioritize author control and legacy color use-cases. Authors who do choose to
emit non-legacy colors are less likely to be working in legacy color spaces to
begin with. So we've established the following guidelines for color conversion
and mapping in Sass color functions:

* Every color function returns a color in the same space as the original color,
  no matter what space was used for transformations. The only exception is
  `color.to-space()`, which can be used for manual space conversion. Functions
  that accept two colors (e.g. `color.mix()`) return a color in the same space
  as the first color argument.

* No color function performs gamut-mapping on out-of-gamut channels, except
  `color.to-gamut()`, which can be used for manual gamut-mapping.

Browsers currently use channel-clipping rather than the proposed
[css gamut mapping algorithm][css-mapping] to handle colors that cannot be
shown correctly on a given display. We've decided to provide `color.to-gamut()`
as a way for authors to opt-into the proposed behavior, aware that browsers
may eventually choose to provide a different algorithm. If that happens, we
will consider adding an additional algorithm-selection argument. However, the
primary goal of this function is not to match CSS behavior, but to provide a
better mapping than the default channel-clipping.

We are not attempting to support all of [CSS Color Level 5][color-5] at this
point, since it is not yet implemented in browsers. However, we have used it as
a reference while updating color manipulation functions such as `color.mix()`.

There is also an [open issue in CSS] to determine how the [relative color syntax]
from Level 5 should handle [missing] color components. Since the relative color
syntax provides similar functionality to the Sass `color.adjust()` and
`color.scale()` functions, we have decided to wait and match the CSS behavior
once it is specified. In the meantime, Sass will throw errors when trying to
adjust or scale a missing component. This is not the ideal behavior, but it
provides us with the most flexibility to change our behavior in the future.

[open issue in CSS]: https://github.com/w3c/csswg-drafts/issues/7771
[color-5]: https://www.w3.org/TR/css-color-5/
[relative color syntax]: https://drafts.csswg.org/css-color-5/#relative-colors

Thanks to the editors of the CSS Color Level 4 specification (Tab Atkins Jr.,
Chris Lilley, and Lea Verou) for answering our many questions along the way. We
also used Chris and Lea's [Color.js](https://colorjs.io/) library as a
reference as we developed this proposal.

## Definitions

### Color

> Note that channel values are stored as specified, maintaining precision where
> possible, even when the values are out-of-gamut for the [known color space].

A *color* is an object with several parts:

* A *color space* that is either a [known color space] or an unquoted string.

* An ordered list of *channel*s, each one containing a [double] or the special
  value `none`.

* An *alpha* that is either the special value `none` or a [double] between
  `0-1` (inclusive).

  > While it's valid to specify numbers outside this range, they are
  > meaningless, and can be clamped by input functions when generating a color.

[known color space]: #known-color-space
[double]: ../spec/types/number.md#double

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

### Color Equality

For determining *equality* between two colors:

* If both colors are [legacy colors](#legacy-color):

  * Set each color to the result of [converting] the color into `rgb` space.

  * Colors are only equal if their channel and alpha values are fuzzy-equal.

    > Since this definition no longer involves rounding channels, it is
    > potentially a breaking change. Moving forward,
    > `rgb(0 0 0.6) != rgb(0 0 1)`.

* Otherwise, colors are only equal when they're in the same color space and
  their channel and alpha values are fuzzy-equal.

### Known Color Space

Each known color space has a name and an ordered list of associated channels.
Each channel has a name, and an associated unit where allowed. Space and
channel names match unquoted strings, ignoring case. They are always emitted as
unquoted lowercase strings by inspection functions.

Values outside a *bounded gamut* range (including infinity or negative infinity)
are valid but are considered *out of gamut* for the given color space. They
remain un-clamped unless the gamut is specifically marked as "clamped". If the
channel is bounded, or has a percentage mapping, then the channel is considered
*scalable*.

Some color spaces use a *polar angle* value for the `hue` channel. Polar-angle
hues represent an angle position around a given hue wheel, using a CSS `<angle>`
dimension or number (interpreted as a `deg` value), and are serialized with
`deg` units.

Colors specified using a CSS color keyword or the hex notation are converted
to `rgb` and serialized as part of the `rgb` color space.

The known color spaces and their channels are:

* `rgb` (RGB, legacy):
  * `red`, `green`, `blue`:
    * gamut: bounded
    * number: `[0,255]`

      > Percentages `[0%,100%]` map to the `[0,255]` range.

* `hwb` (RGB, legacy):
  * `hue`:
    * degrees: polar angle
  * `whiteness`, `blackness`:
    * gamut: bounded
    * percentage: `[0%,100%]`

* `hsl` (RGB, legacy):
  * `hue`:
    * degrees: polar angle
  * `saturation`:
    * gamut: bounded
    * percentage: `[0%,100%]`
  * `lightness`:
    * gamut: bounded, clamped
    * percentage: `[0%,100%]`

* `srgb`, `srgb-linear`, `display-p3`, `a98-rgb`, `prophoto-rgb`,
  `rec2020` (RGB):
  * `red`, `green`, `blue`:
    * gamut: bounded
    * number: `[0,1]`

      > Percentages `[0%,100%]` map to the `[0,1]` range.

* `xyz`, `xyz-d50`, `xyz-d65`:
  * `x`, `y`, `z`:
    * gamut: un-bounded
    * number: `[0,1]`

      > Percentages `[0%,100%]` map to the `[0,1]` range.

* `lab`:
  * `lightness`:
    * gamut: un-bounded, clamped
    * number: `[0,100]`

      > Percentages `[0%,100%]` map to the `[0,100]` range.

  * `a`, `b`:
    * gamut: un-bounded
    * number: `[-125,125]`

      > Percentages `[-100%,100%]` map to the `[-125,125]` range.

* `lch`:
  * `lightness`:
    * gamut: un-bounded, clamped
    * number: `[0,100]`

      > Percentages `[0%,100%]` map to the `[0,100]` range.

  * `chroma`:
    * gamut: un-bounded
    * number: `[0,150]`

      > Percentages `[0%,100%]` map to the `[0,150]` range.

  * `hue`:
    * degrees: polar angle

* `oklab`:
  * `lightness`:
    * gamut: un-bounded, clamped
    * number: `[0,1]`

      > Percentages `[0%,100%]` map to the `[0,1]` range.

  * `a`, `b`:
    * gamut: un-bounded
    * number: `[-0.4,0.4]`

      > Percentages `[-100%,100%]` map to the `[-0.4,0.4]` range.

* `oklch`:
  * `lightness`:
    * gamut: un-bounded, clamped
    * number: `[0,1]`

      > Percentages `[0%,100%]` map to the `[0,1]` range.

  * `chroma`:
    * gamut: un-bounded
    * number: `[0,0.4]`

      > Percentages `[0%,100%]` map to the `[0,0.4]` range.

  * `hue`:
    * degrees: polar angle

### Predefined Color Spaces

> 'Predefined color spaces' can be described using the `color()` function.

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

In some cases, a color can have one or more missing components (channel or
alpha values). Missing components are represented by the special value `none`.
When interpolating between colors, the missing component is replaced by the
value of that same component in the other color. In all other cases, the
missing value is treated as `0`.

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

[interpolating]: #interpolating-colors

### Powerless Components

In some color spaces, it is possible for a channel value to become 'powerless'
in certain circumstances.

* `hsl`:

  * If the `saturation` value is `0%`, then the `hue` channel is powerless.

  * If the `lightness` value is either `0%` or `100%`, then both the `hue` and
    `saturation` values are powerless.

* `hwb`:

  * If the combined `whiteness` and `blackness` values (after normalization)
    are equal to `100%`, then the `hue` channel is powerless.

* `lab`/`oklab`:

  * If the `lightness` value is either `0%` or `100%`, then both the `a` and
  `b` channels are powerless.

* `lch`/`oklch`:

  * If the `chroma` value is 0%, then the `hue` channel is powerless.

  * If the `lightness` value is either `0%` or `100%`, then both the `hue` and
    `chroma` channels are powerless.

### Color Interpolation Method

A *color interpolation method* is a space-separated list of unquoted strings,
parsed according to the following syntax definition:

<x><pre>
**ColorInterpolationMethod** ::= RectangularColorSpace
&#32;                          | (PolarColorSpace HueInterpolationMethod?)
**HueInterpolationMethod**   ::= (
&#32;                                'shorter'
&#32;                              | 'longer'
&#32;                              | 'increasing'
&#32;                              | 'decreasing'
&#32;                            ) 'hue'
</pre></x>

A valid *PolarColorSpace* is the name of a [known color space] with a polar
angle hue channel. A *RectangularColorSpace* is the name of any other
[known color space], without a polar-angle hue. The *interpolation color space*
is the result of [looking up a known color space] named by either the
`PolarColorSpace` or `RectangularColorSpace` productions.

> Different color interpolation methods provide different advantages. For that
> reason, individual color procedures and functions can establish their own
> color interpolation defaults, or provide a syntax for authors to explicitly
> choose the method that best fits their need. The [CSS Color Level 4][color-4]
> specification provides [additional guidance][default-space] for determining
> appropriate defaults.

[default-space]: https://www.w3.org/TR/css-color-4/#interpolation-space
[color interpolation method]: #color-interpolation-method

## Serialization

### Serialization of Non-Legacy Colors

To serialize a non-legacy color `color`:

* Let `space-name` be an unquoted lowercase string of `color`'s space name.

* Let `known-space` be the result of [looking up a known color space] with a
  `name` of `space-name`.

* Let `components` be an empty space-separated list.

* For each `channel` in `color`'s channels:

  * If `channel` is missing a value, set `channel` to the unquoted string "none".

  * Otherwise:

    * Let `unit` be the unit associated with `channel` in `known-space`, if
      defined, and `null` otherwise.

    * If `unit` is not null, append `unit` units to the `channel` value.

  * Append `channel` as the last element of `components`.

* Let `alpha` be the alpha value of `color`.

* If `alpha != 1`:

  * Set `components` to the result of appending " / " and then the values of
    `alpha` to the end of `components`.

* If `color` has a [known color space] that is not a [predefined color space]:

  > Since a [predefined color space] is defined as a [known color space] that
  > uses the `color()` syntax, this is a reliable way to get the remaining
  > known color spaces that provide their own function syntax.

  * Emit `space-name` followed by "(", `components`, and then ")".

* Otherwise, emit "color(", followed by `space-name`, " ", `components`, and
  then ")".

[predefined color space]: #predefined-color-spaces

## Procedures

### Looking Up a Known Color Space

This procedure accepts a `name`, and attempts to look up a [known color space]
with a matching name. It throws an error if `name` is not a valid color space
name, and either returns the known color space, or `null` if no color space is
matched.

* If `name` is not an unquoted string, throw an error.

* Let `lower-name` be the result of calling `string.to-lower-case(name)`.

* If `lower-name` is the name of a [known color space], return the matching
  [known color space].

* Otherwise, throw an error.

  > In the future, we can add support for custom/unknown spaces by returning
  > `null` when no space is found.

[looking up a known color space]: #looking-up-a-known-color-space

### Converting a Color

Colors can be converted from one [known color space] to another. This procedure
accepts a color `origin-color`, and a [known color space] `target-space`, and
returns a color `color`.

> Since the individual CSS color conversion algorithms don't explicitly handle
> the process of 'carrying over' missing values on analogous channels, we have
> to handle that here.

* Let `origin-space` be `origin-color`'s color space.

* If `origin-space == target-space` return `origin-color`.

  > CSS doesn't perform conversions unless they are required.

* Let `missing` be a list of channel names in `origin-color` that are [missing].

* Let `color` be the result of [css-converting] `origin-color` into
  `target-space`.

* For each `channel` in `missing`:

  * If `target-space` has an [analogous component][missing] to `channel`, set
    the analogous component in `color` to `none`.

* If any `channel` of `color` is [powerless] and not already [missing], set
  `channel` to the special value `none`.

* Return `color`.

[missing]: #missing-components
[powerless]: #powerless-components

## CSS-Converting a Color Space

[css-converting]: #css-converting-a-color-space

Algorithms for individual color space conversion are defined in the
[CSS Color Level 4][color-4] specification. [CSS color conversion] takes a
color `origin-color`, and a [known color space] `target-space`, and returns a
color `output-color`.

[CSS color conversion]: https://www.w3.org/TR/css-color-4/#color-conversion

The individual conversion algorithms are:

* [HSL to sRGB](https://www.w3.org/TR/css-color-4/#hsl-to-rgb)

* [sRGB to HSL](https://www.w3.org/TR/css-color-4/#rgb-to-hsl)

* [HWB to sRGB](https://www.w3.org/TR/css-color-4/#hwb-to-rgb)

* [sRGB to HWB](https://www.w3.org/TR/css-color-4/#rgb-to-hwb)

* [Lab to LCH, OKLab to OKLCH](https://www.w3.org/TR/css-color-4/#lab-to-lch)

* [LCH to Lab, OKLCH to OKLab](https://www.w3.org/TR/css-color-4/#lch-to-lab)

* [Between predefined RGB spaces](https://www.w3.org/TR/css-color-4/#predefined-to-predefined)

* [Any RGB to Lab/OKLab](https://www.w3.org/TR/css-color-4/#predefined-to-lab-oklab)

* [Lab/OKLab to any RGB](https://www.w3.org/TR/css-color-4/#oklab-lab-to-predefined)

> For additional details, see the [Sample code for color conversions][convert].

[convert]: https://www.w3.org/TR/css-color-4/#color-conversion-code

### Gamut Mapping

> Some [known color space]s describe limited color gamuts. If a color is 'out of
> gamut' for a particular space (most often because of conversion from a
> larger-gamut color-space), it can be useful to 'map' that color to the nearest
> available 'in-gamut' color. Gamut mapping is the process of finding an
> in-gamut color with the least objectionable change in visual appearance.

Gamut mapping in Sass follows the [CSS gamut mapping algorithm][css-mapping].
This procedure accepts a color `origin`, and a [known color space]
`destination`. It returns the result of a [CSS gamut map][css-map] procedure,
converted back into the original color space.

* Let `origin-space` be `origin`'s color space.

* If either `origin-space` or `destination` is not a [known color space], throw
  an error.

* Let `mapped` be the result of [CSS gamut mapping][css-mapping] `origin`
  color, with an origin color space of `origin-space`, and destination of
  `destination`.

* Return the result of [converting] `mapped` into `origin-space`.

> This algorithm implements a relative colorimetric intent, and colors inside
> the destination gamut are unchanged. Since the process is lossy, authors
> should be encouraged to let the browser handle gamut mapping when possible.

[css-mapping]: https://www.w3.org/TR/css-color-4/#css-gamut-mapping-algorithm
[css-map]: https://www.w3.org/TR/css-color-4/#css-gamut-map

### Parsing Color Components

This procedure accepts an `input` parameter to parse, along with an optional
[known color space] `space`. It throws common parse errors when necessary, and
returns either a single string of components to emit in a CSS function, or
three values: a color space, a list of channel values, and an alpha value.

> This supports both the space-specific color formats like `hsl()` and `rgb()`,
> where the space is determined by the function, as well as the syntax of
> `color()`, where the space is included as one of the input arguments (and may
> be a user-defined space).

The procedure is:

* If `input` is a [special variable string], return an unquoted string with
    the value of `input`.

* If `input` is a bracketed list, or a list with a separator other than
    'slash' or 'space', throw an error.

* If `input` is a slash-separated list:

  * If `input` doesn't have exactly two elements, throw an error.

  * Otherwise, let `components` be the first element and `alpha` the second
      element of `input`.

* Otherwise:

  * Let `components` be an unbracketed space separated list of all except the
      last element of `input`.

  * If the last element of `input` is an unquoted string that contains `/`:

    * Let `split-last` be the result calling `string.split()` with the last
        element of `input` as the string to split, and `/` as the separator.

    * If `split-last` has two items, and one or both items are an unquoted
        string that's case-insensitively equal to 'none':

        > Special handling for `none/none`, `none/<number>`, and `<number>/none`.

      * If either item in `split-last` can be coerced to a number, replace
          the current value of the item with the resulting number value.

      * If any item in `split-last` is not a number or an unquoted string
          that's case-insensitively equal to 'none', return an unquoted string
          with the value of `input`.

      * Otherwise, let `alpha` be the second element in `split-last`, and
          append the first element of `split-last` to `components`.

    * Otherwise, return an unquoted string with the value of `input`.

      > This solves for a legacy handling of `/` in Sass that would produce an
      > unquoted string when the alpha value is a CSS function such as `var()`
      > or when either value is `none`.

  * Otherwise, if the last element of `input` has preserved its status as two
      slash-separated numbers:

    * Let `alpha` be the number after the slash, and append the number before
        the slash to `components`.

  * Otherwise, append the last element of `input` to `components`.

* If `components` is an empty list, throw an error.

* If `components` is a [special variable string]:

  * Let `channels` be the value of `components`.

* Otherwise:

  * If `components` is not an unbracketed space-separated list, throw an error.

  * If `space` is null:

    * Let `input-space` be the first element in `components`.

    * If `input-space` is a [special variable string], return an unquoted
        string with the value of `input`.

    * Set `space` be the result of [looking up a known color space] with the
        name `input-space`.

    * If `space` is not a [predefined color space], throw an error.

        > Only predefined spaces can be passed in as color syntax components.
        > All other known color spaces use explicit functions.

    * Let `channels` be an unbracketed space-separated list with the
        remaining elements from `components`.

  * Otherwise, let `channels` be the value of `components`.

  * Let `expected` be the number of channels in `space`.

  * If any element of `channels` is not either a number, a special variable
      string, a [special number], or an unquoted string that's
      case-insensitively equal to 'none', throw an error.

* If `alpha` is null, let `alpha` be `1`.

* Otherwise, If `alpha` is not a [special number]:

  * If `alpha` is a number, set `alpha` to the result of
      [percent-converting] `alpha` with a max of 1, and then clamping the value
      between 0 and 1, inclusive.

  * Otherwise, throw an error.

* If `channels` is a [special variable string], or if `alpha` is a [special
    number], return an unquoted string with the value of `input`.

* If any element of `channels` is a [special number]:

  * If `space` is a [legacy color] space:

    * Let `comma-list` be the result of calling
        `list.append(channels, alpha, 'comma')`.

    * Return an unquoted string with the value of `comma-list`.

  * Otherwise, return an unquoted string with the value of `input`.

    > Doing this late in the process allows us to throw any obvious syntax
    > errors, even for colors that can't be fully resolved during compilation.

* If the length of `channels` is not equal to `expected`, throw an error.

    > Once special values have been handled, any colors remaining should have
    > exactly the expected number of channels.

* Set `channels` to the result of [normalizing] `channels` in `space`.

* Let `space-name` be a lowercase unquoted string of the `space` name.

* Return `space-name`, `channels` channels, and `alpha` alpha value.

[special variable string]: ../spec/functions.md#special-variable-string
[special number]: ../spec/functions.md#special-number
[percent-converting]: #percent-converting-a-number

### Percent-Converting a Number

This algorithm takes a SassScript number `number` and a number `max`. It returns
a number relative to the range `[0,max]` without clamping.

> In order to support both out-of-gamut channels and unbounded ranges, this
> value is no longer clamped between 0 and `max`

* If `number` has units other than `%`, throw an error.

* If `number` has the unit `%`, set `number` to `number * max / 100%`.

* Return `number`.

### Validating a Color Channel

[validating]: #validating-a-color-channel

This process accepts a SassScript value `channel` to validate, a [known color
space] `space` to validate against, and the `key` name of the channel. It
throws an error if the channel is invalid for the color space, or returns a
normalized channel value otherwise.

* If `channel` is not a number or an unquoted string that's case-insensitively
  equal to 'none', throw an error.

* If `channel == NaN`, throw an error.

* If `channel` is an unquoted string that's case-insensitively equal to 'none',
  return `channel`.

* Otherwise:

  * Let `valid` be the corresponding channel defined by the [known color space]
    `space` with a name of `key`.

  * If `valid` is a polar-angle `hue`:

    * Let `angle` be the result of [converting][number-to-unit] `channel` to
      `deg` allowing unitless.

    * Return the result of `angle % 360deg`.

  * Otherwise, if `valid` requires a percentage:

    * If `channel` is a number with units other than `%`, throw an error.

    * Return `channel`.

  * Otherwise, set `channel` to the result of [percent-converting] `channel`
    with a `min` and `max` defined by the `valid` channel range.

  * If `valid` is a `lightness` channel, and `space` is not a [legacy color]
    space, set `channel` to the result of clamping the `channel` value between
    0 and 100, inclusive.

  * Return `channel`.

[number-to-unit]: https://github.com/sass/sass/blob/main/spec/types/number.md#converting-a-number-to-a-unit

### Normalizing Color Channels

[normalizing]: #normalizing-color-channels

This process accepts a list of `channels` to validate, and a [known color space]
`space` to normalize against. It throws an error if any channel is invalid for
the color space, or returns a normalized list of valid channels otherwise.

* If `channels` is not a list, throw an error.

* If `space` is not a [known color space], throw an error.

* Let `normal` be an empty list.

* For each `channel` in `channels`:

  * Let `key` be the name of `channel` in `space`.

  * Let `valid` be the result of [validating] `channel` as `key` channel in
    `space`.

  * Append `valid` as the next item in `normal`.

* Let `unitless` be an empty list.

* For each `channel` in `normal`.

  * If the value of `channel` is the special value `none`, append `none` as the
    next item in `unitless`.

  * Otherwise, append the value of `channel` as a [double] without units as the
    next item in `unitless`.

* Return `unitless`.

### Interpolating Legacy Colors

> This procedure is based on the legacy behavior of the `color.mix()` function,
> but returns a color in the original `color1` color space.

This procedure accepts two legacy colors (`color1` and `color2`), and an
optional percentage `weight` for `color1` in the mix. It returns a new color
`mix` that represents the appropriate mix of input colors.

* Let `origin-space` be `color1`'s color space.

* Let `rgb1` and `rgb2` be the result of [converting] `color1` and `color2`
  respectively into `rgb`.

* If `weight` is null, set `weight-scale` to `0.5`.

* Otherwise, set `weight-scale` to the result of [percent-converting] `weight`
  with a max of 1, and then clamping the value between 0 and 1 (inclusive).

* Let `normal-weight` be `weight-scale * 2 - 1`.

* Let `alpha1` and `alpha2` be the alpha values of `rgb1` and `rgb2`
  respectively.

* Let `alpha-distance` be `alpha1 - alpha2`.

* Let `weight-by-distance` be `normal-weight * alpha-distance`.

* If `weight-by-distance == -1`, let `combined-weight1` be `normal-weight`.

* Otherwise:

  * Let `weight-distance-sum` be `normal-weight + alpha-distance`.

  * Let `combined-weight1` be `weight-distance-sum / (1 + weight-by-distance)`.

* Let `weight1` be `(combined-weight1 + 1) / 2`.

* Let `weight2` be `1 - weight1`.

* Let `red1` and `red2` be the red channels of `rgb1` and `rgb2` respectively.

* Let `red` be `red1 * weight1 + red2 * weight2`.

* Let `green1` and `green2` be the green channels of `rgb1` and `rgb2`
  respectively.

* Let `green` be `green1 * weight1 + green2 * weight2`.

* Let `blue1` and `blue2` be the blue channels of `rgb1` and `rgb2`
  respectively.

* Let `blue` be `blue1 * weight1 + blue2 * weight2`.

* Let `alpha` be `alpha1 * weight-scale + alpha2 * (1 - weight-scale)`.

* Let `mix` be a [legacy color] in the `rgb` space, with the given `red`,
  `green`, and `blue` channels, and `alpha` value.

* Return the result of [converting] `mix` into `origin-space`.

[legacy interpolation]: #interpolating-legacy-colors

### Interpolating Colors

> This procedure is based on the
> [color interpolation](https://www.w3.org/TR/css-color-4/#interpolation)
> procedures defined in [CSS Color Level 4][color-4].

This procedure accepts two color arguments (`color1` and `color2`), a
[color interpolation method] `method`, and a percentage `weight` for `color1`
in the mix. It returns a new color `mix` that represents the appropriate mix of
input colors.

* If either `color1` or `color2` is not a color in a [known color space], throw
  an error.

* Let `origin-space` be `color1`'s color space.

* If `weight` is null, set `weight` to `0.5`.

* Otherwise, set `weight` to the result of [percent-converting] `weight` with a
  max of 1.

* If `weight > 1` or `weight < 0`, throw an error.

* If `weight == 0`, return `color2`.

* If `weight == 1`, return `color1`.

* Let `space` be the *interpolation color space* specified by the `method`
  [color interpolation method].

  > Only known color spaces are allowed as part of a color interpolation method.

* If `space` is a [PolarColorSpace][color-method]:

  * Let `hue-arc` be the `HueInterpolationMethod` specified in `method`, or
      `shorter` if no hue interpolation is specified.

* Set `color1` and `color2` respectively to the results of [converting] `color1`
  and `color2` into `space`.

* For each `color` in `color1` and `color2`:

  * If any non-`alpha` `component` of `color` is `none`, set that `component` to
    the value of the corresponding component in the other color.

    > If both values are `none`, the interpolation result for that component
    > will also be `none`.

  * Set `color` to the result of [premultiplying] `color`.

  * If `color`'s `alpha` component is `none`, set it to the value of the `alpha`
    component in the other color.

    > This is resolved after premultiplying, because premultiplying has special
    > handling for a missing `alpha` component.

* Let `mix` be a new color in the color space `space`, with `none` for all
  channel and alpha values.

* For each `channel` of `mix`:

  * Let `channel1` and `channel2` be the corresponding channel values in
    `color1` and `color2` respectively.

  * If `channel` has a polar angle value, set `channel1` and `channel2`
    respectively to the results of [hue interpolation][hue-method] with
    `channel1` as `hue1`, `channel2` as `hue2`, using the `hue-arc` method.

  * Set `channel` to the result of calculating
    `(channel1 * weight) + (channel2 * (1 - weight))`.

    > Channel rounding has been removed, since it is a lossy transform.

* Set `mix` the result of [un-premultiplying] `mix`.

* Return the result of [converting] `mix` into `origin-space`.

[premultiplying]: #premultiply-transparent-colors
[un-premultiplying]: #premultiply-transparent-colors
[color-method]: #color-interpolation-method
[hue-method]: #hue-interpolation
[converting]: #converting-a-color

#### Premultiply Transparent Colors

When the colors being interpolated are not fully opaque, they are transformed
into premultiplied color values. This process accepts a single `color` and
updates the channel values if necessary, returning a new color with
premultiplied channels.

* If the `color` has an `alpha` value of 1 or `none`, return `color` unchanged.

  > It's not possible to premultiply channels relative to a missing alpha,
  > and no multiplication is necessary with full opacity.

* Otherwise, for each `channel` in `color`:

  * If the `channel` value is `none`, or if `channel` is a polar-angle `hue`,
    keep the original value of `channel`.

  * Otherwise, set `channel` to the result of multiplying the `channel` value
    by the `alpha` value.

* Return the resulting `color` with premultiplied channels.

The same process can be run in reverse, to **un-premultiply** the channels of a
given `color`:

* If `color` has an `alpha` value of 1, 0, or `none`, return `color` unchanged.

* Otherwise, for each `channel` in `color`:

  * If the `channel` value is `none`, or if `channel` is a polar-angle `hue`,
    keep the original value of `channel`.

  * Otherwise, set `channel` to the result of dividing the premultiplied
    `channel` value by the `alpha` value.

* Return the resulting `color` with un-premultiplied channels.

#### Hue Interpolation

> When interpolating between polar-angle hue channels, there are multiple
> 'directions' the interpolation could move, following different logical rules.

This process accepts two hue angles (`hue1` and `hue2`), and returns both hues
adjusted according to the given `method`. When no hue interpolation `method` is
specified, the default is `shorter`.

The process for each [hue interpolation method][hue-interpolation] is defined
in [CSS Color Level 4][color-4].

* [shorter](https://www.w3.org/TR/css-color-4/#shorter)

* [longer](https://www.w3.org/TR/css-color-4/#hue-longer)

* [increasing](https://www.w3.org/TR/css-color-4/#increasing)

* [decreasing](https://www.w3.org/TR/css-color-4/#decreasing)

* [specified](https://www.w3.org/TR/css-color-4/#hue-specified)

[hue-interpolation]: https://www.w3.org/TR/css-color-4/#hue-interpolation

### Scaling a Number

This algorithm takes a number `number`, a value `factor`, a number `max`, and
an optional number `min`. It's written "scale `<number>` by `<factor>` with a
`max` of `<max>` and a `min` of `<min>`". It returns a number with a value
between `min` (or 0) and `max` and the same units as `number`.

> Note that this no longer assumes the original `number` is in a range of
> 0 to `max`. We now allow scaling up negative numbers, and scaling down
> numbers above the `max` value. The inverse operations return the `number`
> unchanged, since that's the asymptotic scale behavior approaching boundaries.

* If `factor` isn't a number with unit `%` between `-100%` and `100%`
  (inclusive), throw an error.

* If `min` is not specified, set `min` to 0.

* If `factor > 0%`:

  * If `number > max`, return `number`.

  * Otherwise, return `number + (max - number) * factor / 100%`.

* Otherwise:

  * If `number < 0`, return `number`.

  * Otherwise, return `number + (number - min) * factor / 100%`.

## New Color Module Functions

These new functions are part of the built-in `sass:color` module.

### `color.space()`

```
space($color)
```

* If `$color` is not a color, throw an error.

* Return an unquoted string with the name of `$color`s color space.

### `color.to-space()`

```
to-space($color, $space)
```

* If `$color` is not a color, throw an error.

* Let `known-space` be the result of [looking up a known color space] named
  `$space`.

* Let `known-origin` be `$color`'s space.

* If `known-origin == known-space`, return `$color`.

* Let `converted` be the result of [converting] the `origin-color` `$color` to
  the `target-space` `known-space`.

* If `converted` is a [legacy color]:

  * For each `component` in the channels and alpha value of `converted`, if
    `component` is [missing], set `component` to `0`.

* Return `converted`.

### `color.is-legacy()`

```
is-legacy($color)
```

* If `$color` is not a color, throw an error.

* Return `true` if `$color` is a [legacy color], or `false` otherwise.

### `color.is-powerless()`

```
is-powerless($color, $channel, $space: null)
```

* If `$color` is not a color, throw an error.

* If `$channel` is not a quoted string, throw an error.

* If `$space` is null:

  * Let `color` be `$color`

  * Let `origin-space` be the result of calling `color.space($color)`.

  * Let `space` be the result of [looking up a known color space] named
    `origin-space`.

* Otherwise:

  * Let `color` be the result of calling `color.to-space($color, $space)`.

  * Let `space` be the result of [looking up a known color space] named
    `$space`.

* Let `channels` be a list of the `color`'s channels.

* If `$channel` is not the name of a channel in `channels`, throw an error.

* Return `true` if the channel `$channel` is [powerless] in `color`,
  otherwise return `false`.

### `color.is-in-gamut()`

```
is-in-gamut($color, $space: null)
```

* If `$color` is not a color, throw an error.

* Let `space-name` be the result of calling `color.space($color)` if `$space`
  is null, and the value of `$space` otherwise.

* Let `space` be the result of [looking up a known color space] named
  `space-name`.

* Let `color` be the result of calling `color.to-space($color, space)`.

* For all bounded channels in `space`, if the associated channel value in
  `$color` is fuzzy greater-than the bounded maximum, or fuzzy less-than the
  bounded minimum, return `false`.

* Otherwise, return `true`.

### `color.to-gamut()`

```
to-gamut($color, $space: null)
```

* If `$color` is not a color, throw an error.

* If `$space` is null:

  * Let `origin-space` be the result of calling `color.space($color)`.

  * Let `target-space` be the result of [looking up a known color space] named
    `origin-space`.

* Otherwise, let `target-space` be the result of [looking up a known color
  space] named `$space`.

* Return the result of [gamut mapping] `$color` with a `target-space`
  destination.

[gamut mapping]: #gamut-mapping

### `color.channel()`

> Note that channel values are stored as specified, even if those values are
> out-of-gamut for the [known color space] used. Similarly, this color-channel
> inspection function may return out-of-gamut channel values.

```
channel($color, $channel, $space: null)
```

* If `$color` is not a color, throw an error.

* If `$channel` is not a quoted string, throw an error.

* If `$channel == 'alpha'` (ignoring case), let `value` be the alpha value of
  `$color`.

* Otherwise:

  * Let `color` be `$color` if `$space` is null, and the result of calling
    `color.to-space($color, $space)` otherwise.

  * If `channel` is not the name of a channel in `color`, throw an error.

  * Let `value` be the channel value in `color` with name of `channel`.

  * Let `unit` be the unit associated with `channel` in `color`'s space, if
    defined, and `null` otherwise.

* If `value` is `null`, return `0`.

* If `unit` is not null, return the result of appending `unit` units to `value`.

* Return `value`.

### `color.is-missing()`

```
is-missing($color, $channel)
```

* If `$color` is not a color, throw an error.

* If `$channel` is not an unquoted string, throw an error.

* If `$channel == alpha` (ignoring case), let `value` be the alpha value of
  `$color`.

* Otherwise:

  * If `channel` is not the name of a channel in `$color`, throw an error.

  * Let `value` be the channel value in `color` with name of `channel`.

* Return `true` if `value == null`, and `false` otherwise.

### `color.same()`

> While it's already possible to compare the [equality](#color-equality) of
> two colors, the result is always false when the two colors are in different
> color spaces. This function compares colors across color spaces, to determine
> if they are equal after being converted into the same space.

```
same($color1, $color2)
```

* If either `$color1` or `$color2` is not a color in a [known color space]:

  * Let `color1` be `$color1`, and let `color2` be `$color2`.

  > We can compare, but we can't do conversion. The color space remains
  > relevant to equality. While this is technically the same as using `==`,
  > it makes the function more robust to allow comparison of all colors.

* Otherwise:

  * Let `color1` and `color2` be the result of [converting] `$color1` and
    `$color2` into `xyz` color space, respectively.

* Return `color1 == color2`.

## Modified Color Module Functions

### `color.hwb()`

These functions are now deprecated. Authors should use global `hwb()` instead.

* ```
  hwb($channels)
  ```

  * Return the result of calling the global function `hwb($channels)`.

* ```
  hwb($hue, $whiteness, $blackness, $alpha: 1)
  ```

  * Return the result of calling the global function
    `hwb(list.slash($hue $whiteness $blackness, $alpha))`.

### `color.mix()`

```
mix($color1, $color2,
  $weight: 50%,
  $method: null)
```

* If either `$color1` or `$color2` is not a color, throw an error.

* If `$method` is null:

  * If either `$color1` or `$color2` is not a [legacy color], throw an error.

    > Method is required for non-legacy colors. This matches the `color-mix()`
    > function defined in [Colors Level 5][color-5], and allows us to add
    > additional default behavior in the future.

  * Return the result of [legacy interpolation] between `$color1` and `$color2`
    with the specified `$weight`.

* Otherwise, if `$method` is not a [color interpolation method], throw an error.

* Return the result of [interpolating] between `$color1` and
  `$color2` with the specified `$weight` and `$method`.

### `color.change()`

```
change($color, $args...)
```

This function is also available as a global function named `change-color()`.

* If `$color` is not a color, throw an error.

* If any item in `$args` is not a keyword argument, throw an error.

* Let `color` be the value of `$color`.

* Let `origin-space` be `color`'s space.

* If the keyword argument `$space` is specified in `$args`:

  * Let `known-space` be the result [looking up a known color space] named
      `$space`.

  * If `space != origin-space`, set `color` to the result of calling
      `color.to-space(color, space)`.

* Otherwise, let `known-space` be `origin-space`.

* Let `alpha` be `color`'s alpha property.

* If the keyword argument `$alpha` is specified in `$args`:

  * Set `alpha` to the result of [percent-converting] `$alpha`, and clamping
      it between 0 and 1 (inclusive).

* Let `channel-args` be the remaining keyword arguments in `$args`, not
  including `$space` or `$alpha` arguments.

* Let `channels` be a list of the `color`'s channels.

* For each keyword `key` and value `new` in `channel-args`:

  * If `new` is not a number or an unquoted string that's case-insensitively
    equal to 'none', throw an error.

    > This basic restriction can be applied to all spaces. Further channel
    > restrictions are enforced by the normalization step for known spaces.

  * If `key` is not the name of a channel in `channels`:

    * If `$space` is specified, throw an error.

    * If `color` is not a [legacy color], throw an error.

    * If `key` is one of `red`, `green`, or `blue`:

      * Let `legacy-color` be the result of [converting] `color` to `rgb`.

    * Otherwise, if `key` is one of `hue`, `saturation`, or `lightness`:

      * Let `legacy-color` be the result of [converting] `color` to `hsl`.

    * Otherwise, if `key` is one of `whiteness`, or `blackness`:

      * Let `legacy-color` be the result of [converting] `color` to `hwb`.

    * Otherwise, throw an error.

    * Set `channels` to be a list of `legacy-color`'s channels.

  * Set the corresponding `key` value in `channels` to `new`.

* Set `channels` to the result of [normalizing] `channels` in `known-space`.

* Let `new` be a color in color space `known-space`, with `channels` channels,
  and an alpha of `alpha`.

* Return the result of [converting] `new` into `origin-space`.

### `color.adjust()`

```
adjust($color, $args...)
```

This function is also available as a global function named `adjust-color()`.

* If `$color` is not a color, throw an error.

* If any item in `$args` is not a keyword argument, throw an error.

* Let `color` be the value of `$color`.

* Let `origin-space` be `color`'s space.

* If the keyword argument `$space` is specified in `$args`:

  * Let `known-space` be the result [looking up a known color space] named
      `$space`.

  * If `space != origin-space`, set `color` to the result of calling
      `color.to-space(color, space)`.

* Otherwise, let `known-space` be `origin-space`.

* Let `alpha` be `color`'s alpha property.

* If the keyword argument `$alpha` is specified in `$args`:

  * If `alpha == none`, throw an error.

      > This is not the ideal solution for handling `none`, but we want to
      > match CSS relative color syntax if possible. Throwing an error for now
      > means we can adjust to match the CSS behavior once it is defined.

  * Let `new-alpha` be the result of [percent-converting] `$alpha` with a max
    of 1.

  * Set `alpha` to the value of `new-alpha + alpha` clamped between 0 and 1.

* Let `channel-args` be the remaining keyword arguments in `$args`, not
  including `$space` or `$alpha` arguments.

* Let `channels` be a list of the `color`'s channels.

* For each keyword `key` and value `adjust` in `channel-args`:

  * If `key` is not the name of a channel in `channels`:

    * If `$space` is specified, throw an error.

    * If `color` is not a [legacy color], throw an error.

    * If `key` is one of `red`, `green`, or `blue`:

      * Let `legacy-color` be the result of [converting] `color` to `rgb`.

    * Otherwise, if `key` is one of `hue`, `saturation`, or `lightness`:

      * Let `legacy-color` be the result of [converting] `color` to `hsl`.

    * Otherwise, if `key` is one of `whiteness`, or `blackness`:

      * Let `legacy-color` be the result of [converting] `color` to `hwb`.

    * Otherwise, throw an error.

    * Set `channels` to be a list of `legacy-color`'s channels.

  * Let `channel` be the value of the channel in `channels` with name of `key`.

  * Let `valid` be the channel in by `known-space` with a name of `key`.

  * If `channel == none`, throw an error.

    > This is not the ideal solution for handling `none`, but we want to
    > match CSS relative color syntax if possible. Throwing an error for now
    > means we can adjust to match the CSS behavior once it is defined.

  * If `adjust` has the unit `%`:

    * If `valid` requires a percentage, set `channel` to the result of appending
      `%` units to `channel`.

    * Otherwise, if `valid` allows percentage mapping, set `adjust` to the
      result of [percent-converting] `adjust` with a `min` and `max` defined
      by the `valid` channel range.

    * Otherwise, throw an error.

  * Set `channel` to `channel + adjust`.

    > Once percentage/number conversions have been normalized, this will throw
    > an error if `adjust` and `channel` are not compatible.

* Set `channels` to the result of [normalizing] `channels` in `known-space`.

* Let `new` be a color in color space `known-space`, with `channels` channels,
  and an alpha of `alpha`.

* Return the result of [converting] `new` into `origin-space`.

### `color.scale()`

```
scale($color, $args...)
```

This function is also available as a global function named `scale-color()`.

* If `$color` is not a color, throw an error.

* If any item in `$args` is not a keyword argument, throw an error.

* Let `origin-space` be `$color`'s color space.

* If the keyword argument `$space` is specified in `$args`:

  * Let `space` be the result of [looking up a known color space] named
      `$space`.

  * Let `color` be the result of [converting] `$color` to `space`.

* Otherwise:

  * Let `space` be `origin-space`.

  * Let `color` be the value of `$color`.

* Let `alpha` be `color`'s alpha property.

* If the keyword argument `$alpha` is specified in `$args`:

  * If `alpha == none`, throw an error.

      > This is not the ideal solution for handling `none`, but we want to
      > match CSS relative color syntax if possible. Throwing an error for now
      > means we can adjust to match the CSS behavior once it is defined.

  * Set `alpha` to the result of [scaling] `alpha` by `$alpha` with `max` 1.

* Let `channel-args` be the remaining keyword arguments in `$args`, not
  including `$space` or `$alpha` arguments.

* Let `channels` be a list of the `color`'s channels.

* For each keyword `scale`, `factor` in `channel-args`:

  * If `scale` is not the name of a [scalable] channel in `channels`:

    * If `$space` is specified, throw an error.

    * If `color` is not a [legacy color], throw an error.

    * If `scale` is one of `red`, `green`, or `blue`:

      * Let `legacy-color` be the result of [converting] `color` to `rgb`.

    * Otherwise, if `scale` is one of `saturation`, or `lightness`:

      * Let `legacy-color` be the result of [converting] `color` to `hsl`.

    * Otherwise, if `scale` is one of `whiteness`, or `blackness`:

      * Let `legacy-color` be the result of [converting] `color` to `hwb`.

    * Otherwise, throw an error.

    * Set `channels` to be a list of `legacy-color`'s channels.

  * Let `channel` be the corresponding `channel` in `channels` with a name
    matching `scale`.

  * If `channel == none`, throw an error.

    > This is not the ideal solution for handling `none`, but we want to
    > match CSS relative color syntax if possible. Throwing an error for now
    > means we can adjust to match the CSS behavior once it is defined.

  * Let `channel-max` be the upper boundary of `channel` in `space`.

  * Let `channel-min` be the lower boundary of `channel` in `space`.

  * Set the corresponding `channel` in `channels` to the result of [scaling]
    `channel` by `factor` with a `max` of `channel-max` and a `min` of
    `channel-min`.

* Set `channels` be the result of [normalizing] `channels` in `space`.

* Let `new` be a color in color space `space`, with `channels` channels, and an
  alpha of `alpha`.

* Return the result of [converting] `new` into `origin-space`.

[scalable]: #known-color-space
[scaling]: #scaling-a-number

### `color.complement()`

```
complement($color, $space: null)
```

This function is also available as a global function named `complement()`.

* If `$color` is not a color, throw an error.

* If `$space` is null:

  * If `$color` is a legacy color, let `space` be the [known color space]
    named `hsl`.

  * Otherwise, throw an error.

* Otherwise:

  * Let `space` be the result of [looking up a known color space] named
    `$space`.

  * If `space` is not a [known color space] with a polar-angle hue channel,
    throw an error.

    > This currently allows `hsl`, `hwb`, `lch`, and `oklch`. We may decide to
    > provide additional options in the future.

* Return the result of calling
  `color.adjust($color, $hue: 180deg, $space: space)`.

### `color.invert()`

```
invert($color,
  $weight: 100%,
  $space: null)
```

This function is also available as a global function named `invert()`.

* If `$color` is not a color, throw an error.

* If `$space` is null:

  * If `$color` is a legacy color, let `space` be `rgb`, and let `mix-space`
    be null.

    > This allows us to also enforce legacy behavior in the final weighted mix.

  * Otherwise, throw an error.

* Otherwise:

  * Let `space` be the result of [looking up a known color space] named
    `$space`.

  * If `space` is not a [known color space], throw an error.

  * Let `mix-space` be `space`.

* If `$weight == 0%`, return the value of `$color`.

* If `space` is not a valid [color interpolation method] *interpolation color
  space*, and `$weight != 100%`, throw an error.

* Let `color` be the result of [converting] `$color` into `space`.

* If `space` is the [known color space] named `hwb`:

  * Let `hue`, `white`, and `black` be the three elements of `color`'s channels.

  * Let `hue-out` be the result of `(hue + 180deg) % 360deg`.

  * Let `invert` be the result of calling
    `color.change(color, $hue: hue-out, $white: black, $black: white)`.

* Otherwise:

  * Let `invert` be the value of `color`.

  * For each `channel` element in `color`'s channels:

    * If `channel` is a polar-angle `hue`:

      * Let `new` be `(channel + 180deg) % 360deg`.

    * Otherwise, if `channel`'s name is either `chroma` or `saturation`:

      * Let `new` be `channel`.

    * Otherwise:

      * Let `min` and `max` be the minimum and maximum values defined for
        `channel` in `space`.

      * Let `new` be `max - channel` if `min == 0`, and `channel * -1` otherwise.

    * Set the corresponding channel of `invert` to be `new`.

* If `$weight == 100%`, return the value of `invert`.

* Return the result of calling `color.mix(invert, color, $weight, mix-space)`.

### `color.grayscale()`

```
grayscale($color)
```

> No space argument is provided, since the results should always be in gamut.

This function is also available as a global function named `grayscale()`.

* If `$color` is not a color, throw an error.

* If `$color` is a legacy color:

  * Return the result of [converting] `$color` to `hsl`, and changing the
    'saturation' channel to 0.

* Otherwise:

  * Let `origin` be `$color`'s color space.

  * Let `color` be the result of [converting] `$color` to `oklch`, and
    setting the `chroma` channel to 0.

  * Return the result of [converting] `color` to `origin`.

### `color.ie-hex-str()`

This function is also available as a global function named `ie-hex-str()`. Both
functions are deprecated.

```
ie-hex-str($color)
```

* If `$color` is not a color, throw an error.

* Let `rgb` be the result of [converting] and [gamut mapping] `$color` to `rgb`.

* Let `hex-list` be an empty list.

* For each `channel` in `rgba`'s channels, as numbers:

  * Let `hex-channel` be the hexadecimal representation of `channel`'s value.

  * Append `hex-channel` as the next item in `hex-list`.

* Let `alpha` be `rgb`'s alpha value.

* Let `hex-alpha` be the hexadecimal representation of `alpha * 255`.

* Append `hex-alpha` as the next item in `hex-list`.

* Return the result of concatenating `hex-list` into a string.

## New Global Functions

These new CSS functions are provided globally.

### `hwb()`

* ```
  hwb($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `hwb` space.

    > Normalization and clamping is handled as part of the [parsing] process.

  * If `parsed` is a string, return a plain CSS function string with the name
    `"hwb"` and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `hue`, `whiteness`, and `blackness` be the three elements of `channels`.

  * Return a [legacy color] in the `hwb` space, with the given `hue`,
    `whiteness`, and `blackness` channels, and `alpha` value.

[parsing]: #parsing-color-components

### `lab()`

* ```
  lab($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `lab` space.

  * If `parsed` is a string, return a plain CSS function string with the name
    `"lab"` and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `lightness`, `a`, and `b` be the three elements of `channels`.

  * Return a color in the `lab` [known color space], with the given `lightness`,
    `a`, and `b` channels, and `alpha` value.

### `lch()`

* ```
  lch($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `lch` space.

  * If `parsed` is a string, return a plain CSS function string with the name
    `"lch"` and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `lightness`, `chroma`, and `hue` be the three elements of `channels`.

  * Return a color in the `lch` [known color space], with the given `lightness`,
    `chroma`, and `hue` channels, and `alpha` value.

### `oklab()`

* ```
  oklab($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `oklab` space.

  * If `parsed` is a string, return a plain CSS function string with the name
    `"oklab"` and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `lightness`, `a`, and `b` be the three elements of `channels`.

  * Return a color in the `oklab` [known color space], with the given `lightness`,
    `a`, and `b` channels, and `alpha` value.

### `oklch()`

* ```
  oklch($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `oklch` space.

  * If `parsed` is a string, return a plain CSS function string with the name
    `"oklch"` and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `lightness`, `chroma`, and `hue` be the three elements of `channels`.

  * Return a color in the `oklch` [known color space], with the given `lightness`,
    `chroma`, and `hue` channels, and `alpha` value.

### `color()`

* ```
  color($description)
  ```

  * Let `parsed` be the result of [parsing] `$description` without a space.

  * If `parsed` is a string, return a plain CSS function string with the name
    `"color"` and the argument `parsed`.

  * Let `space` be the color space, `channels` the channel list, and `alpha`
    the alpha value of `parsed`.

  * Return a color in `space`, with the given `channels` and `alpha` value.

## Modified Global Functions

Any legacy global functions that are not explicitly updated here should continue
to behave as alias functions for their appropriately updated counterparts.

> Note that the new logic preserves decimal values in color channels, as well
> as preserving the initial color-space used in defining a color.

### `rgb()` and `rgba()`

The `rgba()` function is identical to `rgb()`, except that if it would return a
plain CSS function named `"rgb"` that function is named `"rgba"` instead.

* ```
  rgb($red, $green, $blue, $alpha: 1)
  ```

  * If any argument is an unquoted string that's case-insensitively equal to
    'none', throw an error.

    > Missing channels are not allowed in legacy syntax.

  * If any argument is a [special number], return a plain CSS function
    string with the name `"rgb"` and the arguments `$red`, `$green`, `$blue`,
    and `$alpha`.

  * If `$alpha` is not a number, throw an error.

  * Let `alpha` be the result of [percent-converting] `alpha` with a max of 1,
    and then clamping the value between 0 and 1, inclusive.

  * Let `red`, `green`, and `blue` be the three elements returned by
    [normalizing] `($red, $green, $blue)` in the [known color space] named `rgb`.

  * Return a [legacy color] in the `rgb` space, with the given `red`,
    `green`, and `blue` channels, and `alpha` value.

* ```
  rgb($red, $green, $blue)
  ```

  * If any argument is a [special number], return a plain CSS function string
    with the name `"rgb"` and the arguments `$red`, `$green`, and `$blue`.

  * Otherwise, return the result of calling `rgb($red, $green, $blue, 1)`.

* ```
  rgb($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `rgb` space.

  * If `parsed` is a string, return a plain CSS function string with the name
    `"rgb"` and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `red`, `green`, and `blue` be the three elements of `channels`.

  * Return the result of calling `rgb(red, green, blue, alpha)`.

* ```
  rgb($color, $alpha)
  ```

  * If either argument is a [special variable string], return a plain CSS
    function string with the name `"rgb"` and the same arguments.

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `rgb()` with `$color`'s red, green, and blue
    channels as unitless number arguments, and `$alpha` as the final argument.

### `hsl()` and `hsla()`

The `hsla()` function is identical to `hsl()`, except that if it would return a
plain CSS function named `"hsl"` that function is named `"hsla"` instead.

* ```
  hsl($hue, $saturation, $lightness, $alpha: 1)
  ```

  * If any argument is an unquoted string that's case-insensitively equal to
    'none', throw an error.

    > Missing channels are not allowed in legacy syntax.

  * If any argument is a [special number], return a plain CSS function
    string with the name `"hsl"` and the arguments `$hue`, `$saturation`,
    `$lightness`, and `$alpha`.

  * If `$alpha` is not a number, throw an error.

  * Let `alpha` be the result of [percent-converting] `alpha` with a max of 1,
    and then clamping the value between 0 and 1, inclusive.

  * Let `hue`, `saturation`, and `lightness` be the three elements returned
    by [normalizing] `($hue, $saturation, $lightness)` in the
    [known color space] named `hsl`.

  > Conversion to rgb has been removed.

  * Return a [legacy color] in the `hsl` space, with the given `hue`,
    `saturation`, and `lightness` channels, and `alpha` value.

* ```
  hsl($hue, $saturation, $lightness)
  ```

  * If any argument is a [special number], return a plain CSS function string
    with the name `"hsl"` and the arguments `$hue`, `$saturation`, and
    `$lightness`.

  * Otherwise, return the result of calling
    `hsl($hue, $saturation, $lightness, 1)`.

* ```
  hsl($hue, $saturation)
  ```

  * If either argument is a [special variable string], return a plain CSS
    function string with the name `"hsl"` and the same arguments.

  * Otherwise, throw an error.

* ```
  hsl($channels)
  ```

  * Let `parsed` be the result of [parsing] `$channels` in `hsl` space.

    > Normalization and clamping is handled as part of the [parsing] process.

  * If `parsed` is a string, return a plain CSS function string with the name
    `"hsl"` and the argument `parsed`.

  * Let `channels` be the channel list, and `alpha` the alpha value of `parsed`.

  * Let `hue`, `saturation`, and `lightness` be the three elements of `channels`.

  * Return a [legacy color] in the `hsl` space, with the given `hue`,
    `saturation`, and `lightness` channels, and `alpha` value.

## Deprecated Functions

Individual color-channel functions defined globally or in the color module are
deprecated in favor of the new `color.channel()` function. Legacy global color
functions are also deprecated. These functions always throw errors. During
the deprecation process, they act as alias functions described below.

### `color.red()`, `red()`

* ```
  color.red($color)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `color.channel($color, 'red', rgb)`.

This function is also available as a global function named `red()`.

### `color.green()`, `green()`

* ```
  color.green($color)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `color.channel($color, 'green', rgb)`.

This function is also available as a global function named `green()`.

### `color.blue()`, `blue()`

* ```
  color.blue($color)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `color.channel($color, 'blue', rgb)`.

This function is also available as a global function named `blue()`.

### `color.hue()`, `hue()`

* ```
  color.hue($color)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `color.channel($color, 'hue', hsl)`.

This function is also available as a global function named `hue()`.

### `color.saturation()`, `saturation()`

* ```
  color.saturation($color)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `color.channel($color, 'saturation', hsl)`.

This function is also available as a global function named `saturation()`.

### `color.lightness()`, `lightness()`

* ```
  color.lightness($color)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `color.channel($color, 'lightness', hsl)`.

This function is also available as a global function named `lightness()`.

### `color.whiteness()`

* ```
  color.whiteness($color)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `color.channel($color, 'whiteness', hwb)`.

### `color.blackness()`

* ```
  color.blackness($color)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `color.channel($color, 'blackness', hwb)`.

### `color.alpha()`

* ```
  color.alpha($color)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `color.channel($color, 'alpha')`.

### `adjust-hue()`

* ```
  adjust-hue($color, $amount)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling
    `color.adjust($color, $hue: $amount, $space: hsl)`.

### `saturate()`

* ```
  saturate($color, $amount)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling
    `color.adjust($color, $saturation: $amount, $space: hsl)`.

### `desaturate()`

* ```
  desaturate($color, $amount)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling
    `color.adjust($color, $saturation: -$amount, $space: hsl)`.

### `transparentize()`, `fade-out()`

* ```
  transparentize($color, $amount)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `color.adjust($color, $alpha: -$amount)`.

This function is also available as a global function named `fade-out()`.

### `opacify()`, `fade-in()`

* ```
  opacify($color, $amount)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling `color.adjust($color, $alpha: $amount)`.

This function is also available as a global function named `fade-in()`.

### `lighten()`

* ```
  lighten($color, $amount)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling
    `color.adjust($color, $lightness: $amount, $space: hsl)`.

### `darken()`

* ```
  darken($color, $amount)
  ```

  * If `$color` is not a [legacy color], throw an error.

  * Return the result of calling
    `color.adjust($color, $lightness: -$amount, $space: hsl)`.
