# CSS Color Level 4, New Color Spaces: Draft 1

*([Issue](https://github.com/sass/sass/issues/2831))*

This proposal adds Sass support for several new CSS color spaces defined in
[CSS Color Level 4][color-4], including access to non-RGB color models and
colors outside the sRGB gamut.

[color-4]: https://www.w3.org/TR/css-color-4/

## Table of Contents

* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Definitions](#definitions)
  * [Color](#color)
  * [Legacy Color](#legacy-color)
  * [Color Space](#color-space)
  * [Predefined Color Spaces](#predefined-color-spaces)
  * [Missing Components](#missing-components)
  * [Powerless Components](#powerless-components)
  * [Color Gamuts](#color-gamuts)
  * [Color Interpolation Method](#color-interpolation-method)
* [Procedures](#procedures)
  * [Converting a Color](#converting-a-color)
  * [Gamut Mapping](#gamut-mapping)
  * [Parsing Color Components](#parsing-color-components)
  * [Normalizing Color Channels](#normalizing-color-channels)
  * [Interpolating Colors](#interpolating-colors)
    * [Premultiply Transparent Colors](#premultiply-transparent-colors)
    * [Hue Interpolation](#hue-interpolation)
* [Deprecated Functions](#deprecated-functions)
* [New Color Module Functions](#new-color-module-functions)
  * [`color.space()`](#colorspace)
  * [`color.to-space()`](#colorto-space)
  * [`color.is-legacy()`](#coloris-legacy)
  * [`color.is-powerless()`](#coloris-powerless)
  * [`color.is-in-gamut()`](#coloris-in-gamut)
  * [`color.to-gamut()`](#colorto-gamut)
  * [`color.channel()`](#colorchannel)
* [Modified Color Module Functions](#modified-color-module-functions)
  * [`color.hwb()`](#colorhwb)
  * [`color.mix()`](#colormix)
  * [`color.alpha()`](#coloralpha)
  * [`color.change()`](#colorchange)
  * [`color.adjust()`](#coloradjust)
  * [`color.scale()`](#colorscale)
  * [`color.complement()`](#colorcomplement)
  * [`color.invert()`](#colorinvert)
  * [`color.grayscale()`](#colorgrayscale)
  * [`color.ie-hex-str()`](#colorie-hex-str)
* [New Global Functions](#new-global-functions)
  * [`hwb()`](#hwb)
  * [`lab()`](#lab)
  * [`lch()`](#lch)
  * [`oklab()`](#oklab)
  * [`oklch()`](#oklch)
  * [`color()`](#color)
* [Modified Global Functions](#modified-global-functions)
  * [`rgb()` and `rgba()`](#rgb-and-rgba)
  * [`hsl()` and `hsla()`](#hsl-and-hsla)

## Background

> This section is non-normative.

When working with color on the web, there are a few important terms:

- A *color model* is a mathematical approach to representing colors and their
  relationships. Historically, RGB has been the dominant color model for both
  computer monitors and web browsers. Lately, CIELab and OKLab models have
  shown significant benefits by providing a more *perceptually uniform*
  distribution of colors, so that similar mathematical adjustments achieve
  visually similar results.
- A *color space* is the result of projecting a color model into a coordinate
  system. In CSS, each color format describes a specific (and often unique)
  color space. For example, `rgb()` projects the RGB color model into a cubic
  coordinate system, while `hsl()` projects the same model into a cylindrical
  (polar-angle) space. Different spaces will have different benefits when
  adjusting or interpolating colors for different purposes.
- A *color gamut* is the full range of colors that can be described in a color
  space. Historically, all CSS spaces have been limited to the same sRGB gamut.
  However, modern computer monitors often support wider gamuts like display-p3.

Historically, CSS has only provided authors with color formats using the RGB
model, limited to the sRGB gamut. As CSS is used for more applications (such as
print) and displays continue to improve, those limitations become more clear.
The [CSS Color Level 4][color-4] specification defines a number of new color
spaces, each with its own syntax, representing both new color models and
wider RGB gamuts.

Since all CSS colors up until this point have been restricted to RGB math in
the sRGB gamut, Sass has treated all color formats as interchangeable. That
has allowed authors to inspect and manipulate colors in any space, without
careful management or gamut mapping. It has also allowed Sass to output the
most browser-compatible CSS format for any given color.

In order to support the color spaces in CSS Sass will need to start tracking
the space and gamut associated with any given color, and provide author tools
for managing those color spaces. In addition to supporting the new color space
functions, we plan to update all functions in the color module, and provide
some additional space and gamut management and inspection functions.

## Summary

> This section is non-normative.

This proposal defines Sassified versions of all the color functions in
[CSS Color Level 4][color-4]. Since the CIE color space defines the entire
gamut of visible color, much larger than the target sRGB gamut, out-of-range
color definitions will be clipped using a *relative-colorimetric* approach that
leaves in-gamut colors un-affected.

The `oklab()` (cubic) and `oklch()` (cylindrical) functions provide access to
an unbounded gamut of colors in perceptually uniform space. Authors can use
these functions to define reliably uniform colors. For example, the following
colors are perceptually similar in both luminosity and saturation. The `oklch()`
format reflects that with consistent 'lightness' and 'chroma' values, while the
`hsl()` format shows dramatic changes in both 'lightness' and 'saturation':

```scss
$pink: oklch(64% 0.196 353); // hsl(329.8 70.29% 58.75%)
$blue: oklch(64% 0.196 253); // hsl(207.4 99.22% 50.69%)
```

The new `color()` function provides access to a number of specialty spaces.
Most notably, `display-p3` is a common space for wide-gamut monitors, making
it likely one of the more popular options for authors who simply want access to
a wider range of colors. For example, P3 greens are significantly 'brighter'
and more saturated than the greens available in sRGB:

```scss
$fallback-green: rgb(0% 100% 0%);
$brighter-green: color(display-p3 0 1 0);
```

<!-- flesh this out more -->

### Design Decisions

Most of the design decisions involved in the proposal are based on the
[CSS Color Level 4][color-4] specification, which we have tried to emulate as
closely as possible, while maintaining support for legacy projects. In some
cases, that required major changes to the way Sass handles colors:

1. Channel values are no longer clamped to the gamut of a given color space.
   Authors can use the provided `to-gamut()` function to map colors into gamut,
   or allow out-of-gamut colors in output. Browsers will also provide improved
   gamut mapping, based on the user device capabilities.
2. Channel values are no longer rounded to the nearest integer, since the spec
   now requires maintaining precision wherever possible. This is especially
   important in RGB spaces, where color distribution is inconsistent.

While we are not attempting to support all of [CSS Color Level 5][color-5] at
this point, we have used it as a reference while updating color manipulation
functions such as `color.mix()`.

[color-5]: https://www.w3.org/TR/css-color-5/

<!-- flesh this out more -->

## Definitions

### Color

> Note that channel values are stored as specified, maintaining precision where
> possible, even when the values are out-of-gamut for the given [color space][].

A *color* is an object with several parts:

* A string [*color space*](#color-space)

* An ordered list of *channel* values as defined by that [color space][].

* A numeric *alpha* value which can be safely clamped in the `0-1` or `0%-100%`
  range. Values outside that range are allowed, but meaningless.

* A boolean *is-legacy* to indicate a [legacy color][].

[legacy color]: #legacy-color
[color space]: #color-space

### Legacy Color

> Both Sass and CSS have similar legacy behavior that relies on all colors
> being interchangeable as part of a shared `srgb` color space. While the new
> color formats will opt users into new default behavior, some legacy color
> formats behave differently for the sake of backwards-compatibility.

Colors that are defined using the CSS color names, hex syntax, `rgb()`,
`rgba()`, `hsl()`, `hsla()`, or `hwb()` -- along with colors that result from
legacy interpolation -- are considered *legacy colors*. All legacy colors are
in the `srgb` gamut. The output of a legacy color is not required to match the
input [color space][].

### Color Space

Every color is stored internally as part of a defined color space. Each space
has a name, and an ordered list of associated channels that can be accessed and
manipulated in that space. Each channel value can be any number, or the keyword
`none`.

Values outside a _bounded gamut_ range are valid, and remain un-clamped, but
are considered _out of gamut_ for the given color space. Unbounded channels
that accept both number and percentage values can still provide a reference
range for mapping between the two value types. If the channel is bounded, or
has a reference range with a lower-boundary of zero, then the channel is
considered _scalable_.

> This follows the CSS specification, which defines percentage-mapping onto
> several channels that are technically unbounded. Note that some mappings
> have a lower boundary of `-100%` rather than `0`.

Colors specified using a CSS color keyword, or the hex notation are converted
to `rgb` and serialized as part of the `rgb` color space.

The color spaces and their channels are:

* `rgb` (RGB, legacy):
  * `red`, `green`, `blue`:
    * gamut: bounded
    * number: [0,255]
    * percentage: [0%,100%]

* `hwb` (RGB, legacy):
  * `hue`: polar angle
  * `whiteness`, `blackness`:
    * gamut: bounded
    * percentage: [0%,100%]

* `hsl` (RGB, legacy):
  * `hue`: polar angle
  * `saturation`, `lightness`:
    * gamut: bounded
    * percentage: [0%,100%]

* `srgb`, `srgb-linear`, `display-p3`, `a98-rgb`, `prophoto-rgb`,
  `rec2020` (RGB):
  * `red`, `green`, `blue`:
    * gamut: bounded
    * number: [0,1]
    * percentage: [0%,100%]

* `xyz`, `xyz-d50`, `xyz-d65`:
  * `x`, `y`, `z`:
    * gamut: un-bounded, scalable
    * number: [0,1]
    * percentage: [0%,100%]

* `lab`:
  * `lightness`:
    * gamut: un-bounded, scalable
    * number: [0,100]
    * percentage: [0%,100%]
  * `a`, `b`:
    * gamut: un-bounded
    * number: [-125,125]
    * percentage: [-100%,100%]

* `lch`:
  * `lightness`:
    * gamut: un-bounded, scalable
    * number: [0,100]
    * percentage: [0%,100%]
  * `chroma`:
    * gamut: un-bounded, scalable
    * number: [0,150]
    * percentage: [0%,100%]
  * `hue`: polar angle

* `oklab`:
  * `lightness`:
    * gamut: un-bounded, scalable
    * number: [0,1]
    * percentage: [0%,100%]
  * `a`, `b`:
    * gamut: un-bounded
    * number: [-0.4,0.4]
    * percentage: [-100%,100%]

* `oklch`:
  * `lightness`:
    * gamut: un-bounded, scalable
    * number: [0,1]
    * percentage: [0%,100%]
  * `chroma`:
    * gamut: un-bounded, scalable
    * number: [0,0.4]
    * percentage: [0%,100%]
  * `hue`: polar angle

### Predefined Color Spaces

> 'Predefined color spaces' can be described using the `color()` function.

The _predefined RGB spaces_ are:

* `srgb`
* `srgb-linear`
* `display-p3`
* `a98-rgb`
* `prophoto-rgb`
* `rec2020`

The _predefined XYZ spaces_ are:

* `xyz`
* `xyz-d50`
* `xyz-d65`

### Missing Components

> This feature is at-risk, pending proper specification:
> https://github.com/w3c/csswg-drafts/issues/7536#issuecomment-1202799109

In some cases, a color can have one or more missing components (channel or
alpha values). Missing components are represented by the keyword `none`. When
interpolating between colors, the missing component is replaced by the value
of that same component in the other color. In all other cases, the missing
value is treated as `0`.

### Powerless Components

> This feature is at-risk, pending proper specification:
> https://github.com/w3c/csswg-drafts/issues/7536#issuecomment-1202799109

In some color spaces, it is possible for a channel value to become 'powerless'
in certain circumstances. If a powerless channel value is produced as the
result of color-space conversion, then that value is considered to be
[missing][], and is replaced by the keyword `none`.

[missing]: #missing-components

* `hsl`:

  * If the `saturation` value is `0%`, then the `hue` channel is powerless.

  * If the `lightness` value is either `0%` or `100%`, then both the `hue` and
    `saturation` values are powerless.

* `hwb`:

  * If the combined `whiteness` and `blackness` values (after normalization)
    are equal to `100%`, then the `hue` channel is powerless.

* `lab`/`oklab`:

  * If the `lightness` value is `0%`, then both the `a` and `b` channels are
    powerless.

  > The current spec has an inline issue asking if high values of
  > `lightness` (whites) should make the `a` and `b` values powerless:
  > See: https://drafts.csswg.org/css-color-4/#issue-e05ac5c3

* `lch`/`oklch`:

  * If the `chroma` value is 0%, then the `hue` channel is powerless.

  * If the `lightness` value is `0%`, then both the `hue` and `chroma` channels
    are powerless.

  > The current spec has an inline issue asking if high values of
  > `lightness` (whites) should make the `hue` and `chroma` values powerless.
  > See: https://drafts.csswg.org/css-color-4/#issue-1813c844

### Color Gamuts

A _color gamut_ is a range of colors that can be displayed by a given device,
or can be described in a given [color space][]. A color is considered _out of
gamut_ if any channel value is outside a bounded range defined by the given
channel in the given color space.

The predefined RGB gamuts are:

* `srgb`
* `display-p3`
* `a98-rgb`
* `prophoto-rgb`
* `rec2020`

There are several additional color spaces that are associated with the
`srgb` gamut:

* `rgb`
* `srgb-linear`
* `hwb`
* `hsl`

All other color spaces describe unknown or theoretically infinite gamuts.

### Color Interpolation Method

<x><pre>
**ColorInterpolationMethod** ::= 'in' (
&#32;                                 RectangularColorSpace
&#32;                               | PolarColorSpace HueInterpolationMethod?
&#32;                             )
**RectangularColorSpace**    ::= 'srgb'
&#32;                          | 'srgb-linear'
&#32;                          | 'lab'
&#32;                          | 'oklab'
&#32;                          | 'xyz'
&#32;                          | 'xyz-d50'
&#32;                          | 'xyz-d65'
**PolarColorSpace**          ::= 'hsl'
&#32;                          | 'hwb'
&#32;                          | 'lch'
&#32;                          | 'oklch'
**HueInterpolationMethod**   ::= (
&#32;                                'shorter'
&#32;                              | 'longer'
&#32;                              | 'increasing'
&#32;                              | 'decreasing'
&#32;                              | 'specified'
&#32;                            ) 'hue'
</pre></x>


> Different color interpolation methods provide different advantages. For that
> reason, individual color procedures and functions (the host syntax) can
> establish their own color interpolation defaults, or provide a syntax for
> authors to explicitly choose the method that best fits their need.

The **host syntax** for a given interpolation procedure is the color syntax
or function that instigates that interpolation. When selecting a color
interpolation method:

* If the host syntax defines what method to use use, use the specified method.

* Otherwise, if all the colors involved are [legacy colors](#legacy-color),
  use `srgb`.

* Otherwise, use `oklab`.


## Procedures

### Converting a Color

Colors can be converted from one [color space][] to another. Algorithms for
color conversion are defined in the [CSS Color Level 4][color-4]
specification. Each algorithm takes a color `origin-color`, and a string
`target-space`, and returns a color `output-color`.

{% warn %}
There's an open issue about
how to properly handle `none` values while
converting a color.
See https://github.com/w3c/csswg-drafts/issues/7536
{% endwarn %}

[color-space]: #color-space

The algorithms are:

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

> Some [color spaces](#color-space) describe limited
> [color gamuts](#color-gamuts). If a color is 'out of gamut' for a particular
> space (most often because of conversion from a larger-gamut color-space), it
> can be useful to 'map' that color to the nearest available 'in-gamut' color.
> Gamut mapping is the process of finding an in-gamut color with the least
> objectionable change in visual appearance.

Gamut mapping in Sass follows the [CSS gamut mapping algorithm][css-mapping].
This procedure accepts a color `origin` in the color space `origin color space`,
and a destination color space `destination`. It returns the result of a
[CSS gamut map](css-map) procedure, which is a color in the `destination` color
space.

> This algorithm implements a relative colorimetric intent, and colors inside
> the destination gamut are unchanged.

[css-mapping]: https://drafts.csswg.org/css-color/#css-gamut-mapping-algorithm
[css-map]: https://drafts.csswg.org/css-color/#css-gamut-map

### Parsing Color Components

This procedure accepts an `input` parameter to parse, along with a `space`
parameter representing the [color space][], if known. It throws common parse
errors if necessary, and returns either `null` (if the `input` contains special
CSS values), or a list of parsed and normalized values. The return value is in
the format `<color-space>? (<channel>+) / <alpha>`, where the color space is
included in the return value if it was not passed in initially.

> This supports both the known color formats like `hsl()` and `rgb()`, where
> the space is determined by the function, as well as the syntax of `color()`,
> where the space is included as one of the input arguments (and may be a
> user-defined space).

The procedure is:

  * If `input` is a [special variable string][], return `null`.

  * Let `include-space` be true if `space` is undefined, and false otherwise.

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

      * If there are not two items in `split-last`, throw an error.

      * If either item in `split-last` can be coerced to a number, replace the
        current value of the item with the resulting number value.

      * Let `alpha` be the second element in `split-last`, and append the first
        element of `split-last` to `components`.

      > This solves for a legacy handling of `/` in Sass that would produce an
      > unquoted string when the alpha value is a css function such as `var()`
      > or when either value is the keyword `none`.

    * Otherwise, if the last element of `input` has preserved its status as two
      slash-separated numbers:

      * Let `alpha` be the number after the slash, and append the number before
        the slash to `components`.

    * Otherwise, append the last element of `input` to `components`

  * If `components` is undefined or an empty list, throw an error.

  * If `components` is a [special variable string][]:

    * Let `channels` be the value of `components`.

  * Otherwise:

    * If `components` is not an unbracketed space-separated list, throw an error.

    * If `space` is undefined, let `space` be the first element in `components`,
      and let `channels` be an unbracketed space-separated list with the
      remaining elements from `components`.

    * Otherwise, let `channels` be the value of `components`.

    * If `space` is not a string, throw an error.

    * Let `expected` be the number of channels in `space` if `space` is a known
      [color-space][], and null otherwise.

    * If `expected` is not null, and `channels` has more than `expected`
      elements, throw an error.

    * If any element of channels is not either a number, a special variable
      string, a special number string, or the keyword `none`, throw an error.

  * If `alpha` is undefined, let `alpha` be `1`.

  * Otherwise, If `alpha` is not a [special number string][]:

    * If `alpha` is a number, set `alpha` to the result of
      [percent-converting][] `alpha` with a max of 1.

    * Otherwise, throw an error.

  * If `space` or `channels` is a [special variable string][], or if `alpha` is
    a [special number string][], or if `space` is not a known [color space][],
    return `null`.

    > Unknown color spaces are valid in CSS, but should not be treated as
    > color objects for the sake of Sass manipulation.

  * If any element of `channels` is a [special number string][], return `null`.

    > Doing this late in the process allows us to throw any obvious syntax
    > errors, even for colors that can't be fully resolved on the server.

  * If `expected` is not null, and the length of `channels` is not equal to
    `expected`, throw an error.

    > Once special values have been handled, any colors remaining should have
    > exactly the expected number of channels.

  * Set `normal` to the result of [normalizing][] `channels` in `space`.

  * If `include-space` is true, let `parsed` be an unbracketed space-separated
    list with `space` as the first element, and `normal` as the second.

  * Otherwise, let `parsed` be the value of `normal`.

  * Return an unbracketed slash-separated list with `parsed` as the first
    element, and `alpha` as the second.

    > This results in valid CSS color-value output, while also grouping
    > space, channels, and alpha as separate elements in nested lists.
    > Alternately, we could allow `parsed` to be a single flat list, even
    > when the color-space is included?

[special variable string]: ../spec/functions.md#special-variable-string
[special number string]: ../spec/functions.md#special-number-string
[percent-converting]: #percent-converting-a-number

### Normalizing Color Channels

[normalizing]: #normalizing-color-channels

This process accepts an ordered list `channels` to validate, and a string
`space` to normalize against. It throws an error if any channel is invalid for
the [color space][] `space`, or returns a normalized list of valid channels.

* Let `normal` be an empty list.

* For each `channel` in `channels`:

  * If `channel` is not a number or the keyword `'none'`, throw an error.

  * If `channel` is the keyword `'none'`, append `channel` as the next item in
    `normal`.

  * Otherwise:

    * Let `valid` be the corresponding channel defined by the [color space][]
      `space`.

    * If `valid` is a polar-angle `hue`:

      * Let `normal-channel` be the result converting `channel` to `deg`
        allowing unitless.

      * Append `normal-channel` as the next item in `normal`.

      > Normalizing the result into a half-open range of `[0,360)` would be a
      > lossy transformation, since some forms of [hue interpolation][hue-method]
      > require the specified hue values.

    * Otherwise:

      * If `channel` is a number with units other than `%`, throw an error.

      * If `valid` requires a percentage, and `channel` is a number without
        units, throw an error.

      * Append `channel` as the next item in `normal`.

* Return `normal`.

### Interpolating Colors

This procedure accepts two color arguments (`color1` and `color2`), a
[color interpolation method][] `method`, and a percentage `weight` for `color1`
in the mix. It returns a new color `mix` that represents the appropriate mix of
input colors.

* If either `color1` or `color2` is not a color, throw an error.

* If `weight` is undefined, set `weight` to `50%`.

* Set `weight` to the result of [percent-converting][] `weight` with a max of 1.

* If `method` is undefined, throw an error.

* Otherwise:

  * If `method` is not a [color interpolation method][color-method], throw an
    error.

  * Let `space` be the [color space][] specified in `method`.

  * If `space` is a [PolarColorSpace][color-method]:

    * Let `hue-arc` be the `HueInterpolationMethod` specified in `method`, or
      `shorter` if no hue interpolation is specified.

* For each `color` of `color1` and `color2`:

  * Set `color` to the results of [converting][] `color` into `space`.

  * If any `component` of `color` is `none`, set that `component` to the value
    of the corresponding component in the other color.

    > If both values are `none`, the interpolation result for that component
    > will also be `none`.

  * Set `color` to the result of [premultiplying] `color`.

* Let `mix` be a new color in the `space` [color space][], with `none` for
  alpha and all channel values.

* For each `channel` of `mix`:

  * Let `channel1` and `channel2` be the corresponding channel values in
    `color1` and `color2` respectively.

  * If `channel` represents a hue angle, set `channel1` and `channel2`
    respectively to the results of [hue interpolation][hue-method] with
    `channel1` as `hue1`, `channel2` as `hue2`, using the `hue-arc` method.

  * Set `channel` to the result of calculating
    `(channel1 * weight) + (channel2 * (1 - weight))`.

    > Channel rounding has been removed, since it is a lossy transform.

* Return `mix`.

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

* If the `color` has an `alpha` value of 1, return `color` unchanged.

* Otherwise, for each `channel` in `color`:

  * If either the `alpha` value, or the `channel` value is `none`, or the
    `channel` represents a polar-angle `hue`, keep the original value of
    `channel`.

  * Otherwise, set `channel` to the result of multiplying the `channel` value
    by the `alpha` value.

* Return the resulting `color` with premultiplied channels.

The same process can be run in reverse, to **un-premultiply** the channels of a
given `color`:

* If the `color` has an `alpha` value of 1, return `color` unchanged.

* Otherwise, for each `channel` in `color`:

  * If either the `alpha` value, or the `channel` value is 0 or `none`, or the
    `channel` represents a polar-angle `hue`, keep the original value of
    `channel`.

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
in [CSS Color Level 4][color-4]. If the `method` is not the value
`'specified'`, both angles need to be constrained to `[0, 360)` prior to
interpolation.

* [shorter](https://www.w3.org/TR/css-color-4/#shorter)

* [longer](https://www.w3.org/TR/css-color-4/#hue-longer)

* [increasing](https://www.w3.org/TR/css-color-4/#increasing)

* [decreasing](https://www.w3.org/TR/css-color-4/#decreasing)

* [specified](https://www.w3.org/TR/css-color-4/#hue-specified)

[hue-interpolation]: https://www.w3.org/TR/css-color-4/#hue-interpolation


## Deprecated Functions

Individual color-channel functions defined globally or in the color module are
deprecated in favor of the new `color.channel()` function. That includes:

* `color.red()`/`red()`
* `color.green()`/`green()`
* `color.blue()`/`blue()`
* `color.hue()`/`hue()`
* `color.saturation()`/`saturation()`
* `color.lightness()`/`lightness()`
* `color.whiteness()`
* `color.blackness()`

While deprecated, if the specified color argument is not a [legacy color][],
throw an error.


## New Color Module Functions

These new functions are part of the built-in `sass:color` module.

### `color.space()`

* ```
  space($color)
  ```

  * If `$color` is not a color, throw an error.

  * Return a quoted string with the name of `$color`s associated
    [color space][].

### `color.to-space()`

* ```
  to-space($color, $space)
  ```

  * If `$color` is not a color, throw an error.

  * Let `origin-space` be the result of calling `space($color)`.

  * If `origin-space` equals-equals `$space`, return `$color`.

    > This allows unknown spaces, as long as they match the origin space.

  * If `$space` is not a [color space][], throw an error.

  * Return the result of [converting][] the `origin-color`
    `$color` to the `target-space` `$space`.

### `color.is-legacy()`

* ```
  is-legacy($color)
  ```

  * If `$color` is not a color, throw an error.

  * Return `true` if `$color` is a [legacy color][], or `false`
    otherwise.

### `color.is-powerless()`

* ```
  is-powerless($color, $channel, $space)
  ```

  * If `$color` is not a valid color, throw an error.

  * If `$space` is null:

    * Let `color` be the value of `color`, and let `space` be the result of
      calling `space($color)`.

  * Otherwise:

    * Let `color` be the result of calling `to-space($color, $space)`, and let
      `space` be the value of `$space`.

  * If `$channel` is not the name of a channel in the color-space `space`,
    throw an error.

  * Return `true` if the channel `$channel` is [powerless](#powerless-components)
    in `color`, otherwise return `false`.

### `color.is-in-gamut()`

* ```
  is-in-gamut($color, $space)
  ```

  * If `$color` is not a color, throw an error.

  * Let `space` be the value of `$space` if specified, or the result of calling
    `space($color)` otherwise.

  * If `space` is not a valid [color space][], throw an error.

  * Let `gamut` be the [color gamut](#color-gamuts) associated with `space`
    if an association is defined, or the value of `space` otherwise.

  * Let `color` be the result of calling `to-space($color, space)`.

  * For all bounded channels in `space`, if the associated channel value in
    `$color` is outside the bounded range, return `false`.

  * Otherwise, return `true`.

### `color.to-gamut()`

* ```
  to-gamut($color, $space)
  ```

  * If `$color` is not a color, throw an error.

  * Let `origin-space` be the result of calling `space($color)` otherwise.

  * Let `target-space` be the value of `$space` if specified, or the value of
    `origin-space` otherwise.

  * If `target-space` is not a valid [color space][], throw an error.

  * Return the result of [gamut mapping][] with `$color` as the
    origin color, `origin-space` as the origin color space, and
    `target-space` as the destination color space.

[gamut mapping]: #gamut-mapping

### `color.channel()`

> Note that channel values are stored as specified, even if those values are
> out-of-gamut for the [color space][] used. Similarly, this color-channel
> inspection function may return out-of-gamut channel values.

* ```
  channel($color, $channel, $space)
  ```

  * If `$space` is null:

    * Let `space` be the result of calling `space($color)`, and let `color` be
      the value of `$color`.

  * Otherwise:

    * Let `color` be the result of calling `to-space($color, $space)`, and let
      `space` be the value of `$space`.

  * If `space` is a known [color space][], let `channels` be a map of channel
    names defined for `space`, and their corresponding values in `color`, or a
    map with 1-indexed number keys and their corresponding values in `color`
    otherwise.

  * Let `value` be the result of calling `map.get(channels, $channel)`.

  * If `value` is `null`, throw an error.

  * Otherwise, return `value`.


## Modified Color Module Functions

### `color.hwb()`

These functions are now deprecated. Authors should use global `hwb()` instead.

> Channel clamping and scaling have been removed from the global function,
> since we now allow out-of-gamut color-channels to be stored as specified.

* ```
  hwb($channels)
  ```

  This function is available as a global function named `hwb()`.

* ```
  hwb($hue, $whiteness, $blackness, $alpha: 1)
  ```

  * Return the result of calling the global function
    `hwb($hue $whiteness $blackness / $alpha)`.

### `color.mix()`

```
mix($color1, $color2,
  $weight: 50%,
  $method: null)
```

* If `$method` undefined:

  * If `$color1` and `$color2` are both legacy colors, let `method` be `rgb`.

  * Otherwise, throw an error.

    > Method is required for non-legacy colors. This matches the `color-mix()`
    > function defined in [Colors Level 5][color-5], and allows us to add
    > additional default behavior in the future.

* Otherwise:

  * If `method` is not a [color interpolation method][color-method], throw an
    error.

  * Let `method` be the value of `$method`.

  * Let `space` be the [color space][] specified in `method`.

* If `space` is one of 'hsl' or 'hwb':

  * Let `color1` and `color2` be the result of [converting][] and
    [gamut mapping][] `$color1` and `$color2` respectively into `space`.

  > These color spaces are unable to express colors outside the `srgb` gamut.

* Otherwise, let `color1` and `color2` be the result of [converting][]
  `$color1` and `$color2` respectively into `space`.

* Return the result of [interpolating](#interpolating-colors) between `color1`
  and `color2` with the specified `$weight` and `method`.

### `color.alpha()`

* ```
  alpha($color)
  ```

  * If `$color` is not a color, call the other overload and return its result.

  * Return the alpha channel of `$color` as a unitless number.

* ```
  alpha($args...)
  ```

  > This overload exists to support Microsoft's proprietary [`alpha()`
  > function][].

  [`alpha()` function]: https://blogs.msdn.microsoft.com/ie/2010/08/17/ie9-opacity-and-alpha/

  * If `$args` is empty, throw an error.

  * If `$args` has any keyword arguments, throw an error.

  * Unless all arguments of `$args` are unquoted strings that begin with a
    sequence of ASCII letters, followed by one or more spaces, followed by `=`
    throw an error.

  * Return a plain CSS function string with the name `"alpha"` and the arguments
    `$args`.

### `color.change()`

```
change($color, $args...)
```

This function is also available as a global function named `change-color()`.

* If `$color` is not a color, throw an error.

* If any item in `$args` is not a keyword argument, throw an error.

* Let `space` be `$color`'s [color space][].

* If the keyword argument `$space` is specified in `$args`:

    * If `$space` is not a known [color space][], throw an error.

    * Set `space` to the value of `$space`.

    * Let `color` be the result of [converting][] `$color` to `space`.

* Otherwise, let `color` be the value of `$color`.

* Let `legacy` be `true` if `$color` is a legacy color and `space` is a
  legacy color space, and `false` otherwise.

* Let `alpha` be `color`'s alpha property.

* If the keyword argument `$alpha` is specified in `$args`:

    * If `$alpha` is not a unitless number between `-1` and `1` (inclusive),
      throw an error.

    * Set `alpha` to the value of `$alpha + alpha` clamped between 0 and 1.

* Let `channel-args` be the remaining keyword arguments in `$args`, not
  including `$space` or `$alpha` arguments.

* Let `channels` be a list of the `color`'s channels.

* For each keyword `new` in `channel-args`:

  * If `new` is not the name of a channel in `channels`, throw an error.

  * Set the corresponding `channel` in `channels` to `new`.

* Let `normal` be the result of [normalizing][] `channels`.

* Return a color in the `space` [color space][], with `normal` channels, an
  alpha value of `alpha`, and a legacy value of `legacy`.

### `color.adjust()`

```
adjust($color, $args...)
```

This function is also available as a global function named `adjust-color()`.

* If `$color` is not a color, throw an error.

* If any item in `$args` is not a keyword argument, throw an error.

* Let `space` be `$color`'s [color space][].

* If the keyword argument `$space` is specified in `$args`:

    * If `$space` is not a known [color space][], throw an error.

    * Set `space` to the value of `$space`.

    * Let `color` be the result of [converting][] `$color` to `space`.

* Otherwise, let `color` be the value of `$color`.

* Let `legacy` be `true` if `$color` is a legacy color and `space` is a
  legacy color space, and `false` otherwise.

* Let `alpha` be `color`'s alpha property.

* If the keyword argument `$alpha` is specified in `$args`:

    * If `$alpha` is not a unitless number between `-1` and `1` (inclusive),
      throw an error.

    * Set `alpha` to the value of `$alpha + alpha` clamped between 0 and 1.

* Let `channel-args` be the remaining keyword arguments in `$args`, not
  including `$space` or `$alpha` arguments.

* Let `channels` be a list of the `color`'s channels.

* For each keyword `adjust` in `channel-args`:

  * If `adjust` is not the name of a channel in `channels`, throw an error.

  * Set the corresponding `channel` in `channels` to `channel + adjust`,
    treating any `'none'` keyword as a value of `0`.

* Let `normal` be the result of [normalizing][] `channels`.

* Return a color in the `space` [color space][], with `normal` channels, an
  alpha value of `alpha`, and a legacy value of `legacy`.

### `color.scale()`

```
scale($color, $args...)
```

This function is also available as a global function named `scale-color()`.

* If `$color` is not a color, throw an error.

* If any item in `$args` is not a keyword argument, throw an error.

* Let `space` be `$color`'s [color space][].

* If the keyword argument `$space` is specified in `$args`:

    * If `$space` is not a known [color space][], throw an error.

    * Set `space` to the value of `$space`.

    * Let `color` be the result of [converting][] `$color` to `space`.

* Otherwise, let `color` be the value of `$color`.

* Let `legacy` be `true` if `$color` is a legacy color and `space` is a
  legacy color space, and `false` otherwise.

* Let `alpha` be `color`'s alpha property.

* If the keyword argument `$alpha` is specified in `$args`:

    * If `$alpha` is not a unitless number between `-1` and `1` (inclusive),
      throw an error.

    * Set `alpha` to the result of [scaling][] `alpha` by `$alpha` with `max` 1,
      treating any `'none'` keyword as a value of `0`.

* Let `channel-args` be the remaining keyword arguments in `$args`, not
  including `$space` or `$alpha` arguments.

* Let `channels` be a list of the `color`'s channels.

* For each keyword `scale` in `channel-args`:

  * If `scale` is not the name of a bounded or percentage-mapped channel in
    `channels`, throw an error.

  * Set the corresponding `channel` in `channels` to the result of [scaling][]
    `channel` by `scale` with a `max` defined by the channel boundary.

* Let `normal` be the result of [normalizing][] `channels`.

* Return a color in the `space` [color space][], with `normal` channels, an
  alpha value of `alpha`, and a legacy value of `legacy`.

### `color.complement()`

```
complement($color, $space)
```

This function is also available as a global function named `complement()`.

* If `$color` is not a color, throw an error.

* If `$space` is undefined:

  * If `$color` is a legacy color, let `space` be `hsl`.

  * Otherwise, throw an error.

* Otherwise:

  * If `$space` is not a [color space][] with a polar-angle hue channel, throw
    an error.

    > This currently allows `hsl`, `hwb`, `lch`, and `oklch`. We could also
    > map some cubic spaces with matching gamuts e.g. (`oklab` -> `oklch`),
    > but that adds implicit complexity, while still excluding color spaces.

  * Let `space` be the value of `$space`.

* Return the result of calling `color.adjust()` with `$color`, a $hue of
  `180deg`, and a $space of `space`.

### `color.invert()`

```
invert($color, $space)
```

This function is also available as a global function named `invert()`.

* If `$color` is not a color, throw an error.

* If `$space` is undefined:

  * If `$color` is a legacy color, let `space` be `hsl`.

  * Otherwise, throw an error.

* Otherwise:

  * If `$space` is not a [color space][], throw an error.

  * If `$space` is 'xyz', 'xyz-d50', or 'xyz-d65', throw an error.

    > It might be possible to define these inversions, but I'm not clear on
    > the algorithm at this point.

  * If `$space` is 'lab', let `space` be `lch`.

  * Otherwise, if `$space` is 'oklab', let `space` be `oklch`.

  * Otherwise, let `space` be the value of `$space`.

* Let `color` be the result of [converting][] and [gamut mapping][] `$color`
  into the color space `space`.

* If `space` is a color space with a lightness channel and polar-angle hue:

  * Let `light` be the value of `color`'s lightness channel as a percentage,
    and let `hue` be be the value of `color`'s hue channel.

  * Let `light-output` be the result of `100% - light`.

  * Let `hue-output` be the result of `(hue + 180deg) % 360deg`.

  * Let `invert` be the result of calling `color.change()` with `color`, a $hue
    argument of `hue-output`, and a lightness of `light-output`.

  * If `$space` is defined and not equal to `space`, return the result of
    [converting][] `color` into `$space`.

    > This only applies to `lab`/`oklab` and `lch`/`oklch` pairings.

  * Otherwise, return `color`.

* Otherwise, if `space` is 'hwb':

  * Let `hue`, `whiteness` and `blackness` be the three elements of `color`'s
    channels.

  * Let `hue-output` be the result of `(hue + 180deg) % 360deg`.

  * Let `gray` be the result of `whiteness + blackness`.

  * Return the result of calling `color.change()` with `color`, a $hue of `hue`,
    $whiteness of `gray - whiteness`, and $blackness of `gray - blackness`.

* Otherwise:

  > Inversion for RGB color spaces.

  * Let `invert` be the value of `color`.

  * For each `channel` element in `color`'s channels:

    * Let `max` be the maximum bounded value of `channel` in `space`, in the
      same unit as `channel`.

    * Set the corresponding channel of `invert` to be `max - channel`.

  * Return `invert`.

### `color.grayscale()`

```
grayscale($color)
```

> No space argument is provided, since the results should always be in gamut.

This function is also available as a global function named `grayscale()`.

* If `$color` is not a color, throw an error.

* If `$color` is a legacy color:

  * Return the result of [converting][] `$color` to 'hsl', and changing the
    'saturation' channel to 0.

* Otherwise:

  * Let `origin` be `$color`'s [color space].

  * Let `color` be the result of [converting][] `$color` to 'oklch', and
    setting the 'chroma' channel to 0.

  * Return the result of [converting][] `color` to `origin`.

### `color.ie-hex-str()`

This function is also available as a global function named `ie-hex-str()`. Both
functions are deprecated.

```
ie-hex-str($color)
```

* If `$color` is not a color, throw an error.

* Let `rgb` be the result of [converting][] `$color` to 'rgb'.

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

  * Let `components` be the result of [parsing] `$channels` with an `hwb` space.

  * If `components` is null, return a plain CSS function string with the name
    `"hwb"` and the argument `$channels`.

  * Let `channels` be the first element and `alpha` the second element of
    `components`.

  * Let `hue`, `whiteness`, and `blackness` be the three elements of `channels`.

    > Channel clamping and scaling have been removed, since we now allow
    > out-of-gamut color-channels to be stored as specified.

  * Return a [legacy color][] in the `hwb` space, with the given `hue`,
    `whiteness`, and `blackness` channels, and `alpha` value.

[parsing]: #parsing-color-components

### `lab()`

* ```
  lab($channels)
  ```

  * Let `components` be the result of [parsing] `$channels` in an `lab` space.

  * If `components` is null, return a plain CSS function string with the name
    `"lab"` and the argument `$channels`.

  * Let `channels` be the first element and `alpha` the second element of
    `components`.

  * Let `lightness`, `a`, and `b` be the three elements of `channels`.

  * Return a color in the `lab` [color space][], with the given `lightness`,
    `a`, and `b` channels, and `alpha` value.

### `lch()`

* ```
  lch($channels)
  ```

  * Let `components` be the result of [parsing] `$channels` in an `lch` space.

  * If `components` is null, return a plain CSS function string with the name
    `"lab"` and the argument `$channels`.

  * Let `channels` be the first element and `alpha` the second element of
    `components`.

  * Let `lightness`, `chroma`, and `hue` be the three elements of `channels`.

  * Return a color in the `lch` [color space][], with the given `lightness`,
    `chroma`, and `hue` channels, and `alpha` value.

### `oklab()`

* ```
  oklab($channels)
  ```

  * Let `components` be the result of [parsing] `$channels` in an `oklab` space.

  * If `components` is null, return a plain CSS function string with the name
    `"lab"` and the argument `$channels`.

  * Let `channels` be the first element and `alpha` the second element of
    `components`.

  * Let `lightness`, `a`, and `b` be the three elements of `channels`.

  * Return a color in the `oklab` [color space][], with the given `lightness`,
    `a`, and `b` channels, and `alpha` value.

### `oklch()`

* ```
  oklch($channels)
  ```

  * Let `components` be the result of [parsing] `$channels` in an `oklch` space.

  * If `components` is null, return a plain CSS function string with the name
    `"lab"` and the argument `$channels`.

  * Let `channels` be the first element and `alpha` the second element of
    `components`.

  * Let `lightness`, `chroma`, and `hue` be the three elements of `channels`.

  * Return a color in the `oklch` [color space][], with the given `lightness`,
    `chroma`, and `hue` channels, and `alpha` value.

### `color()`

* ```
  color($description)
  ```

  * Let `components` be the result of [parsing] `$description` with
    undefined space.

  * If `components` is null, return a plain CSS function string with the name
    `"color"` and the argument `$description`.

  * Let `color` be the first element and `alpha` the second element of
    `components`.

  * Let `space` be the first element and `channels` the second element of
    `color`.

  * If `space` is not a [predefined color space][predefined], throw an error,.

    > Custom spaces have already been output as CSS functions.

  * Return a color in the `space` [color space][], with the given `channels`
    and `alpha` value.

[predefined]: #predefined-color-spaces

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

  * If any argument is a [special number][], return a plain CSS function
    string with the name `"rgb"` and the arguments `$red`, `$green`, `$blue`,
    and `$alpha`.

  * If `$alpha` is not a number, throw an error.

  * Let `alpha` be the result of [percent-converting][] `alpha` with a max of 1.

  * Let `red`, `green`, and `blue` be the three elements returned by
    [normalizing][] `($red, $green, $blue)` in `rgb` color space.

  * Return a [legacy color][] in the `rgb` space, with the given `red`,
    `green`, and `blue` channels, and `alpha` value.

* ```
  rgb($red, $green, $blue)
  ```

  * If any argument is a [special number][], return a plain CSS function string
    with the name `"rgb"` and the arguments `$red`, `$green`, and `$blue`.

  * Otherwise, return the result of calling `rgb()` with `$red`, `$green`,
    `$blue`, and `1`.

* ```
  rgb($channels)
  ```

  * Let `components` be the result of [parsing] `$channels` with an `rgb` space.

  * If `components` is null, return a plain CSS function string with the name
    `"rgb"` and the argument `$channels`.

  * Let `channels` be the first element and `alpha` the second element of
    `components`.

  * Let `red`, `green`, and `blue` be the three elements of `channels`.

  * Return the result of calling `rgb()` with `red`, `green`, `blue`, and
    `alpha` as arguments.

* ```
  rgb($color, $alpha)
  ```

  * If either argument is a [special variable string][], return a plain CSS
    function string with the name `"rgb"` and the same arguments.

  * If `$color` is not a [legacy color][], throw an error.

  * Return the result of calling `rgb()` with `$color`'s red, green, and blue
    channels as unitless number arguments, and `$alpha` as the final argument.

### `hsl()` and `hsla()`

The `hsla()` function is identical to `hsl()`, except that if it would return a
plain CSS function named `"hsl"` that function is named `"hsla"` instead.

* ```
  hsl($hue, $saturation, $lightness, $alpha: 1)
  ```

  * If any argument is a [special number][], return a plain CSS function
    string with the name `"hsl"` and the arguments `$hue`, `$saturation`,
    `$lightness`, and `$alpha`.

  * If `$alpha` is not a number, throw an error.

  * Let `alpha` be the result of [percent-converting][] `alpha` with a max of 1.

  * Let `hue`, `saturation`, and `lightness` be the three elements returned
    by [normalizing][] `($hue, $saturation, $lightness)` in `hsl` color space.

  > Clamping and conversion to rgb have been removed.

  * Return a [legacy color][] in the `hsl` space, with the given `hue`,
    `saturation`, and `lightness` channels, and `alpha` value.

* ```
  hsl($hue, $saturation, $lightness)
  ```

  * If any argument is a [special number][], return a plain CSS function string
    with the name `"hsl"` and the arguments `$hue`, `$saturation`, and
    `$lightness`.

  * Otherwise, return the result of calling `hsl()` with `$hue`, `$saturation`,
    `$lightness`, and `1`.

* ```
  hsl($hue, $saturation)
  ```

  * If either argument is a [special variable string][], return a plain CSS
    function string with the name `"hsl"` and the same arguments.

  * Otherwise, throw an error.

* ```
  hsl($channels)
  ```

  * Let `components` be the result of [parsing] `$channels` with an `hsl` space.

  * If `components` is null, return a plain CSS function string with the name
    `"hsl"` and the argument `$channels`.

  * Let `channels` be the first element and `alpha` the second element of
    `components`.

  * Let `hue`, `saturation`, and `lightness` be the three elements of `channels`.

  * Return a [legacy color][] in the `hsl` space, with the given `hue`,
    `saturation`, and `lightness` channels, and `alpha` value.
