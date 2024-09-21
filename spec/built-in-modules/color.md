# Color Module

This built-in module is available from the URL `sass:color`.

## Table of Contents

* [Definitions](#definitions)
  * [Color Interpolation Method](#color-interpolation-method)
* [Procedures](#procedures)
  * [Looking Up a Known Color Space](#looking-up-a-known-color-space)
  * [Converting a Color](#converting-a-color)
  * [CSS-Converting a Color Space](#css-converting-a-color-space)
  * [Gamut Mapping](#gamut-mapping)
    * [`local-minde`](#local-minde)
    * [`clip`](#clip)
  * [Parsing Color Components](#parsing-color-components)
  * [Percent-Converting a Number](#percent-converting-a-number)
  * [Validating a Color Channel](#validating-a-color-channel)
  * [Normalizing Color Channels](#normalizing-color-channels)
  * [Interpolating Legacy Colors](#interpolating-legacy-colors)
  * [Interpolating Colors](#interpolating-colors)
    * [Premultiply Transparent Colors](#premultiply-transparent-colors)
    * [Hue Interpolation](#hue-interpolation)
  * [Scaling a Number](#scaling-a-number)
* [Functions](#functions)
  * [`adjust()`](#adjust)
  * [`adjust-hue()`](#adjust-hue)
  * [`alpha()`](#alpha)
  * [`blackness()`](#blackness)
  * [`blue()`](#blue)
  * [`change()`](#change)
  * [`channel()`](#channel)
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
  * [`is-in-gamut()`](#is-in-gamut)
  * [`is-legacy()`](#is-legacy)
  * [`is-missing()`](#is-missing)
  * [`is-powerless()`](#is-powerless)
  * [`lighten()`](#lighten)
  * [`lightness()`](#lightness)
  * [`mix()`](#mix)
  * [`opacify()`](#opacify)
  * [`red()`](#red)
  * [`same()`](#same)
  * [`saturate()`](#saturate)
  * [`saturation()`](#saturation)
  * [`scale()`](#scale)
  * [`space()`](#space)
  * [`to-gamut()`](#to-gamut)
  * [`to-space()`](#to-space)
  * [`transparentize()`](#transparentize)
  * [`whiteness()`](#whiteness)

## Definitions

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

[known color space]: ../types/color.md#known-color-space

`ColorInterpolationMethod` is case-insensitive.

> Different color interpolation methods provide different advantages. For that
> reason, individual color procedures and functions can establish their own
> color interpolation defaults, or provide a syntax for authors to explicitly
> choose the method that best fits their need. The [CSS Color Level 4][color-4]
> specification provides [additional guidance][default-space] for determining
> appropriate defaults.

[default-space]: https://www.w3.org/TR/css-color-4/#interpolation-space
[color interpolation method]: #color-interpolation-method

## Procedures

### Looking Up a Known Color Space

This procedure accepts a `name`, and attempts to look up a [known color space]
with a matching name. It returns the color space named `name`, or throws an
error if `name` is not the name of a known color space.

* If `name` is not an unquoted string, throw an error.

* If `name`'s value is case-insensitively equal to the name of a [known color
  space], return that color space.

* Otherwise, throw an error.

  > In the future, we can add support for custom/unknown spaces by returning
  > `null` when no space is found.

[looking up a known color space]: #looking-up-a-known-color-space

### Converting a Color

Colors can be converted from one [known color space] to another. This procedure
accepts a color `origin-color`, and `target-space` which may be either a [known
color space] or a SassScript value, and returns a color `color` in
`target-space`.

> Since the individual CSS color conversion algorithms don't explicitly handle
> the process of "carrying over" missing values on analogous channels, we have
> to handle that here.

* If `target-space` is a SassScript value, set it to the result of [looking up a
  known color space] named `target-space`.

* If `origin-color`'s space is already `target-space`, return `origin-color`.

  > CSS doesn't perform conversions unless they are required.

* Let `missing` be a list of channel names in `origin-color` that are [missing].

* Let `color` be the result of [css-converting] `origin-color` into
  `target-space`.

* For each `channel` in `missing`:

  * If `target-space` has an [analogous component][missing] to `channel`, set
    the analogous component in `color` to `none`.

* If `target-space` is not a [legacy color] space and any `channel`s of `color`
  are [powerless] and not already [missing], set those channels to the special
  value `none`.

  > Don't introduce missing colors to legacy spaces because if users are
  > converting to legacy spaces they're likely targeting older browsers which
  > will choke on `none`.

* Return `color`.

[missing]: ../types/color.md#missing-components
[powerless]: ../types/color.md#powerless-components

### CSS-Converting a Color Space

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

* [Lab to LCH, Oklab to Oklch](https://www.w3.org/TR/css-color-4/#lab-to-lch)

* [LCH to Lab, Oklch to Oklab](https://www.w3.org/TR/css-color-4/#lch-to-lab)

* [Between predefined RGB spaces](https://www.w3.org/TR/css-color-4/#predefined-to-predefined)

* [Any RGB to Lab/Oklab](https://www.w3.org/TR/css-color-4/#predefined-to-lab-oklab)

* [Lab/Oklab to any RGB](https://www.w3.org/TR/css-color-4/#oklab-lab-to-predefined)

> For additional details, see the [Sample code for color conversions][convert].

[convert]: https://www.w3.org/TR/css-color-4/#color-conversion-code

### Gamut Mapping

> Some [known color space]s describe limited color gamuts. If a color is "out of
> gamut" for a particular space (most often because of conversion from a
> larger-gamut color-space), it can be useful to "map" that color to the nearest
> available "in-gamut" color. Gamut mapping is the process of finding an
> in-gamut color with the least objectionable change in visual appearance, which
> is to some degree inherently contextual and subjective.

Gamut mapping takes a color `origin`, a [known color space] `destination`, and
an unquoted string `method`. It returns a color in `origin`'s color space.

* Let `color` be the result of [converting] `origin` into `destination`.

* If `method` is not case-insensitively equal to one of the methods defined
  below, throw an error.

* If `color.is-in-gamut(color)`, return `origin`.

* Let `mapped` be the result of running the method defined below whose name
  matches `method`.

* Return the result of [converting] `mapped` into `origin`'s space.

#### `local-minde`

The `local-minde` gamut mapping procedure in Sass follows the 13 February 2024
draft of CSS Color Module Level 4. It returns the result of [CSS gamut
mapping][css-mapping] `origin` with an origin color space of `origin-space` and
destination of `destination`.

[css-mapping]: https://www.w3.org/TR/2024/CRD-css-color-4-20240213/#css-gamut-mapping-algorithm

> This algorithm implements a relative colorimetric intent, and colors inside
> the destination gamut are unchanged.

#### `clip`

The `clip` gamut mapping procedure is not expected to produce good-looking
results, but it can be useful to match the current behavior of browsers.

* Let `new-color` be a copy of `color`.

* For each `channel` in `new-color`'s channels:

  * If `channel` is bounded and not [missing], set its value to the result of
    clamping the original value within `channel`'s minimum and maximum values.

* Return `new-color`.

### Parsing Color Components

This procedure accepts an `input` parameter to parse, along with an optional
[known color space] `space`. It throws common parse errors when necessary, and
returns either a single string of components to emit in a CSS function, or
three values: a color space, a list of channel values, and an alpha value.

> This supports both the space-specific color formats like `hsl()` and `rgb()`,
> where the space is determined by the function, as well as the syntax of
> `color()`, where the space is included as one of the input arguments.

The procedure is:

* If `input` is a [special variable string], return an unquoted string with
  the value of `input`.

* If `input` is a bracketed list, or a list with a separator other than
  slash or space, throw an error.

* If `input` is a slash-separated list:

  * If `input` doesn't have exactly two elements, throw an error.

  * Otherwise, let `components` be the first element and `alpha` the second
    element of `input`.

* Otherwise:

  * Let `components` be an unbracketed space separated list of all except the
    last element of `input`.

  * If the last element of `input` is an unquoted string that contains `/`:

    > This solves for a legacy handling of `/` in Sass that would produce an
    > unquoted string when the alpha value is a CSS function such as `var()`,
    > when either value is `none`, or when using relative color syntax.

    * Let `split-last` be the result calling `string.split()` with the last
      element of `input` as the string to split, and `/` as the separator.

    * If `split-last` doesn't have exactly two items, return an unquoted string
      with the value of `input`.

      > This ensures that `rgb(1 2 calc(var(--a) / var(--b)) / var(--c))` is
      > handled correctly after the final expresssion is fully converted to a
      > string due to legacy `/` behavior.

    * If either item in `split-last` can be parsed as a number, replace the
      current value of the item with the resulting number value.

    * Let `alpha` be the second element in `split-last`, and append the first
      element of `split-last` to `components`.

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

  * If the first element of `components` is an unquoted string which is
    case-insensitively equal to "from", return an unquoted string with the
    value of `input`.

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

  * If any element of `channels` is not either a number, a special variable
    string, a [special number], or an unquoted string that's
    case-insensitively equal to "none", throw an error.

* If `alpha` is null, let `alpha` be `1`.

* Otherwise, If `alpha` is neither a [special number] nor an unquoted string
  that's case-insensitively equal to "none":

  * If `alpha` is not a number, throw an error.

  * Set `alpha` to the result of [percent-converting] `alpha` with a max of 1,
    and then clamping the value between 0 and 1, inclusive.

* If `channels` is a [special variable string], or if `alpha` is a [special
  number], return an unquoted string with the value of `input`.

* If any element of `channels` is a [special number]:

  * If `space` is a [legacy color] space:

    * Let `comma-list` be the result of calling
      `list.append(channels, alpha, "comma")`.

    * Return an unquoted string with the value of `comma-list`.

  * Otherwise, return an unquoted string with the value of `input`.

    > Doing this late in the process allows us to throw any obvious syntax
    > errors, even for colors that can't be fully resolved during compilation.

* If the length of `channels` is not 3, throw an error.

  > Once special values have been handled, any colors remaining should have
  > exactly the expected number of channels.

* Set `channels` to the result of [normalizing] `channels` in `space`.

* Let `space-name` be a lowercase unquoted string of `space`'s name.

* Return `space-name`, `channels` channels, and `alpha` alpha value.

[special variable string]: ../functions.md#special-variable-string
[special number]: ../functions.md#special-number
[percent-converting]: #percent-converting-a-number

### Percent-Converting a Number

This algorithm takes a SassScript number `number` and a number `max`. It returns
a number relative to the range `[0,max]` without clamping.

> In order to support both out-of-gamut channels and unbounded ranges, this
> value is no longer clamped between 0 and `max`

* If `number` has units other than `%`, throw an error.

* Otherwise, if `number` has the unit `%`, return `number * max / 100%`.

* Otherwise, return `number`.

### Validating a Color Channel

[validating]: #validating-a-color-channel

This process accepts a SassScript value `value` to validate, a [known color
space] `space` to validate against, and `channel` a channel in `space`. It
throws an error if the channel is invalid for the color space, or returns a
normalized channel value otherwise.

* If `value` is not a number or an unquoted string that's case-insensitively
  equal to "none", throw an error.

* If `value` is an unquoted string that's case-insensitively equal to "none",
  return `value`.

* Otherwise:

  * If `channel` is a polar angle:

    * Let `angle` be the result of [converting][number-to-unit] `value` to `deg`
      allowing unitless.

    * Return the result of `angle % 360deg`.

  * Let `min` and `max` be the upper and lower bounds of `channel`'s range,
    respectively.

  * Otherwise, set `channel` to the result of [percent-converting] `channel`
    with `min` and `max`.

  * If this was (transitively) invoked from the global [`rgb()`], [`lab()`],
    [`lch()`], [`oklab()`], [`oklch()`], or [`color()`] functions and `valid` is
    a clamped channel, return the result of clamping `value` within `min` and
    `max`.

  * Return `value`.

[number-to-unit]: https://github.com/sass/sass/blob/main/spec/types/number.md#converting-a-number-to-a-unit
[`rgb()`]: ../functions.md#rgb-and-rgba
[`lab()`]: ../functions.md#lab
[`lch()`]: ../functions.md#lch
[`oklab()`]: ../functions.md#oklab
[`oklch()`]: ../functions.md#oklch
[`color()`]: ../functions.md#color

### Normalizing Color Channels

[normalizing]: #normalizing-color-channels

This process accepts a list of `values` to validate and a [known color space]
`space` to normalize against. It throws an error if any channel is invalid for
the color space, or returns a normalized list of valid channels otherwise.

* If `values` is not a list, throw an error.

* Let `normal` be an empty list.

* For each `value` at position `i` in `values`:

  * Let `channel` be the channel at position `i` in `space`'s channels.

  * Let `valid` be the result of [validating] `value` as `channel` in `space`.

  * Append `valid` as the next item in `normal`.

* Let `unitless` be an empty list.

* For each `value` in `normal`.

  * If the value of `value` is the special value `none`, append `none` as the
    next item in `unitless`.

  * Otherwise, append the value of `value` as a [double] without units as the
    next item in `unitless`.

* Return `unitless`.

### Interpolating Legacy Colors

> This procedure is based on the legacy behavior of the `color.mix()` function,
> but returns a color in the original `color1` color space.

This procedure accepts two [legacy colors] (`color1` and `color2`), and an
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

* Let `mix` be an `rgb` color with the given `red`, `green`, and `blue`
  channels, and `alpha` value.

* Return the result of [converting] `mix` into `origin-space`.

[legacy interpolation]: #interpolating-legacy-colors

### Interpolating Colors

> This procedure is based on the
> [color interpolation](https://www.w3.org/TR/css-color-4/#interpolation)
> procedures defined in [CSS Color Level 4][color-4].

This procedure accepts two color arguments (`color1` and `color2`), a [color
interpolation method] `method`, and a percentage `weight` for `color1` in the
mix. It returns a new color that represents the appropriate mix of input colors.

* Let `origin-space` be `color1`'s color space.

* If `weight` is null, set `weight` to `0.5`.

* Otherwise, set `weight` to the result of [percent-converting] `weight` with a
  max of 1.

* If `weight > 1` or `weight < 0`, throw an error.

* If `weight == 0`, return the result of [converting] `color2` into
  `origin-space`.

* If `weight == 1`, return `color1`.

* Let `space` be the *interpolation color space* specified by `method`.

* If `space` is a [`PolarColorSpace`][color-method]:

  * Let `hue-arc` be the `HueInterpolationMethod` specified in `method`, or
    `shorter` if no hue interpolation is specified.

* Set `color1` and `color2` respectively to the results of [converting] `color1`
  and `color2` into `space`.

* For each `color` in `color1` and `color2`:

  * If any non-`alpha` component of `color` is [missing], set that its value to
    the value of the corresponding component in the other color unless it's
    missing as well.

  * Set `color` to the result of [premultiplying] `color`.

  * If `color`'s `alpha` component is [missing], set its value to the value of
    the `alpha` component in the other color unless it's missing as well.

    > This is resolved after premultiplying, because premultiplying has special
    > handling for a missing `alpha` component.

* Let `mix` be a new color in the color space `space`, with all channel and
  alpha values initially [missing].

* For each `channel` of `mix`:

  * If the corresponding channels of `color1` and `color2` are missing, continue
    to the next channel.

  * Let `value1` and `value2` be the corresponding channel values in
    `color1` and `color2` respectively.

  * If `channel` has a polar angle value, set `value1` and `value2`
    respectively to the results of [hue interpolation][hue-method] with
    `value1` as `hue1`, `value2` as `hue2`, using the `hue-arc` method.

  * Set `channel` to the result of calculating `(value1 * weight) + (value2 *
    (1 - weight))`.

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

  * If the `channel` is not [missing] and `channel` is not a polar angle, set
    `channel`'s value to the result of multiplying `channel`'s value by
    `color`'s `alpha` value.

* Return the resulting `color`.

The same process can be run in reverse, to **un-premultiply** the channels of a
given `color`:

* If `color` has an `alpha` value of 1, 0, or `none`, return `color` unchanged.

* Otherwise, for each `channel` in `color`:

  * If the `channel` is not [missing] and `channel` is a polar angle, set
    `channel`'s value to the result of dividing `channel`'s value by `color`'s
    `alpha` value.

* Return the resulting `color`.

#### Hue Interpolation

> When interpolating between polar-angle channels, there are multiple
> "directions" the interpolation could move, following different logical rules.

This process accepts two angles (`hue1` and `hue2`), and returns both angles
adjusted according to an optional [hue interpolation method] `method`, which
defaults to `shorter`.

[hue interpolation method]: https://www.w3.org/TR/css-color-4/#hue-interpolation

The process for each hue interpolation method is defined in [CSS Color Level
4][color-4] (matched case-insensitively):

* [`shorter`](https://www.w3.org/TR/css-color-4/#shorter)

* [`longer`](https://www.w3.org/TR/css-color-4/#hue-longer)

* [`increasing`](https://www.w3.org/TR/css-color-4/#increasing)

* [`decreasing`](https://www.w3.org/TR/css-color-4/#decreasing)

### Scaling a Number

This algorithm takes a number `number`, a value `factor`, a number `max`, and an
optional number `min` which defaults to 0. It's written "scale `<number>` by
`<factor>` with a `max` of `<max>` and a `min` of `<min>`". It returns a number
with the same units as `number`.

> Note that this no longer assumes the original `number` is in a range of `min`
> to `max`. We now allow scaling up negative numbers, and scaling down numbers
> above the `max` value. The inverse operations return the `number` unchanged,
> since that's the asymptotic scale behavior approaching boundaries.

* If `factor` isn't a number with unit `%` between `-100%` and `100%`
  (inclusive), throw an error.

* If `factor > 0%`:

  * If `number > max`, return `number`.

  * Otherwise, return `number + (max - number) * factor / 100%`.

* Otherwise:

  * If `number < min`, return `number`.

  * Otherwise, return `number + (number - min) * factor / 100%`.

## Functions

### `adjust()`

```
adjust($color, $args...)
```

This function is also available as a global function named `adjust-color()`.

* If `$color` is not a color, throw an error.

* If `$args` contains any positional arguments, throw an error.

* Let `space` be the argument named `$space` in `$args`, or null if it doesn't
  have one.

* Let `color` be `$color` if `space` is null, and the result of [converting]
  `$color` to `space` otherwise.

* Let `alpha` be the value of `color`'s alpha channel, or the unquoted string
  `none` if it's missing.

* If the keyword argument `$alpha` is specified in `$args`:

  * If `color`'s alpha channel is missing, throw an error.

    > This is not the ideal solution for handling `none`, but we want to match
    > CSS relative color syntax if possible. Throwing an error for now means we
    > can adjust to match the CSS behavior once it is defined. See the following
    > issues for details:
    >
    > * [w3c/csswg-drafts#10151](https://github.com/w3c/csswg-drafts/issues/10151)
    > * [w3c/csswg-drafts#10211](https://github.com/w3c/csswg-drafts/issues/10211)

  * If `$alpha` isn't a number, throw an error.

  * Let `new-alpha` be the result of [percent-converting] `$alpha` with a
    `max` of 1.

  * Set `alpha` to the value of `new-alpha + alpha` clamped between 0 and 1.

* Let `channel-args` be the remaining keyword arguments in `$args`, not
  including `$space` or `$alpha` arguments.

* Let `channels` be a list of `color`'s channels.

* For each keyword `key` and value `adjust` in `channel-args`:

  * If `adjust` is not a number, throw an error.

  * If `key` is not the (case-sensitive) name of a channel in `channels`:

    * If `space` isn't null, throw an error.

    * If `color` is not a [legacy color], throw an error.

    * If `key` is one of `red`, `green`, or `blue`, set `space` to `rgb`.

    * Otherwise, if `key` is one of `saturation` or `lightness`, or if `key` is
      `hue` and the only other keywords in `channel-args` are `saturation` or
      `lightness`, set `space` to `hsl`.

      > This includes the case where `hue` is the only keyword.

    * Otherwise, if `key` is one of `whiteness` or `blackness`, or if `key` is
      `hue` and the only other keywords in `channel-args` are `whiteness` or
      `blackness`, set `space` to `hwb`.

    * Otherwise, throw an error.

    * Set `channels` to a list of the channels in `color` [converted] to
      `space`.

  * Let `channel` be the channel named `key` in `channels`.

  * If `channel` is [missing] and `space` isn't null, throw an error.

    > We leave an exemption for null spaces for backwards-compatibility with
    > Sass's behavior before powerless channel support was introduced.

  * Let `original` be `channel`'s value, or 0 if `channel` is missing.

    > This is not the ideal solution for handling `none`, but we want to match
    > CSS relative color syntax if possible. Throwing an error for now means we
    > can adjust to match the CSS behavior once it is defined. See the following
    > issues for details:
    >
    > * [w3c/csswg-drafts#10151](https://github.com/w3c/csswg-drafts/issues/10151)
    > * [w3c/csswg-drafts#10211](https://github.com/w3c/csswg-drafts/issues/10211)

  * If `adjust` has the unit `%`:

    * If `channel` is not [scalable], throw an error.

    * Set `adjust` to the result of [percent-converting] `adjust` with
      `channel`'s upper bound as `max`.

    * Otherwise, throw an error.

  * Let `value` to `original + adjust`.

    > Once percentage/number conversions have been normalized, this will throw
    > an error if `adjust` and `value` are not compatible.

  * If `channel`'s upper bound `bound` is clamped and `value > bound`:

    * Set `value` to `math.min(original, value)` if `original > bound` and
      `bound` otherwise.

  * Otherwise, if `channel`'s lower bound `bound` is clamped and `value < bound`:

    * Set `value` to `math.max(original, value)` if `original < bound` and
      `bound` otherwise.

  > This ensures that adjustment won't ever make a color go out-of-bounds, which
  > preserves the historical clamping behavior (which is particularly important
  > because negative saturation behaves *very* strangely) while still ensuring
  > that adjustment works rationally for channels that are already
  > out-of-bounds.

* Set `channels` to the result of [normalizing] `channels` in `space`.

* Let `new-color` be a color in `space` with channels `channels` and alpha
  `alpha`, or a missing alpha if `alpha` is a string.

* Return `color.to-space(new-color, color.space($color))`.

  > Call `color.to-space()` rather than [converting] `$color` directly so that
  > this won't cause powerless channels to become unexpectedly missing in legacy
  > color spaces, thereby unexpectedly reducing their browser compatibility.

[converted]: #converting-a-color
[scalable]: ../types/color.md#known-color-space

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

* Throw an error.

  > This error should indicate that the user should use the [`channel()`
  > function](#channel) instead.

### `blackness()`

```
blackness($color)
```

* Throw an error.

  > This error should indicate that the user should use the [`channel()`
  > function](#channel) instead.

### `blue()`

```
blue($color)
```

* Throw an error.

  > This error should indicate that the user should use the [`channel()`
  > function](#channel) instead.

### `change()`

```
change($color, $args...)
```

This function is also available as a global function named `change-color()`.

* If `$color` is not a color, throw an error.

* If `$args` contains any positional arguments, throw an error.

* Let `space` be the argument named `$space` in `$args`, or null if it doesn't
  have one.

* Let `color` be `$color` if `space` is null, and the result of [converting]
  `color` to `space` otherwise.

* Let `alpha` be `color`'s alpha property.

* If the keyword argument `$alpha` is specified in `$args`:

  * If `alpha` isn't a number or an unquoted string that's case-insensitively
    equal to "none", throw an error.

  * If `alpha` is a number, set `alpha` to the result of [percent-converting]
    `$alpha` with a `max` of 1, and clamping it between 0 and 1 (inclusive).

* Let `channel-args` be the remaining keyword arguments in `$args`, not
  including `$space` or `$alpha` arguments.

* Let `channels` be a list of `color`'s channels.

* For each keyword `key` and value `new` in `channel-args`:

  * If `new` is not a number or an unquoted string that's case-insensitively
    equal to "none", throw an error.

    > This basic restriction can be applied to all spaces. Further channel
    > restrictions are enforced by the normalization step for known spaces.

  * If `key` is not the (case-sensitive) name of a channel in `channels`:

    * If `space` isn't null, throw an error.

    * If `color` is not a [legacy color], throw an error.

    * If `key` is one of `red`, `green`, or `blue`, set `space` to `rgb`.

    * Otherwise, if `key` is one of `saturation` or `lightness`, or if `key` is
      `hue` and the only other keywords in `channel-args` are `saturation` or
      `lightness`, set `space` to `hsl`.

      > This includes the case where `hue` is the only keyword.

    * Otherwise, if `key` is one of `whiteness` or `blackness`, or if `key` is
      `hue` and the only other keywords in `channel-args` are `whiteness` or
      `blackness`, set `space` to `hwb`.

    * Otherwise, throw an error.

    * Set `channels` to be a list of the channels in `color` [converted] to
      `space`.

  * Set the corresponding `key` value in `channels` to `new`.

* Set `channels` to the result of [normalizing] `channels` in `space`.

* Let `new-color` be a color in `space` with channels `channels` and alpha
  `alpha`, or a missing alpha if `alpha` is a string.

* Return `color.to-space(new-color, color.space($color))`.

  > Call `color.to-space()` rather than [converting] `$color` directly so that
  > this won't cause powerless channels to become unexpectedly missing in legacy
  > color spaces, thereby unexpectedly reducing their browser compatibility.

### `channel()`

> Note that channel values are stored as specified, even if those values are
> out-of-gamut for the [known color space] used. Similarly, this color-channel
> inspection function may return out-of-gamut channel values.

```
channel($color, $channel, $space: null)
```

* If `$color` is not a color, throw an error.

* If `$channel` is not a quoted string, throw an error.

* If `$channel == "alpha"`, return `$color`'s `alpha` value.

* Let `color` be `$color` if `$space` is null, and the result of [converting]
  `$color` to `$space` otherwise.

* Let `channel` be the channel in `color` (case sensitively) named `$channel`.
  Throw an error if no such channel exists.

* Let `value` be `channel`'s value in `color`, or `0` if the channel's value
  is [missing].

* If `channel`'s associated unit is `%`, return `value * 100` divided by the
  maximum of `channel`'s gamut range with unit `%`.

* Otherwise, if `channel` has an associated unit, return `value` with that unit.

* Otherwise, return `value` as a unitless number.

### `complement()`

```
complement($color, $space: null)
```

This function is also available as a global function named `complement()`.

* Return the result of calling `color.adjust($color, $hue: 180deg, $space:
  $space)`.

  > This will throw an error if `$color` is not a color, if `space` doesn't have
  > a polar-angle hue channel, or if `$color`'s hue is missing (for a non-legacy
  > color or an explicit space).

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

> No space argument is provided, since the results should always be in gamut.

This function is also available as a global function named `grayscale()`.

* If `grayscale()` was called as a global function and `$color` is either a
  number or a [special number], return an unquoted string representing a CSS
  function call with name "invert" and argument `$color`.

* If `$color` is a [legacy color], return the result of `color.change($color,
  $saturation: 0, $space: hsl)`.

* Otherwise, return the result of `color.change($color, $chroma: 0, $space:
  oklch)`.

### `green()`

```
green($color)
```

* Throw an error.

  > This error should indicate that the user should use the [`channel()`
  > function](#channel) instead.

### `hue()`

```
hue($color)
```

* Throw an error.

  > This error should indicate that the user should use the [`channel()`
  > function](#channel) instead.

### `hwb()`

* ```
  hwb($channels)
  ```

  * Return the result of calling the global function `hwb($channels)`.

* ```
  hwb($hue, $whiteness, $blackness, $alpha: 1)
  ```

  * Return the result of calling the global function
    `hwb(list.slash($hue $whiteness $blackness, $alpha))`.

[parsing]: #parsing-color-components

### `ie-hex-str()`

```
ie-hex-str($color)
```

This function is also available as a global function named `ie-hex-str()`.

* If `$color` is not a color, throw an error.

* Let `rgb` be the result of [converting] and [gamut mapping] `$color` to `rgb`
  with method `local-minde`.

* Let `alpha` be `rgb`'s alpha value.

* Let `hex-alpha` be the hexadecimal representation of `alpha * 255`.

* Append `hex-alpha` to `hex-list`.

* Let `hex-list` be an empty list.

* For each `channel` in `rgba`'s channels, as numbers:

  * Let `hex-channel` be the hexadecimal representation of `channel`'s value.

  * Append `hex-channel` to `hex-list`.

* Return the result of concatenating `hex-list` into a string.

### `invert()`

```
invert($color,
  $weight: 100%,
  $space: null)
```

This function is also available as a global function named `invert()`.

* If `invert()` was called as a global function, `$color` is either a number or
  a [special number], `$weight == 100%`, and `$space` is null, return an
  unquoted string representing a CSS function call with name "invert" and
  argument `$color`.

* If `$color` is not a color, throw an error.

* If `$weight` is neither null nor a number with unit `%`, throw an error.

* If `$space` is null:

  * If `$color` is a [legacy color], let `color` be the result of [converting]
    `$color` to `rgb`.

  * Let `mix-space` be null.

    > This allows us to also enforce legacy behavior in the final weighted mix.

  * Otherwise, throw an error.

* Otherwise:

  * Let `color` be the result of [converting] `$color` to `$space`.

  * Let `mix-space` be the name of `color`'s space.

* If `$weight == 0%`, return `$color`.

* If `color`'s space is `hwb`:

  * Let `white` and `black` be the values of `color`'s channels of those names,
    or the unquoted string "none" if the respective channels are [missing].

  * Let `invert` be the result of calling `color.change(color.complement(color,
    $space: hwb), $white: black, $black: white)`.

* Otherwise:

  * Let `invert` be the value of `color`.

  * For each `channel` in `color`'s channels:

    * Let `value` be `channel`'s value.

    * If `channel` is a polar-angle `hue`:

      * If `channel` is [missing], throw an error.

        > This is not the ideal solution for handling `none`, but we want to
        > match CSS relative color syntax if possible. Throwing an error for now
        > means we can adjust to match the CSS behavior once it is defined. See
        > the following issues for details:
        >
        > * [w3c/csswg-drafts#10151](https://github.com/w3c/csswg-drafts/issues/10151)
        > * [w3c/csswg-drafts#10211](https://github.com/w3c/csswg-drafts/issues/10211)

      * Let `new` be `(value + 180deg) % 360deg`.

    * Otherwise, if `channel`'s name is either `chroma` or `saturation`:

      * Let `new` be `channel`.

    * Otherwise:

      * If `channel` is [missing], throw an error.

        > This is not the ideal solution for handling `none`, but we want to
        > match CSS relative color syntax if possible. Throwing an error for now
        > means we can adjust to match the CSS behavior once it is defined. See
        > the following issues for details:
        >
        > * [w3c/csswg-drafts#10151](https://github.com/w3c/csswg-drafts/issues/10151)
        > * [w3c/csswg-drafts#10211](https://github.com/w3c/csswg-drafts/issues/10211)

      * Let `min` and `max` be the lower and upper bounds of `channel`'s range.

      * Let `new` be `max - value` if `min == 0`, and `value * -1` otherwise.

    * Set the corresponding channel of `invert` to `new`.

* If `$weight == 100%`, let `result` be `invert`.

* Otherwise, let `result` be the result of calling `color.mix(invert, color,
  $weight, mix-space)`.

* Return `color.to-space(result, color.space($color))`.

  > Call `color.to-space()` rather than [converting] `$color` directly so that
  > this won't cause powerless channels to become unexpectedly missing in legacy
  > color spaces, thereby unexpectedly reducing their browser compatibility.

### `is-in-gamut()`

```
is-in-gamut($color, $space: null)
```

* If `$color` is not a color, throw an error.

* Let `color` be `$color` if `$space` is null, and the result of [converting]
  `$color` to `$space` otherwise.

* For all bounded channels in `color`'s space, if the associated channel value
  in `$color` is fuzzy greater-than the bounded maximum or fuzzy less-than the
  bounded minimum, return `false`.

* Otherwise, return `true`.

### `is-legacy()`

```
is-legacy($color)
```

* If `$color` is not a color, throw an error.

* Return `true` if `$color` is a [legacy color], or `false` otherwise.

### `is-missing()`

```
is-missing($color, $channel)
```

* If `$color` is not a color, throw an error.

* If `$channel` is not a quoted string, throw an error.

* If `$channel == "alpha"`, return `true` if `$color`'s alpha value is missing
  and `false` otherwise.

* Let `channel` be the channel in `$color` (case sensitively) named `$channel`.
  Throw an error if no such channel exists.

* Return `true` if `channel` is missing in `$color`, and `false` otherwise.

### `is-powerless()`

```
is-powerless($color, $channel, $space: null)
```

* If `$color` is not a color, throw an error.

* If `$channel` is not a quoted string, throw an error.

* Let `color` be `$color` if `$space` is null, and the result of [converting]
  `$color` to `$space` otherwise.

* Let `channel` be the channel in `color` (case sensitively) named `$channel`.
  Throw an error if no such channel exists.

* Return `true` if `channel` is [powerless] in `color`, or `false` otherwise.


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

* Throw an error.

  > This error should indicate that the user should use the [`channel()`
  > function](#channel) instead.

### `mix()`

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

### `opacify()`

```
opacify($color, $amount)
```

* Throw an error.

  > This error should indicate that the user should use the [`adjust()`
  > function](#adjust) instead.

### `red()`

```
red($color)
```

* Throw an error.

  > This error should indicate that the user should use the [`channel()`
  > function](#channel) instead.

### `same()`

> While it's possible to compare the [equality](../types/color.md#equality) of
> two colors, the result is always false when the two colors are in different
> color spaces. This function compares colors across color spaces, to determine
> if they are equal after being converted into the same space.

```
same($color1, $color2)
```

* Let `color1` and `color2` be `$color1` and `$color2`, respectively, with any
  [missing] components replaced with 0.

  > This is necessary to ensure that `same()` actually matches visual rendering.
  > For example, `rgb(100 200 0)` and `rgb(100 200 none)` are rendered
  > identically, but the former converts to `color(xyz 0.2590878471 0.4401656621
  > 0.0713080481)` while the latter converts to `color(xyz 0.2590878471
  > 0.4401656621 none)`, which are *not* equivalent if we convert missing
  > channels to zero after XYZ conversion.

* Let `xyz1` and `xyz2` be the result of [converting] `color1` and `color2` to
  `xyz`, respectively.

* Return `xyz1 == xyz2`.

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

* Throw an error.

  > This error should indicate that the user should use the [`channel()`
  > function](#adjust) instead.

### `scale()`
```
scale($color, $args...)
```

This function is also available as a global function named `scale-color()`.

* If `$color` is not a color, throw an error.

* If `$args` contains any positional arguments, throw an error.

* Let `space` be the argument named `$space` in `$args`, or null if it doesn't
  have one.

* Let `color` be `$color` if `space` is null, and the result of [converting]
  `$color` to `space` otherwise.

* Let `alpha` be the value of `color`'s alpha channel, or the unquoted string
  `none` if it's missing.

* If the keyword argument `$alpha` is specified in `$args`:

  * If `color`'s alpha channel is missing, throw an error.

    > This is not the ideal solution for handling `none`, but we want to match
    > CSS relative color syntax if possible. Throwing an error for now means we
    > can adjust to match the CSS behavior once it is defined. See the following
    > issues for details:
    >
    > * [w3c/csswg-drafts#10151](https://github.com/w3c/csswg-drafts/issues/10151)
    > * [w3c/csswg-drafts#10211](https://github.com/w3c/csswg-drafts/issues/10211)

  * If `$alpha` isn't a number, throw an error.

  * Set `alpha` to the result of [scaling] `alpha` by `$alpha` with `max` 1.

* Let `channel-args` be the remaining keyword arguments in `$args`, not
  including `$space` or `$alpha` arguments.

* Let `channels` be a list of `color`'s channels.

* For each keyword `key` and value `factor` in `channel-args`:

  * If `key` is not the (case-sensitive) name of a [scalable] channel in
    `channels`:

    * If `space` isn't null, throw an error.

    * If `color` is not a [legacy color], throw an error.

    * If `key` is one of `red`, `green`, or `blue`, set `space` to `rgb`.

    * Otherwise, if `key` is one of `saturation` or `lightness`, set `space` to
      `hsl`.

    * Otherwise, if `key` is one of `whiteness` or `blackness`, set `space` to
      `hwb`.

    * Otherwise, throw an error.

    * Set `channels` to a list of the channels in `color` [converted] to
      `space`.

  * Let `channel` be the channel named `key` in `channels`.

  * If `channel` is missing in `color`, throw an error.

    > This is not the ideal solution for handling `none`, but we want to match
    > CSS relative color syntax if possible. Throwing an error for now means we
    > can adjust to match the CSS behavior once it is defined. See the following
    > issues for details:
    >
    > * [w3c/csswg-drafts#10151](https://github.com/w3c/csswg-drafts/issues/10151)
    > * [w3c/csswg-drafts#10211](https://github.com/w3c/csswg-drafts/issues/10211)

  * Let `min` and `max` be the lower and upper bounds of `channel`,
    respectively.

  * Set the corresponding `channel` in `channels` to the result of [scaling]
    `channel` by `factor` with min `min` and max `max`.

* Set `channels` be the result of [normalizing] `channels` in `space`.

* Let `new-color` be a color in `space` with channels `channels` and alpha
  `alpha`, or a missing alpha if `alpha` is a string.

* Return `color.to-space(new-color, color.space($color))`.

  > Call `color.to-space()` rather than [converting] `$color` directly so that
  > this won't cause powerless channels to become unexpectedly missing in legacy
  > color spaces, thereby unexpectedly reducing their browser compatibility.

[scaling]: #scaling-a-number

### `space()`

```
space($color)
```

* If `$color` is not a color, throw an error.

* Return the name of `$color`s color space as an unquoted lowercase string.

### `to-gamut()`

```
to-gamut($color, $space: null, $method: null)
```

* If `$color` is not a color, throw an error.

* If `$method` is not an unquoted string whose value is (case sensitively)
  either `local-minde` or `clip`, throw an error.

* Otherwise, let `destination` be the result of [looking up a known color space]
  named `$space` if it's not null, or `$color`'s space otherwise.

* Return the result of [gamut mapping] `$color` with destination `destination`
  and method `$method`.

[gamut mapping]: #gamut-mapping

### `to-space()`

```
to-space($color, $space)
```

* If `$color` is not a color, throw an error.

* Let `converted` be the result of [converting] `$color` to `$space`.

* If `converted` is a [legacy color]:

  * For each `component` in the channels and alpha value of `converted`, if
    `component` is [missing], set `component` to `0`.

* Return `converted`.

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

* Throw an error.

  > This error should indicate that the user should use the [`channel()`
  > function](#channel) instead.
