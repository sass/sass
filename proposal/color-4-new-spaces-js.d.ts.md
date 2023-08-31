# CSS Color Level 4, New Color Spaces JavaScript API

*([Issue](https://github.com/sass/sass/issues/2831))*

This proposal updates Sass's JavaScript (JS) API to match the [color spaces
proposal].

[color spaces proposal]: ./color-4-new-spaces.md

## Table of Contents

* [API](#api)
* [Types](#types)
  * [Color Space Definitions](#color-space-definitions)
  * [New Color Functions](#new-color-functions)
    * [`space`](#space)
    * [`channels`](#channels)
    * [`channelsOrNull`](#channelsornull)
    * [`isLegacy`](#islegacy)
    * [`isInGamut`](#isingamut)
    * [`channel`](#channel)
    * [`isChannelMissing`](#ischannelmissing)
    * [`isChannelPowerless`](#ischannelpowerless)
    * [`toSpace`](#tospace)
    * [`toGamut`](#togamut)
    * [`changeChannels`](#changechannels)
  * [`interpolate()`](#interpolate)
  * [New Constructors](#new-constructors)
    * [HSL Channel Constructor](#hsl-channel-constructor)
    * [HWB Channel Constructor](#hwb-channel-constructor)
    * [LAB Channel Constructor](#lab-channel-constructor)
    * [LCH Channel Constructor](#lch-channel-constructor)
    * [RGB Channel Constructor](#rgb-channel-constructor)
    * [XYZ Channel Constructor](#xyz-channel-constructor)
  * [Deprecations](#deprecations)
* [Embedded Protocol](#embedded-protocol)
  * [SassColor](#sasscolor)
  * [Removed SassScript values](#removed-sassscript-values)

## API

```ts
import {Value} from '../spec/js-api/value';
```

## Types

### Color Space Definitions

```ts

export type ColorSpaceHSL = 'hsl';

export type ChannelNameHSL = 'hue' | 'saturation' | 'lightness';

export type ColorSpaceHWB = 'hwb';

export type ChannelNameHWB = 'hue' | 'whiteness' | 'blackness';

export type ColorSpaceLAB = 'lab' | 'oklab';

export type ChannelNameLAB = 'lightness' | 'a' | 'b';

export type ColorSpaceLCH = 'lch' | 'oklch';

export type ChannelNameLCH = 'lightness' | 'chroma' | 'hue';

export type ColorSpaceRGB =
  | 'a98-rgb'
  | 'display-p3'
  | 'prophoto-rgb'
  | 'rgb'
  | 'srgb'
  | 'srgb-linear';

export type ChannelNameRGB = 'red' | 'green' | 'blue';

export type ColorSpaceXYZ = 'xyz' | 'xyz-d50' | 'xyz-d65';

export type ChannelNameXYZ = 'x' | 'y' | 'z';

export type ChannelName =
  | ChannelNameHSL
  | ChannelNameHWB
  | ChannelNameLAB
  | ChannelNameLCH
  | ChannelNameRGB
  | ChannelNameXYZ;

export type KnownColorSpace =
  | ColorSpaceHSL
  | ColorSpaceHWB
  | ColorSpaceLAB
  | ColorSpaceLCH
  | ColorSpaceRGB
  | ColorSpaceXYZ;

export type PolarColorSpace = ColorSpaceHSL | ColorSpaceHWB | ColorSpaceLCH;

export type RectangularColorSpace = Omit<KnownColorSpace, PolarColorSpace>;

export type HueInterpolationMethod =
  | 'decreasing'
  | 'increasing'
  | 'longer'
  | 'shorter';

export type ChannelValue = number | null;
```

### New Color Functions

```ts
export class SassColor extends Value {
```

#### `space`

Returns the name of [`internal`]'s space.

```ts
get space(): KnownColorSpace;
```

#### `channels`

Returns an array of channel values for [`internal`], with missing channels
converted to `0`.

```ts
get channels(): number[];
```

#### `channelsOrNull`

Returns an array of channel values for [`internal`], with missing channels
converted to `null`.

```ts
get channelsOrNull(): ChannelValue[];
```

#### `isLegacy`

Returns whether [`internal`] is in a [legacy color space] (`rgb`, `hsl`, or
`hwb`).

```ts
get isLegacy(): boolean;
```

[legacy color space]: ./color-4-new-spaces.md#legacy-color

#### `isInGamut`

Returns whether [`internal`] is [in-gamut] for its color space (as opposed to
having one or more of its channels out of bounds, like `rgb(300 0 0)`).

```ts
get isInGamut(): boolean;
```

[in-gamut]: ./color-4-new-spaces.md#coloris-in-gamut

#### `channel`

Returns the value of the given `channel` in [`internal`]. If `space` is set, it
will first [convert] to that `space`.

```ts
channel(channel: ChannelName): ChannelValue;
channel(
  channel: ChannelNameHSL,
  options?: {space: ColorSpaceHSL}
): ChannelValue;
channel(
  channel: ChannelNameHWB,
  options?: {space: ColorSpaceHWB}
): ChannelValue;
channel(
  channel: ChannelNameLAB,
  options?: {space: ColorSpaceLAB}
): ChannelValue;
channel(
  channel: ChannelNameLCH,
  options?: {space: ColorSpaceLCH}
): ChannelValue;
channel(
  channel: ChannelNameRGB,
  options?: {space: ColorSpaceRGB}
): ChannelValue;
channel(
  channel: ChannelNameXYZ,
  options?: {space: ColorSpaceXYZ}
): ChannelValue;
```

[convert]: ./color-4-new-spaces.md#converting-a-color

#### `isChannelMissing`

Returns whether the given `channel` of [`internal`] is missing. Missing channels
can be explicitly specified using the special value `none` and can appear
automatically when [toSpace()] returns a color with a powerless channel.

```ts
isChannelMissing(channel: ChannelName): boolean;
```

[toSpace()]: #tospace

#### `isChannelPowerless`

Returns whether the given `channel` of [`internal`] is powerless in `space`,
defaulting to its own color space. A channel is "powerless" if its value doesn't
affect the way the color is displayed, such as hue for a color with 0 chroma.
Throws an error if `channel` is not a channel in `space`.

```ts
isChannelPowerless(channel: ChannelName): boolean;
isChannelPowerless(
  channel: ChannelNameHSL,
  options?: {space: ColorSpaceHSL}
): boolean;
isChannelPowerless(
  channel: ChannelNameHWB,
  options?: {space: ColorSpaceHWB}
): boolean;
isChannelPowerless(
  channel: ChannelNameLAB,
  options?: {space: ColorSpaceLAB}
): boolean;
isChannelPowerless(
  channel: ChannelNameLCH,
  options?: {space: ColorSpaceLCH}
): boolean;
isChannelPowerless(
  channel: ChannelNameRGB,
  options?: {space: ColorSpaceRGB}
): boolean;
isChannelPowerless(
  channel: ChannelNameXYZ,
  options?: {space: ColorSpaceXYZ}
): boolean;
```

#### `toSpace`

Returns the result of converting [`internal`] to `space` as a new SassColor.

```ts
toSpace(space: KnownColorSpace): SassColor;
```

#### `toGamut`

Returns [`internal`] constrained to its space's gamut as a new SassColor. This
is generally not recommended since even older browsers will display out-of-gamut
colors as best they can, but it may be necessary in some cases.

```ts
toGamut(): SassColor;
```

#### `changeChannels`

Returns a new SassColor as the result of changing some of [`internal`]'s
channels. The `space` value defaults to the `space` of [`internal`], and any
combination of channels in that space may be changed. Throws an error if any
`channel` is not present in `space`.

* If `space` is not defined, let `space` be the value of [`color.space(internal)`].

* Let `arguments` be the key value pairs in `channels` as keyword arguments.

* Return the value of the result of [`color.change(internal, ...arguments)`].

[`color.space(internal)`]: #space

[`color.change(internal, ...arguments)`]: ./color-4-new-spaces.md#colorchange

```ts
changeChannels(
  options: {
    [key in ChannelName]?: ChannelValue;
  } & {alpha?: number}
): SassColor;

changeChannels(
  options: {
    [key in ChannelNameHSL]?: ChannelValue;
  } & {
    alpha?: number;
    space: ColorSpaceHSL;
  }
): SassColor;

changeChannels(
  options: {
    [key in ChannelNameHWB]?: ChannelValue;
  } & {
    alpha?: number;
    space: ColorSpaceHWB;
  }
): SassColor;

changeChannels(
  options: {
    [key in ChannelNameLAB]?: ChannelValue;
  } & {
    alpha?: number;
    space: ColorSpaceLAB;
  }
): SassColor;

changeChannels(
  options: {
    [key in ChannelNameLCH]?: ChannelValue;
  } & {
    alpha?: number;
    space: ColorSpaceLCH;
  }
): SassColor;

changeChannels(
  options: {
    [key in ChannelNameRGB]?: ChannelValue;
  } & {
    alpha?: number;
    space: ColorSpaceRGB;
  }
): SassColor;

changeChannels(
  options: {
    [key in ChannelNameXYZ]?: ChannelValue;
  } & {
    alpha?: number;
    space: ColorSpaceXYZ;
  }
): SassColor;
```

### `interpolate()`

Returns a new SassColor with the result of mixing [`internal`] with `color2`.

It accepts an optional float `weight`, which defaults to 0.5. Lower values will
appear closer to [`internal`] and higher values will appear closer to `color2`.

If `space` is set, interpolation will happen in that space. Otherwise it will
happen in the color space for [`internal`].

If `space` (or the color space of [`internal`] if no `space` argument is
provided) is a PolarColorSpace (a color space with a polar angle `hue` channel),
a `method` may be provided, which defaults to `shorter`.

```ts
interpolate(options: {
  color2: SassColor;
  weight?: number;
  space?: RectangularColorSpace;
}): SassColor;

interpolate(options: {
  color2: SassColor;
  weight?: number;
  space?: PolarColorSpace;
  method?: HueInterpolationMethod;
}): SassColor;
```

[`internal`]: ../spec/js-api/value/color.d.ts.md#internal

### New Constructors

Because the value of each channel may be a string, number, or `null`, this
algorithm checks if an option with a key exists, and not evaluating whether the
key's value is truthy.

* If `options.space` is not set, follow the legacy procedure for [construction].

* Let `space` be a string with the value of `options.space`.

* To calulate the channels:

  * If `options.red` exists:

    * Let `channel1` be the value of `options.red`.

    * Let `channel2` be the value of `options.green`.

    * Let `channel3` be the value of `options.blue`.

  * If `options.saturation` exists:

    * Let `channel1` be the value of `options.hue`.

    * Let `channel2` be the value of `options.saturation`.

    * Let `channel3` be the value of `options.lightness`.

  * If `options.whiteness` exists:

    * Let `channel1` be the value of `options.hue`.

    * Let `channel2` be the value of `options.whiteness`.

    * Let `channel3` be the value of `options.blackness`.

  * If `options.x` exists:

    * Let `channel1` be the value of `options.x`.

    * Let `channel2` be the value of `options.y`.

    * Let `channel3` be the value of `options.z`.

  * If `options.chroma` exists:

    * Let `channel1` be the value of `options.lightness`.

    * Let `channel2` be the value of `options.chroma`.

    * Let `channel3` be the value of `options.hue`.

  * If `options.a` exists:

    * Let `channel1` be the value of `options.lightness`.

    * Let `channel2` be the value of `options.a`.

    * Let `channel3` be the value of `options.b`.

* If `options.alpha` is set:

  * Let `alpha` be a Sass number with a value of `options.alpha`

  * Set [`internal`] to the result of
    [`color(channel1 channel2 channel3 / alpha)`]

* Otherwise, set [`internal`] to the result of
  [`color(channel1 channel2 channel3 )`]

[`color(channel1 channel2 channel3 / alpha)`]: ./color-4-new-spaces.md#color-1
[`color(channel1 channel2 channel3 )`]: ./color-4-new-spaces.md#color-1

[construction]: ../spec/js-api/value/color.d.ts.md#constructor

#### HSL Channel Constructor

Create a new SassColor in the `hsl` color space. `space` is optional to not
break the legacy constructor, but allowed for constructor consistency.

```ts
constructor(options: {
  hue: ChannelValue;
  saturation: ChannelValue;
  lightness: ChannelValue;
  alpha?: number;
  space?: ColorSpaceHSL;
});
```

#### HWB Channel Constructor

Create a new SassColor in the `hwb` color space. `space` is optional to not
break the legacy constructor, but allowed for constructor consistency.

```ts
constructor(options: {
  hue: ChannelValue;
  whiteness: ChannelValue;
  blackness: ChannelValue;
  alpha?: number;
  space?: ColorSpaceHWB;
});
```

#### LAB Channel Constructor

Create a new SassColor in a color space with LAB channels -- `lab` and `oklab`.
`space` is required as there is no legacy space to default to.

```ts
constructor(options: {
  lightness: ChannelValue;
  a: ChannelValue;
  b: ChannelValue;
  alpha?: number;
  space: ColorSpaceLAB;
});
```

#### LCH Channel Constructor

Create a new SassColor in a color space with LCH channels -- `lch` and `oklch`.
`space` is required as there is no legacy space to default to.

```ts
constructor(options: {
  lightness: ChannelValue;
  chroma: ChannelValue;
  hue: ChannelValue;
  alpha?: number;
  space: ColorSpaceLCH;
});
```

#### RGB Channel Constructor

Create a new SassColor in a color space with RGB channels -- `rgb`, `srgb`,
`srgb-linear`, `display-p3`, `a98-rgb`, and `prophoto-rgb`. If `space` is set,
it will create a new SassColor in that space, and it will default to the legacy
`rgb` space.

```ts
constructor(options: {
  red: ChannelValue;
  green: ChannelValue;
  blue: ChannelValue;
  alpha?: number;
  space?: ColorSpaceRGB;
});
```

#### XYZ Channel Constructor

Create a new SassColor in a color space with XYZ channels -- `xyz`, `xyz-d50`,
and `xyz-d65`. `space` is required as there is no legacy space to default to.

```ts
constructor(options: {
  x: ChannelValue;
  y: ChannelValue;
  z: ChannelValue;
  alpha?: number;
  space: ColorSpaceXYZ;
});
```

```ts
}
```

### Deprecations

A number of SassColor getters only make sense for legacy colors, and so are
being deprecated for `channel`.

* `red`
* `green`
* `blue`
* `hue`
* `saturation`
* `lightness`
* `whiteness`
* `blackness`
* `alpha`

In addition, `change` is deprecated in favor of `changeChannels`.

## Embedded Protocol

This introduces a breaking change in the Embedded Protocol, as it removes the
legacy SassScript values.

### SassColor

```proto
message SassColor {
  // The name of a known color space.
  string space = 1;

  // The value of the first channel associated with `space`.
  double Channel1 = 2;

  // The value of the second channel associated with `space`.
  double Channel2 = 3;

  // The value of the third channel associated with `space`.
  double Channel3 = 4;

  // The color's alpha channel. Mandatory. Must be between 0 and 1,
  // inclusive.
  double alpha = 5;
}
```

### Removed SassScript values

The `RgbColor`, `HslColor` and `HwbColor` SassScript values will be removed from
the Embedded Protocol.
