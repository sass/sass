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
    * [RGB Channel Constructor](#rgb-channel-constructor)
    * [HWB Channel Constructor](#hwb-channel-constructor)
    * [HSL Channel Constructor](#hsl-channel-constructor)
    * [XYZ Channel Constructor](#xyz-channel-constructor)
    * [LCH Channel Constructor](#lch-channel-constructor)
    * [LAB Channel Constructor](#lab-channel-constructor)

## API

```ts
import {Value} from '../spec/js-api/value';
```

## Types

### Color Space Definitions
```ts

type ColorSpaceRGB =
  | 'rgb'
  | 'srgb'
  | 'srgb-linear'
  | 'display-p3'
  | 'a98-rgb'
  | 'prophoto-rgb';

type ChannelNamesRGB = 'red' | 'green' | 'blue';

type ColorSpaceHWB = 'hwb';

type ChannelNamesHWB = 'hue' | 'whiteness' | 'blackness';

type ColorSpaceHSL = 'hsl';

type ChannelNamesHSL = 'hue' | 'saturation' | 'lightness';

type ColorSpaceXYZ = 'xyz' | 'xyz-d50' | 'xyz-d65';

type ChannelNamesXYZ = 'x' | 'y' | 'z';

type ColorSpaceLCH = 'lch' | 'oklch';

type ChannelNamesLCH = 'lightness' | 'chroma' | 'hue';

type ColorSpaceLAB = 'lab' | 'oklab';

type ChannelNamesLAB = 'lightness' | 'a' | 'b';

type ChannelNames =
  | ChannelNamesRGB
  | ChannelNamesHWB
  | ChannelNamesHSL
  | ChannelNamesXYZ
  | ChannelNamesLCH
  | ChannelNamesLAB;

type KnownColorSpace =
  | ColorSpaceRGB
  | ColorSpaceHWB
  | ColorSpaceHSL
  | ColorSpaceXYZ
  | ColorSpaceLCH
  | ColorSpaceLAB;

type PolarColorSpace =
  | ColorSpaceHWB
  | ColorSpaceHSL
  | ColorSpaceLCH
  | ColorSpaceLAB;

type RectangularColorSpace = Omit<KnownColorSpace, PolarColorSpace>;

type HueInterpolationMethod =
  | 'shorter'
  | 'longer'
  | 'decreasing'
  | 'increasing';

type ChannelValues = string | number;
type ChannelValuesOrNull = string | number | null;
```

### New Color Functions

```ts
export class SassColor extends Value {
```

#### `space`

Returns the name of the color's space.

```ts
get space(): KnownColorSpace;
```

#### `channels`

Returns an array of channel values, with missing channels converted to `0`.

```ts
get channels(): ChannelValues;
```

#### `channelsOrNull`

Returns an array of channel values, with missing channels converted to `null`.

```ts
get channelsOrNull(): ChannelValuesOrNull;
```

#### `isLegacy`

Returns whether `color` is in a legacy color space (`rgb`, `hsl`, or `hwb`).

```ts
get isLegacy(): boolean;
```

#### `isInGamut`

Returns whether `color` is in-gamut for its color space (as opposed to having
one or more of its channels out of bounds, like `rgb(300 0 0)`).

```ts
get isInGamut(): boolean;
```

#### `channel`

Returns the value of the given `channel` in `color`, after converting it to
`space` if necessary. It should be used instead of the old channel-specific
functions such as `color.red()` and `color.hue()`.


```ts
channel(options: {
  channel: ChannelNames;
  space?: KnownColorSpace;
}): ChannelValuesOrNull;
```

#### `isChannelMissing`

Returns whether the given `channel` of `color` is missing. Missing channels can
be explicitly specified using the special value `none` and can appear
automatically when [toSpace()] returns a color with a powerless channel. 


```ts
isChannelMissing(options: {channel: ChannelNames}): boolean;
```

[toSpace()]: #toSpace

#### `isChannelPowerless`

Returns whether the given `channel` of `color` is powerless in `space`,
defaulting to its own color space. A channel is "powerless" if its value doesn't
affect the way the color is displayed, such as hue for a color with 0 chroma.
Throws an error if `channel` is not a channel in `space`.

```ts
isChannelPowerless(options: {
  channel: ChannelNames;
  space?: KnownColorSpace;
}): boolean;
```

#### `toSpace`

Returns the result of converting `color` to `space` as a new SassColor. 

```ts
toSpace(space: KnownColorSpace): SassColor;
```

#### `toGamut`

Returns `color` constrained to its space's gamut as a new SassColor. This is
generally not recommended since even older browsers will display out-of-gamut
colors as best they can, but it may be necessary in some cases.

```ts
toGamut(): SassColor;
```

#### `changeChannels`

Returns a new SassColor as the result of changing some of color's channels. The
`space` value defaults to the `space` of `color`, and any combination of
channels in that space may be changed. Throws an error if any `channel` is not
present in `space`.


```ts
changeChannels(
  channels: {
    [key in ChannelNamesRGB]?: ChannelValuesOrNull;
  },
  space?: ColorSpaceRGB
): SassColor;

changeChannels(
  channels: {
    [key in ChannelNamesHWB]?: ChannelValuesOrNull;
  },
  space?: ColorSpaceHWB
): SassColor;

changeChannels(
  channels: {
    [key in ChannelNamesHSL]?: ChannelValuesOrNull;
  },
  space?: ColorSpaceHSL
): SassColor;

changeChannels(
  channels: {
    [key in ChannelNamesXYZ]?: ChannelValuesOrNull;
  },
  space?: ColorSpaceXYZ
): SassColor;

changeChannels(
  channels: {
    [key in ChannelNamesLCH]?: ChannelValuesOrNull;
  },
  space?: ColorSpaceLCH
): SassColor;

changeChannels(
  channels: {
    [key in ChannelNamesLAB]?: ChannelValuesOrNull;
  },
  space?: ColorSpaceLAB
): SassColor;
```

### `interpolate()`

Returns a new SassColor with the result of mixing `this` with `color2`. 

It accepts an optional float `weight`, which defaults to 0.5. Lower values will
appear closer to `this` and higher values will appear closer to `color2`. 

If `space` is set, interpolation will happen in that space. Otherwise it will
happen in the color space for `this`.

If `space` (or the color space of `this` if no `space` argument is provided) is a PolarColorSpace (a color space with a polar angle `hue`
channel), a `method` may be provided, which defaults to `shorter`. 

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


### New Constructors

#### RGB Channel Constructor

Create a new SassColor in a color space with RGB channels- `rgb`, `srgb`,
`srgb-linear`, `display-p3`, `a98-rgb`, and `prophoto-rgb`. If `space` is set,
it will create a new SassColor in that space, and it will default to the legacy
`rgb` space.

```ts
constructor(
  options: {
    red: number;
    green: number;
    blue: number;
    alpha?: number;
  },
  space?: ColorSpaceRGB
);
```

#### HWB Channel Constructor

Create a new SassColor in the `hwb` color space. `space` is optional to not
break the legacy constuctor, but allowed for constructor consistency.

```ts
constructor(
  options: {
    hue: number;
    whiteness: number;
    blackness: number;
    alpha?: number;
  },
  space?: ColorSpaceHWB
);
```

#### HSL Channel Constructor

Create a new SassColor in the `hsl` color space. `space` is optional to not
break the legacy constuctor, but allowed for constructor consistency.

```ts
constructor(
  options: {
    hue: number;
    saturation: number;
    lightness: number;
    alpha?: number;
  },
  space?: ColorSpaceHSL
);
```

#### XYZ Channel Constructor

Create a new SassColor in a color space with XYZ channels- `xyz`, `xyz-d50`, and
`xyz-d65'. `space` is required as there is no legacy space to default to.

```ts
constructor(
  options: {
    x: number;
    y: number;
    z: number;
    alpha?: number;
  },
  space: ColorSpaceXYZ
);
```

#### LCH Channel Constructor

Create a new SassColor in a color space with LCH channels- `lch` and `oklch`.
`space` is required as there is no legacy space to default to.

```ts
constructor(
  options: {
    lightness: number;
    chroma: number;
    hue: number;
    alpha?: number;
  },
  space: ColorSpaceLCH
);
```

#### LAB Channel Constructor

Create a new SassColor in a color space with LAB channels- `lab` and `oklab`.
`space` is required as there is no legacy space to default to.

```ts
constructor(
  options: {
    lightness: number;
    a: number;
    b: number;
    alpha?: number;
  },
  space: ColorSpaceLAB
);
```

```ts
}
```
