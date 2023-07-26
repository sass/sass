# CSS Color Level 4, New Color Spaces JavaScript API

*([Issue](https://github.com/sass/sass/issues/2831))*

This proposal updates Sass's JavaScript (JS) API to match the [color spaces
proposal].

[color spaces proposal]: ./color-4-new-spaces.md

## Table of Contents

## API

```ts
import { Value } from '../spec/js-api/value';
```

## Types

### Known Color Space
```ts
type KnownColorSpace = 
    'rgb' |
    'hwb' |
    'hsl' |
    'srgb' |
    'srgb-linear' |
    'display-p3' |
    'a98-rgb' |
    'prophoto-rgb' |
    'xyz' |
    'xyz-d50' |
    'xyz-d65' |
    'lab' |
    'lch' |
    'oklab' |
    'oklch';

type ColorSpaceRGB = 
    'rgb' |
    'srgb' |
    'srgb-linear' |
    'display-p3' |
    'a98-rgb' |
    'prophoto-rgb';

type ChannelNamesRGB = 'red' | 'green' | 'blue';

type ColorSpaceHWB = 'hwb';

type ChannelNamesHWB = 'hue' | 'whiteness' | 'blackness';

type ColorSpaceHSL = 'hsl';

type ChannelNamesHSL = 'hue' | 'saturation' | 'lightness';

type ColorSpaceXYZ = 
    'xyz' |
    'xyz-d50' |
    'xyz-d65';

type ChannelNamesXYZ = 'x' | 'y' | 'z';

type ColorSpaceLCH = 
    'lch' |
    'oklch';

type ChannelNamesLCH = 'lightness' | 'chroma' | 'hue';

type ColorSpaceLAB = 
    'lab' |
    'oklab';

type ChannelNamesLAB = 'lightness' | 'a' | 'b';

type ChannelNames = 
    ChannelNamesRGB |
    ChannelNamesHWB |
    ChannelNamesHSL |
    ChannelNamesXYZ |
    ChannelNamesLCH |
    ChannelNamesLAB;

type ChannelValues = string | number;
type ChannelValuesOrNull = string | number | null;

```
### New Color Functions

```ts
export class SassColor extends Value{
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
get channels(): ChannelValuesOrNull;
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
get isInGamut():boolean;
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
isChannelPowerless(options: {channel: ChannelNames, space?: KnownColorSpace}): boolean;
```

#### `channel`

Returns the value of the given `channel` in `color`, after converting it to
`space` if necessary. It should be used instead of the old channel-specific
functions such as `color.red()` and `color.hue()`.


```ts
channel(options: {channel: ChannelNames, space?: KnownColorSpace}): ChannelValueOrNull;
```

#### `toSpace`

Returns the result of converting `color` to `space`. 

```ts
toSpace(space: KnownColorSpace): SassColor;
```

## New Constructors

If `space` is set, create a new SassColor in that space. Throws errors if
channels are not valid. Otherwise use the legacy algorithm.

```ts
constructor(options: {
  red: number;
  green: number;
  blue: number;
  alpha?: number;
}, space?: ColorSpaceRGB);

constructor(options: {
  hue: number;
  whiteness: number;
  blackness: number;
  alpha?: number;
}, space?: ColorSpaceHWB);

constructor(options: {
  hue: number;
  saturation: number;
  lightness: number;
  alpha?: number;
}, space?: ColorSpaceHSL);

constructor(options: {
  x: number;
  y: number;
  z: number;
  alpha?: number;
}, space: ColorSpaceXYZ);

constructor(options: {
  lightness: number;
  chroma: number;
  hue: number;
  alpha?: number;
}, space: ColorSpaceLCH);

constructor(options: {
  lightness: number;
  a: number;
  b: number;
  alpha?: number;
}, space: ColorSpaceLAB);

}
```