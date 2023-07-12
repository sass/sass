# CSS Color Level 4, New Color Spaces JavaScript API

*([Issue](https://github.com/sass/sass/issues/2831))*

This proposal updates Sass's JavaScript (JS) API to match the [color spaces
proposal].

[color spaces proposal]: ./color-4-new-spaces.md

## Table of Contents

## API

```ts
import {SassColor} from '../spec/js-api/value';
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

type ChannelNamesRGB = 'red' | 'green' | 'yellow';

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

```
### New Color Functions

#### `space`

Returns the value of the result of [`space(internal)`].

[`space(internal)`]: ../../color-4-new-spaces.md#colorspace-1

```ts
get space(): string;
```

#### `toSpace`

Returns the value of the result of [`to-space(internal, $space)`].

[`to-space(internal)`]: ../../color-4-new-spaces.md#colorto-space

```ts
toSpace(space: KnownColorSpace): string;
```

#### `isLegacy`

Returns the value of the result of [`is-legacy(internal)`].

[`is-legacy(internal)`]: ../../color-4-new-spaces.md#coloris-legacy

```ts
get isLegacy(): boolean;
```

#### `isPowerless`

Returns the value of the result of [`is-powerless()]. `space` defaults to the
value of `space`. Throws an error if
`channel` is not a channel in `space`.

[`is-powerless(internal)`]: ../../color-4-new-spaces.md#coloris-powerless

```ts
isPowerless(options: {channel: ChannelNames, space?: KnownColorSpace})
```

#### `channel`

Returns the result of [`channel()`]. By default, it
only supports channels that are available in the color's own space, but you can
pass the `$space` parameter to return the value of the channel after converting
to the given space.

```ts
channel(options: {channel: ChannelNames, space?: KnownColorSpace})
```

## New Constructors

If space is set, create in that space. Throw errors if not valid. Otherwise
use the legacy algorithm.

```ts
constructor(options: {
  red: number;
  green: number;
  blue: number;
  alpha?: number;
}, space: ColorSpaceRGB);

constructor(options: {
  hue: number;
  whiteness: number;
  blackness: number;
  alpha?: number;
}, space: ColorSpaceHWB);

constructor(options: {
  hue: number;
  saturation: number;
  lightness: number;
  alpha?: number;
}, space: ColorSpaceHSL);

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
```