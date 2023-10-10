# Color API

```ts
import {Value} from './index';
```

## Table of Contents

* [Types](#types)
  * [`ColorSpaceHSL`](#colorspacehsl)
  * [`ChannelNameHSL`](#channelnamehsl)
  * [`ColorSpaceHWB`](#colorspacehwb)
  * [`ChannelNameHWB`](#channelnamehwb)
  * [`ColorSpaceLab`](#colorspacelab)
  * [`ChannelNameLab`](#channelnamelab)
  * [`ColorSpaceLCH`](#colorspacelch)
  * [`ChannelNameLCH`](#channelnamelch)
  * [`ColorSpaceRGB`](#colorspacergb)
  * [`ChannelNameRGB`](#channelnamergb)
  * [`ColorSpaceXYZ`](#colorspacexyz)
  * [`ChannelNameXYZ`](#channelnamexyz)
  * [`ChannelName`](#channelname)
  * [`KnownColorSpace`](#knowncolorspace)
  * [`PolarColorSpace`](#polarcolorspace)
  * [`RectangularColorSpace`](#rectangularcolorspace)
  * [`HueInterpolationMethod`](#hueinterpolationmethod)
  * [`SassColor`](#sasscolor)
    * [`internal`](#internal)
    * [Constructor](#constructor)
    * [`red`](#red)
    * [`green`](#green)
    * [`blue`](#blue)
    * [`hue`](#hue)
    * [`saturation`](#saturation)
    * [`lightness`](#lightness)
    * [`whiteness`](#whiteness)
    * [`blackness`](#blackness)
    * [`alpha`](#alpha)
    * [`change`](#change)

## Types

### `ColorSpaceHSL`

The HSL color space name.

```ts
export type ColorSpaceHSL = 'hsl';
```

### `ChannelNameHSL`

The HSL color space channel names.

```ts
export type ChannelNameHSL = 'hue' | 'saturation' | 'lightness';
```

### `ColorSpaceHWB`

The HWB color space name.

```ts
export type ColorSpaceHWB = 'hwb';
```

### `ChannelNameHWB`

The HWB color space channel names.

```ts
export type ChannelNameHWB = 'hue' | 'whiteness' | 'blackness';
```

### `ColorSpaceLab`

The Lab and Oklab color space names.

```ts
export type ColorSpaceLab = 'lab' | 'oklab';
```

### `ChannelNameLab`

The Lab and Oklab color space channel names.

```ts
export type ChannelNameLab = 'lightness' | 'a' | 'b';
```

### `ColorSpaceLCH`

The LCH and Oklch color space names.

```ts
export type ColorSpaceLCH = 'lch' | 'oklch';
```

### `ChannelNameLCH`

The LCH and Oklch color space channel names.

```ts
export type ChannelNameLCH = 'lightness' | 'chroma' | 'hue';
```

### `ColorSpaceRGB`

Names of color spaces with RGB channels.

```ts
export type ColorSpaceRGB =
  | 'a98-rgb'
  | 'display-p3'
  | 'prophoto-rgb'
  | 'rgb'
  | 'srgb'
  | 'srgb-linear';
```

### `ChannelNameRGB`

RGB channel names.

```ts
export type ChannelNameRGB = 'red' | 'green' | 'blue';
```

### `ColorSpaceXYZ`

Names of color spaces with XYZ channels.

```ts
export type ColorSpaceXYZ = 'xyz' | 'xyz-d50' | 'xyz-d65';
```

### `ChannelNameXYZ`

XYZ channel names.

```ts
export type ChannelNameXYZ = 'x' | 'y' | 'z';
```

### `ChannelName`

All supported channel names.

```ts
export type ChannelName =
  | ChannelNameHSL
  | ChannelNameHWB
  | ChannelNameLab
  | ChannelNameLCH
  | ChannelNameRGB
  | ChannelNameXYZ;
```

### `KnownColorSpace`

All supported color space names.

```ts
export type KnownColorSpace =
  | ColorSpaceHSL
  | ColorSpaceHWB
  | ColorSpaceLab
  | ColorSpaceLCH
  | ColorSpaceRGB
  | ColorSpaceXYZ;
```

### `PolarColorSpace`

Names of known color spaces which use a polar angle value for the `hue` channel.

```ts
export type PolarColorSpace = ColorSpaceHSL | ColorSpaceHWB | ColorSpaceLCH;
```

### `RectangularColorSpace`

Names of known color spaces which do not use a polar angle value for the `hue`
channel.

```ts
export type RectangularColorSpace = Exclude<KnownColorSpace, PolarColorSpace>;
```

### `HueInterpolationMethod`

Methods by which two hues are adjusted when interpolating between polar colors.

```ts
export type HueInterpolationMethod =
  | 'decreasing'
  | 'increasing'
  | 'longer'
  | 'shorter';
```

### `SassColor`

The JS API representation of a Sass color.

```ts
export class SassColor extends Value {
```

#### `internal`

The [private `internal` field] refers to a Sass color.

[private `internal` field]: index.d.ts.md#internal

#### Constructor

* If `options.red` is set:

  * Let `red` be a Sass number with a value of `options.red` `fuzzyRound`ed
    to the nearest integer.

  * Let `green` be a Sass number with a value of `options.green`
    `fuzzyRound`ed to the nearest integer.

  * Let `blue` be a Sass number with a value of `options.blue`
    `fuzzyRound`ed to the nearest integer.

  * If `options.alpha` is set, let `alpha` be a Sass number with a value of
    `options.alpha`. Otherwise, let `alpha` be `null`.

  * Set [`internal`] to the result of [`rgb(red, green, blue, alpha)`].

  [`internal`]: #internal
  [`rgb(red, green, blue, alpha)`]: ../../functions.md#rgb-and-rgba

* Otherwise, if `options.saturation` is set:

  * Let `hue` be a Sass number with a value of `options.hue`.

  * Let `saturation` be a Sass number with a value of `options.saturation`.

  * Let `lightness` be a Sass number with a value of `options.lightness`.

  * If `options.alpha` is set, let `alpha` be a Sass number with a value of
    `options.alpha`. Otherwise, let `alpha` be `null`.

  * Set [`internal`] to the result of [`hsl(hue, saturation, lightness,
    alpha)`].

  [`hsl(hue, saturation, lightness, alpha)`]: ../../functions.md#hsl-and-hsla

* Otherwise, if `options.whiteness` is set:

  * Let `hue` be a Sass number with a value of `options.hue`.

  * Let `whiteness` be a Sass number with a value of `options.whiteness`.

  * Let `blackness` be a Sass number with a value of `options.blackness`.

  * If `options.alpha` is set, let `alpha` be a Sass number with a value of
    `options.alpha`. Otherwise, let `alpha` be `null`.

  * Set [`internal`] to the result of [`hwb(hue, whiteness, blackness, alpha)`].

  [`hwb(hue, whiteness, blackness, alpha)`]: ../../built-in-modules/color.md#hwb

* Return `this`.

```ts
constructor(options: {
  red: number;
  green: number;
  blue: number;
  alpha?: number;
});

constructor(options: {
  hue: number;
  saturation: number;
  lightness: number;
  alpha?: number;
});

constructor(options: {
  hue: number;
  whiteness: number;
  blackness: number;
  alpha?: number;
});
```

#### `red`

Returns [`internal`]'s red channel.

```ts
get red(): number;
```

#### `green`

Returns [`internal`]'s green channel.

```ts
get green(): number;
```

#### `blue`

Returns [`internal`]'s blue channel.

```ts
get blue(): number;
```

#### `hue`

Returns the value of the result of [`hue(internal)`].

[`hue(internal)`]: ../../built-in-modules/color.md#hue

```ts
get hue(): number;
```

#### `saturation`

Returns the value of the result of [`saturation(internal)`].

[`saturation(internal)`]: ../../built-in-modules/color.md#saturation

```ts
get saturation(): number;
```

#### `lightness`

Returns the value of the result of [`lightness(internal)`].

[`lightness(internal)`]: ../../built-in-modules/color.md#lightness

```ts
get lightness(): number;
```

#### `whiteness`

Returns the value of the result of [`whiteness(internal)`].

[`whiteness(internal)`]: ../../built-in-modules/color.md#whiteness

```ts
get whiteness(): number;
```

#### `blackness`

Returns the value of the result of [`blackness(internal)`].

[`blackness(internal)`]: ../../built-in-modules/color.md#blackness

```ts
get blackness(): number;
```

#### `alpha`

Returns the value of the result of [`alpha(internal)`].

[`alpha(internal)`]: ../../built-in-modules/color.md#alpha

```ts
get alpha(): number;
```

#### `change`

Returns a new color created by changing some of this color's channels:

* If `options.whiteness` or `options.blackness` is set, return the result of:

  ```js
  SassColor({
    hue: options.hue ?? this.hue,
    whiteness: options.whiteness ?? this.whiteness,
    blackness: options.blackness ?? this.blackness,
    alpha: options.alpha ?? this.alpha
  })
  ```

* Otherwise, if `options.hue`, `options.saturation`, or `options.lightness`
  is set, return the result of:

  ```js
  SassColor({
    hue: options.hue ?? this.hue,
    saturation: options.saturation ?? this.saturation,
    lightness: options.lightness ?? this.lightness,
    alpha: options.alpha ?? this.alpha
  })
  ```

* Otherwise, return the result of:

  ```js
  SassColor({
    red: options.red ?? this.red,
    green: options.green ?? this.gren,
    blue: options.blue ?? this.blue,
    alpha: options.alpha ?? this.alpha
  })
  ```

```ts
change(options: {
  red?: number;
  green?: number;
  blue?: number;
  alpha?: number;
}): SassColor;

change(options: {
  hue?: number;
  saturation?: number;
  lightness?: number;
  alpha?: number;
}): SassColor;

change(options: {
  hue?: number;
  whiteness?: number;
  blackness?: number;
  alpha?: number;
}): SassColor;
```

```ts
} // SassColor
```
