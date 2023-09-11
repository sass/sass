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
    * [`toSpace`](#tospace)
    * [`isLegacy`](#islegacy)
    * [`isInGamut`](#isingamut)
    * [`toGamut`](#togamut)
    * [`channels`](#channels)
    * [`channelsOrNull`](#channelsornull)
    * [`channel`](#channel)
    * [`alpha`](#alpha)
    * [`isChannelMissing`](#ischannelmissing)
    * [`isAlphaMissing`](#isalphamissing)
    * [`isChannelPowerless`](#ischannelpowerless)
    * [`interpolate`](#interpolate)
  * [Updated Color Functions](#updated-color-functions)
    * [`change`](#change)
  * [New Constructors](#new-constructors)
    * [LAB Channel Constructor](#lab-channel-constructor)
    * [LCH Channel Constructor](#lch-channel-constructor)
    * [Predefined RGB Channel Constructor](#predefined-rgb-channel-constructor)
    * [XYZ Channel Constructor](#xyz-channel-constructor)
    * [Legacy Color Constructors with `space`](#legacy-color-constructors-with-space)
  * [Deprecations](#deprecations)
* [Procedures](#procedures)
  * [Parsing a Channel Value](#parsing-a-channel-value)
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

#### `toSpace`

Returns the result of converting [`internal`] to `space` as a new SassColor.

```ts
toSpace(space: KnownColorSpace): SassColor;
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

#### `toGamut`

Returns [`internal`] constrained to its space's gamut as a new SassColor. This
is generally not recommended since even older browsers will display out-of-gamut
colors as best they can, but it may be necessary in some cases.

```ts
toGamut(): SassColor;
```

#### `channels`

Returns an array of channel values (excluding `alpha`) for [`internal`], with
[missing channels][missing components] converted to `0`.

[missing components]: ./color-4-new-spaces.md#missing-components

```ts
get channels(): [number, number, number];
```

#### `channelsOrNull`

Returns an array of channel values (excluding `alpha`) for [`internal`], with
[missing channels][missing components] converted to `null`.

```ts
get channelsOrNull(): [number | null, number | null, number | null];
```

#### `channel`

Returns the value of the given `channel` in [`internal`]. If `space` is set, it
will first [convert] to that `space`.

```ts
channel(channel: ChannelName): number | null;
channel(
  channel: ChannelNameHSL,
  options: {space: ColorSpaceHSL}
): number | null;
channel(
  channel: ChannelNameHWB,
  options: {space: ColorSpaceHWB}
): number | null;
channel(
  channel: ChannelNameLAB,
  options: {space: ColorSpaceLAB}
): number | null;
channel(
  channel: ChannelNameLCH,
  options: {space: ColorSpaceLCH}
): number | null;
channel(
  channel: ChannelNameRGB,
  options: {space: ColorSpaceRGB}
): number | null;
channel(
  channel: ChannelNameXYZ,
  options: {space: ColorSpaceXYZ}
): number | null;
```

[convert]: ./color-4-new-spaces.md#converting-a-color

#### `alpha`

Returns the value of the `alpha` component, or `null` if one is not set.

```ts
get alpha(): number | null;
```

#### `isChannelMissing`

Returns whether the given `channel` of [`internal`] is missing. [Missing
channels][missing components] can be explicitly specified using the special
value `none` and can appear automatically when [toSpace()] returns a color with
a powerless channel. Throws an error if `channel` is not a channel in
[`internal`]'s `space`.

```ts
isChannelMissing(channel: ChannelName): boolean;
```

#### `isAlphaMissing`

Returns whether the `alpha` component is [missing][missing components].

```ts
get isAlphaMissing(): boolean;
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

#### `interpolate`

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

### Updated Color Functions

#### `change`

Replace the definition of [color.change] with the following:

[color.change]: ../spec/js-api/value/color.d.ts.md#change

This algorithm takes a JavaScript object `options` and returns a new SassColor
as the result of changing some of [`internal`]'s components.

> The `space` value defaults to the `space` of [`internal`], and the caller may
> specify any combination of channels and alpha in that space to be changed.
>
> If `space` is not a [legacy color space], a channel value of `null` will
> result in a [missing component][missing components] value for that channel.

* Let `initialSpace` be the value of [`this.space()`].

* Let `space` be `options.space` if it is defined, and the value of
  `initialSpace` otherwise.

* If `initialSpace` is a [legacy color space] and `options.space` is not set:

  * If `options.red` is set, let `space` be `rgb`.

  * Otherwise, if `options.saturation` is set, let `space` be `hsl`.

  * Otherwise, if `options.whiteness` is set, let `space` be `hwb`.

  * If `initialSpace` is not equal to `space`, emit a deprecation warning named
    `color-4-api`.

* Let `keys` be a list of the keys in `options` without `space`.

* Let `components` be `"alpha"` and the names of the channels in `space`.

* If any key in `keys` is not the name of a channel in `components`, throw an
  error.

* If `space` is not equal to `initialSpace`, let `color` be the result
  of [`this.toSpace(space)`]. Otherwise let `color` be `this`.

* If `space` equals `hsl`:

  * If `options.hue`, `options.saturation`, `options.lightness` or
    `options.alpha` equals null, emit a deprecation warning named `null-alpha`.

  * Let `changedColor` be the result of:

    ```js
    SassColor({
      hue: options.hue ?? color.channel('hue'),
      saturation: options.saturation ?? color.channel('saturation'),
      lightness: options.lightness ?? color.channel('lightness'),
      alpha: options.alpha ?? color.channel('alpha'),
      space: space
    })
    ```

* If space equals `hwb`:

  * If `options.hue`, `options.whiteness`, `options.blackness` or
    `options.alpha` equals null, emit a deprecation warning named `null-alpha`.

  * Let `changedColor` be the result of:

    ```js
    SassColor({
      hue: options.hue ?? color.channel('hue'),
      whiteness: options.whiteness ?? color.channel('whiteness'),
      blackness: options.blackness ?? color.channel('blackness'),
      alpha: options.alpha ?? color.channel('alpha'),
      space: space
    })
    ```

* If space equals `rgb`:

  * If `options.red`, `options.green`, `options.blue` or `options.alpha` equals
    null, emit a deprecation warning named `null-alpha`.

  * Let `changedColor` be the result of:

    ```js
    SassColor({
      red: options.red ?? color.channel('red'),
      green: options.green ?? color.channel('green'),
      blue: options.blue ?? color.channel('blue'),
      alpha: options.alpha ?? color.channel('alpha'),
      space: space
    })
    ```

* If space equals `lab` or `oklab`, let `changedColor` be the result of:

  ```js
  SassColor({
    lightness: keys.includes('lightness') ? options.lightness : color.channel('lightness'),
    a: keys.includes('a') ? options.a : color.channel('a'),
    b: keys.includes('b') ? options.b : color.channel('b'),
    alpha: keys.includes('alpha') ? options.alpha : color.channel('alpha'),
    space: space
  })
  ```

* If space equals `lch` or `oklch`, let `changedColor` be the result of:

  ```js
  SassColor({
    lightness: keys.includes('lightness') ? options.lightness : color.channel('lightness'),
    c: keys.includes('c') ? options.c : color.channel('c'),
    h: keys.includes('h') ? options.h : color.channel('h'),
    alpha: keys.includes('alpha') ? options.alpha : color.channel('alpha'),
    space: space
  })
  ```

* If `space` equals `a98-rgb`, `display-p3`, `prophoto-rgb`, `srgb`, or
  `srgb-linear`, let `changedColor` be the result of:

  ```js
  SassColor({
    red: keys.includes('red') ? options.red : color.channel('red'),
    green: keys.includes('green') ? options.green : color.channel('green'),
    blue: keys.includes('blue') ? options.blue : color.channel('blue'),
    alpha: keys.includes('alpha') ? options.alpha : color.channel('alpha'),
    space: space
  })
  ```

* If `space` equals `xyz`,  `xyz-d50`, or `xyz-d65`, let `changedColor` be the
    result of:

  ```js
  SassColor({
    y: keys.includes('y') ? options.y : color.channel('y'),
    x: keys.includes('x') ? options.x : color.channel('x'),
    z: keys.includes('z') ? options.z : color.channel('z'),
    alpha: keys.includes('alpha') ? options.alpha : color.channel('alpha'),
    space: space
  })
  ```

* If `initialSpace` is not equal to `space`, return the result of
  [`changedColor.toSpace(initialSpace)`].

* Otherwise, return `changedColor`.
  
[`this.space()`]: #space
[`this.toSpace(space)`]: #tospace
[`changedColor.toSpace(initialSpace)`]: #tospace

```ts
change(
  options: {
    [key in ChannelName]?: number | null;
  } & {alpha?: number}
): SassColor;

change(
  options: {
    [key in ChannelNameHSL]?: number | null;
  } & {
    alpha?: number;
    space: ColorSpaceHSL;
  }
): SassColor;

change(
  options: {
    [key in ChannelNameHWB]?: number | null;
  } & {
    alpha?: number;
    space: ColorSpaceHWB;
  }
): SassColor;

change(
  options: {
    [key in ChannelNameLAB]?: number | null;
  } & {
    alpha?: number | null;
    space: ColorSpaceLAB;
  }
): SassColor;

change(
  options: {
    [key in ChannelNameLCH]?: number | null;
  } & {
    alpha?: number | null;
    space: ColorSpaceLCH;
  }
): SassColor;

change(
  options: {
    [key in ChannelNameRGB]?: number | null;
  } & {
    alpha?: number | null;
    space: ColorSpaceRGB;
  }
): SassColor;

change(
  options: {
    [key in ChannelNameXYZ]?: number | null;
  } & {
    alpha?: number | null;
    space: ColorSpaceXYZ;
  }
): SassColor;
```

### New Constructors

* If `options.space` is not set, or `space` is a [legacy color space], follow
  the previous procedure for [construction].

* Otherwise, use the constructor that matches the value of `options.space`.

[construction]: ../spec/js-api/value/color.d.ts.md#constructor

#### LAB Channel Constructor

Create a new SassColor in a color space with LAB channels -- `lab` and `oklab`.

* Let `lightness` be the result of [parsing a channel value] with value
  `options.lightness`.

* Let `a` be the result of [parsing a channel value] with value `options.a`.

* Let `b` be the result of [parsing a channel value] with value `options.b`.
  
* If `options.alpha` is not set, let `alpha` be `1`. Otherwise, let `alpha` be
    the result of [parsing a channel value] with value `options.alpha`.

* If `options.space` equals `lab`, set [`internal`] to the result of
  [`lab(lightness a b / alpha)`].

* Otherwise, if `options.space` equals `oklab`, set [`internal`] to the result
  of [`oklab(lightness a b / alpha)`].

 [`lab(lightness a b / alpha)`]: ./color-4-new-spaces.md#lab
 [`oklab(lightness a b / alpha)`]: ./color-4-new-spaces.md#oklab

```ts
constructor(options: {
  lightness: number | null;
  a: number | null;
  b: number | null;
  alpha?: number | null;
  space: ColorSpaceLAB;
});
```

#### LCH Channel Constructor

Create a new SassColor in a color space with LCH channels -- `lch` and `oklch`.

* Let `lightness` be the result of [parsing a channel value] with value
  `options.lightness`.

* Let `c` be the result of [parsing a channel value] with value `options.c`.

* Let `h` be the result of [parsing a channel value] with value `options.h`.
  
* If `options.alpha` is not set, let `alpha` be `1`. Otherwise, let `alpha` be
    the result of [parsing a channel value] with value `options.alpha`.

* If `options.space` equals `lch`, set [`internal`] to the result of
  [`lch(lightness a b / alpha)`].

* Otherwise, if `options.space` equals `oklch`, set [`internal`] to the result
  of [`oklch(lightness a b / alpha)`].

```ts
constructor(options: {
  lightness: number | null;
  chroma: number | null;
  hue: number | null;
  alpha?: number | null;
  space: ColorSpaceLCH;
});
```

#### Predefined RGB Channel Constructor

Create a new SassColor in a color space with RGB channels -- `srgb`,
`srgb-linear`, `display-p3`, `a98-rgb`, and `prophoto-rgb`. `rgb` is not
supported with this constructor, as the legacy `rgb` color does not support
[missing components].

* Let `red` be the result of [parsing a channel value] with value `options.red`.

* Let `green` be the result of [parsing a channel value] with value
  `options.green`.

* Let `blue` be the result of [parsing a channel value] with value
  `options.blue`.
  
* If `options.alpha` is not set, let `alpha` be `1`. Otherwise, let `alpha` be
    the result of [parsing a channel value] with value `options.alpha`.

* Let `space` be the unquoted string value of `options.space`.

* Set [`internal`] to the result of [`color(space red green blue / alpha)`].

[`color(space red green blue / alpha)`]: ./color-4-new-spaces.md#color-1

```ts
constructor(options: {
  red: number | null;
  green: number | null;
  blue: number | null;
  alpha?: number | null;
  space: Omit<ColorSpaceRGB, 'rgb'>;
});
```

#### XYZ Channel Constructor

Create a new SassColor in a color space with XYZ channels -- `xyz`, `xyz-d50`,
and `xyz-d65`.

* Let `x` be the result of [parsing a channel value] with value `options.x`.

* Let `y` be the result of [parsing a channel value] with value `options.y`.

* Let `z` be the result of [parsing a channel value] with value `options.z`.
  
* If `options.alpha` is not set, let `alpha` be `1`. Otherwise, let `alpha` be
    the result of [parsing a channel value] with value `options.alpha`.

* Let `space` be the unquoted string value of `options.space`.

* Set [`internal`] to the result of [`color(space x y z / alpha)`].

```ts
constructor(options: {
  x: number | null;
  y: number | null;
  z: number | null;
  alpha?: number | null;
  space: ColorSpaceXYZ;
});
```

#### Legacy Color Constructors with `space`

While the [legacy color space] constructors do not require a space, replace the
[existing types] with these constuctor overloads for forward compatibility.

[existing types]: ../spec/js-api/value/color.d.ts.md#constructor

```ts
constructor(options: {
  red: number;
  green: number;
  blue: number;
  alpha?: number;
  space?: 'rgb';
});

constructor(options: {
  hue: number;
  saturation: number;
  lightness: number;
  alpha?: number;
  space?: 'hsl';
});

constructor(options: {
  hue: number;
  whiteness: number;
  blackness: number;
  alpha?: number;
  space?: 'hwb';
});
```

```ts
}
```

### Deprecations

A number of SassColor getters only make sense for [legacy color space], and so
are being deprecated for `channel`. This deprecation is called `color-4-api`.

* `red`
* `green`
* `blue`
* `hue`
* `saturation`
* `lightness`
* `whiteness`
* `blackness`
* `alpha`

## Procedures

### Parsing a Channel Value

This procedure takes a channel value `value`, and returns the special value
`none` if the value is `null`.

* If `value` is a number, return a Sass number with a value of `value`.

* If `value` is the Javascript value `null`, return the unquoted Sass string
  `none`.

## Embedded Protocol

This introduces a breaking change in the Embedded Protocol, as it removes the
legacy SassScript values.

### SassColor

```proto
message SassColor {
  // The name of a known color space.
  string space = 1;

  // The value of the first channel associated with `space`.
  double channel1 = 2;

  // The value of the second channel associated with `space`.
  double channel2 = 3;

  // The value of the third channel associated with `space`.
  double channel3 = 4;

  // The color's alpha channel. Mandatory. Must be between 0 and 1,
  // inclusive.
  double alpha = 5;
}
```

### Removed SassScript values

The `RgbColor`, `HslColor` and `HwbColor` SassScript values will be removed from
the Embedded Protocol.
