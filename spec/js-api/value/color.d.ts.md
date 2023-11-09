# Color API

```ts
import {List} from 'immutable';

import {Value} from './index';
```

## Table of Contents

* [Types](#types)
  * [`ColorSpaceHsl`](#colorspacehsl)
  * [`ChannelNameHsl`](#channelnamehsl)
  * [`ColorSpaceHwb`](#colorspacehwb)
  * [`ChannelNameHwb`](#channelnamehwb)
  * [`ColorSpaceLab`](#colorspacelab)
  * [`ChannelNameLab`](#channelnamelab)
  * [`ColorSpaceLch`](#colorspacelch)
  * [`ChannelNameLch`](#channelnamelch)
  * [`ColorSpaceRgb`](#colorspacergb)
  * [`ChannelNameRgb`](#channelnamergb)
  * [`ColorSpaceXyz`](#colorspacexyz)
  * [`ChannelNameXyz`](#channelnamexyz)
  * [`ChannelName`](#channelname)
  * [`KnownColorSpace`](#knowncolorspace)
  * [`PolarColorSpace`](#polarcolorspace)
  * [`RectangularColorSpace`](#rectangularcolorspace)
  * [`HueInterpolationMethod`](#hueinterpolationmethod)
  * [`SassColor`](#sasscolor)
    * [`internal`](#internal)
    * [Constructor](#constructor)
      * [RGB](#rgb)
      * [HSL](#hsl)
      * [HWB](#hwb)
      * [Lab](#lab)
      * [LCH](#lch)
      * [Predefined RGB](#predefined-rgb)
      * [XYZ](#xyz)
    * [`space`](#space)
    * [`toSpace`](#tospace)
    * [`isLegacy`](#islegacy)
    * [`isInGamut`](#isingamut)
    * [`toGamut`](#togamut)
    * [`channelsOrNull`](#channelsornull)
    * [`channels`](#channels)
    * [`channel`](#channel)
    * [`alpha`](#alpha)
    * [`isChannelMissing`](#ischannelmissing)
    * [`isChannelPowerless`](#ischannelpowerless)
    * [`interpolate`](#interpolate)
    * [`change`](#change)
    * [`red`](#red)
    * [`green`](#green)
    * [`blue`](#blue)
    * [`hue`](#hue)
    * [`saturation`](#saturation)
    * [`lightness`](#lightness)
    * [`whiteness`](#whiteness)
    * [`blackness`](#blackness)
* [Procedures](#procedures)
  * [Parsing a Channel Value](#parsing-a-channel-value)
  * [Parsing a Clamped Channel Value](#parsing-a-clamped-channel-value)
  * [Changing a Component Value](#changing-a-component-value)

## Types

### `ColorSpaceHsl`

The HSL color space name.

```ts
export type ColorSpaceHsl = 'hsl';
```

### `ChannelNameHsl`

The HSL color space channel names.

```ts
export type ChannelNameHsl = 'hue' | 'saturation' | 'lightness' | 'alpha';
```

### `ColorSpaceHwb`

The HWB color space name.

```ts
export type ColorSpaceHwb = 'hwb';
```

### `ChannelNameHwb`

The HWB color space channel names.

```ts
export type ChannelNameHwb = 'hue' | 'whiteness' | 'blackness' | 'alpha';
```

### `ColorSpaceLab`

The Lab and Oklab color space names.

```ts
export type ColorSpaceLab = 'lab' | 'oklab';
```

### `ChannelNameLab`

The Lab and Oklab color space channel names.

```ts
export type ChannelNameLab = 'lightness' | 'a' | 'b' | 'alpha';
```

### `ColorSpaceLch`

The LCH and Oklch color space names.

```ts
export type ColorSpaceLch = 'lch' | 'oklch';
```

### `ChannelNameLch`

The LCH and Oklch color space channel names.

```ts
export type ChannelNameLch = 'lightness' | 'chroma' | 'hue' | 'alpha';
```

### `ColorSpaceRgb`

Names of color spaces with RGB channels.

```ts
export type ColorSpaceRgb =
  | 'a98-rgb'
  | 'display-p3'
  | 'prophoto-rgb'
  | 'rec2020'
  | 'rgb'
  | 'srgb'
  | 'srgb-linear';
```

### `ChannelNameRgb`

RGB channel names.

```ts
export type ChannelNameRgb = 'red' | 'green' | 'blue' | 'alpha';
```

### `ColorSpaceXyz`

Names of color spaces with XYZ channels.

```ts
export type ColorSpaceXyz = 'xyz' | 'xyz-d50' | 'xyz-d65';
```

### `ChannelNameXyz`

XYZ channel names.

```ts
export type ChannelNameXyz = 'x' | 'y' | 'z' | 'alpha';
```

### `ChannelName`

All supported channel names.

```ts
export type ChannelName =
  | ChannelNameHsl
  | ChannelNameHwb
  | ChannelNameLab
  | ChannelNameLch
  | ChannelNameRgb
  | ChannelNameXyz;
```

### `KnownColorSpace`

All supported color space names.

```ts
export type KnownColorSpace =
  | ColorSpaceHsl
  | ColorSpaceHwb
  | ColorSpaceLab
  | ColorSpaceLch
  | ColorSpaceRgb
  | ColorSpaceXyz;
```

### `PolarColorSpace`

Names of known color spaces which use a polar angle value for the `hue` channel.

```ts
export type PolarColorSpace = ColorSpaceHsl | ColorSpaceHwb | ColorSpaceLch;
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

* If `options.space` is set, let `constructionSpace` be `options.space`.

* Otherwise, if `options.red` is set, let `constructionSpace` be "rgb".

* Otherwise, if `options.saturation` is set, let `constructionSpace` be "hsl".

* Otherwise, if `options.whiteness` is set, let `constructionSpace` be "hwb".

* Otherwise, throw an error.

##### RGB

* If `constructionSpace` is "rgb":

  * If `options.alpha` is `null` and `options.space` is not set, emit a
    deprecation warning named `null-alpha`.

  * Let `red` be the result of [parsing a channel value] with value
    `options.red`.

  * Let `green` be the result of [parsing a channel value] with value
    `options.green`.

  * Let `blue` be the result of [parsing a channel value] with value
    `options.blue`.

  * If `options.alpha` is not set, let `alpha` be `1`. Otherwise, let `alpha` be
    the result of [parsing a clamped channel value] with `value` of
    `options.alpha`, `minimum` of `0`, and `maximum` of `1`.

  * Set [`internal`] to the result of [`rgb(red green blue / alpha)`].

  * Return `this`.

[parsing a channel value]: #parsing-a-channel-value
[parsing a clamped channel value]: #parsing-a-clamped-channel-value
[`internal`]: #internal
[`rgb(red green blue / alpha)`]: ../../functions.md#rgb-and-rgba

```ts
constructor(options: {
  red: number | null;
  green: number | null;
  blue: number | null;
  alpha?: number | null;
  space?: 'rgb';
});
```

##### HSL

* Otherwise, if `constructionSpace` is "hsl":

  * If `options.alpha` is `null` and `options.space` is not set, emit a
    deprecation warning named `null-alpha`.

  * Let `hue` be the result of [parsing a channel value] with value
    `options.hue`.

  * Let `saturation` be the result of [parsing a channel value] with value
    `options.saturation`.

  * Let `lightness` be the result of [parsing a clamped channel value] with
    `value` of `options.lightness`, `minimum` of `0`, and `maximum` of `100`.

  * If `options.alpha` is not set, let `alpha` be `1`. Otherwise, let `alpha` be
    the result of [parsing a clamped channel value] with `value` of
    `options.alpha`, `minimum` of `0`, and `maximum` of `1`.

  * Set [`internal`] to the result of [`hsl(hue saturation lightness / alpha)`].

  * Return `this`.

[`hsl(hue saturation lightness / alpha)`]: ../../functions.md#hsl-and-hsla

```ts
constructor(options: {
  hue: number | null;
  saturation: number | null;
  lightness: number | null;
  alpha?: number | null;
  space?: ColorSpaceHsl;
});
```

##### HWB

* Otherwise, if `constructionSpace` is "hwb":

  * If `options.alpha` is `null` and `options.space` is not set, emit a
    deprecation warning named `null-alpha`.

  * Let `hue` be the result of [parsing a channel value] with value
    `options.hue`.

  * Let `whiteness` be the result of [parsing a channel value] with value
    `options.whiteness`.

  * Let `blackness` be the result of [parsing a channel value] with value
    `options.blackness`.

  * If `options.alpha` is not set, let `alpha` be `1`. Otherwise, let `alpha` be
    the result of [parsing a clamped channel value] with `value` of
    `options.alpha`, `minimum` of `0`, and `maximum` of `1`.

  * Set [`internal`] to the result of [`hwb(hue whiteness blackness / alpha)`].

  * Return `this`.

[`hwb(hue whiteness blackness / alpha)`]: ../../../proposal/color-4-new-spaces.md#hwb-1

```ts
constructor(options: {
  hue: number | null;
  whiteness: number | null;
  blackness: number | null;
  alpha?: number | null;
  space?: ColorSpaceHwb;
});
```

##### Lab

* Otherwise, if `constructionSpace` is "lab" or "oklab":

  * If `options.space` equals `lab`, let `maximum` be `100`. Otherwise, let
    `maximum` be `1`.

  * Let `lightness` be the result of [parsing a clamped channel value] with
    `value` of `options.lightness`, `minimum` of `0`, and `maximum` of
    `maximum`.

  * Let `a` be the result of [parsing a channel value] with value `options.a`.

  * Let `b` be the result of [parsing a channel value] with value `options.b`.

  * If `options.alpha` is not set, let `alpha` be `1`. Otherwise, let `alpha` be
    the result of [parsing a clamped channel value] with value `options.alpha`,
    `minimum` of 0, and `maximum` of 1.

  * If `options.space` equals `lab`, set [`internal`] to the result of
    [`lab(lightness a b / alpha)`].

  * Otherwise, if `options.space` equals `oklab`, set [`internal`] to the
    result of [`oklab(lightness a b / alpha)`].

  * Return `this`.

[`lab(lightness a b / alpha)`]: ../../../proposal/color-4-new-spaces.md#lab
[`oklab(lightness a b / alpha)`]: ../../../proposal/color-4-new-spaces.md#oklab

```ts
constructor(options: {
  lightness: number | null;
  a: number | null;
  b: number | null;
  alpha?: number | null;
  space: ColorSpaceLab;
});
```

##### LCH

* Otherwise, if `constructionSpace` is "lch" or "oklch":

  * If `options.space` equals `lch`, let `maximum` be `100`. Otherwise, let
    `maximum` be `1`.

  * Let `lightness` be the result of [parsing a clamped channel value] with
    `value` of `options.lightness`, `minimum` of `0`, and `maximum` of
    `maximum`.

  * Let `c` be the result of [parsing a channel value] with value `options.c`.

  * Let `h` be the result of [parsing a channel value] with value `options.h`.

  * If `options.alpha` is not set, let `alpha` be `1`. Otherwise, let `alpha` be
    the result of [parsing a clamped channel value] with value `options.alpha`,
    `minimum` of 0, and `maximum` of 1.

  * If `options.space` equals `lch`, set [`internal`] to the result of
    [`lch(lightness a b / alpha)`].

  * Otherwise, if `options.space` equals `oklch`, set [`internal`] to the
    result of [`oklch(lightness a b / alpha)`].

  * Return `this`.

[`lch(lightness a b / alpha)`]: ../../../proposal/color-4-new-spaces.md#lch
[`oklch(lightness a b / alpha)`]: ../../../proposal/color-4-new-spaces.md#oklch

```ts
constructor(options: {
  lightness: number | null;
  chroma: number | null;
  hue: number | null;
  alpha?: number | null;
  space: ColorSpaceLch;
});
```

##### Predefined RGB

* Otherwise, if `constructionSpace` is "srgb", "srgb-linear", "display-p3",
  "a98-rgb", "prophoto-rgb", or "rec2020":

  * Let `red` be the result of [parsing a channel value] with value
    `options.red`.

  * Let `green` be the result of [parsing a channel value] with value
    `options.green`.

  * Let `blue` be the result of [parsing a channel value] with value
    `options.blue`.

  * If `options.alpha` is not set, let `alpha` be `1`. Otherwise, let `alpha` be
    the result of [parsing a clamped channel value] with value `options.alpha`,
    `minimum` of 0, and `maximum` of 1.

  * Let `space` be the unquoted string value of `options.space`.

  * Set [`internal`] to the result of [`color(space red green blue / alpha)`].

  * Return `this`.

[`color(space red green blue / alpha)`]: ../../../proposal/color-4-new-spaces.md#color-1

```ts
constructor(options: {
  red: number | null;
  green: number | null;
  blue: number | null;
  alpha?: number | null;
  space: Exclude<ColorSpaceRgb, 'rgb'>;
});
```

##### XYZ

* Otherwise, if `constructionSpace` is "xyz", "xyz-d50", or "xyz-d65":

  * Let `x` be the result of [parsing a channel value] with value `options.x`.

  * Let `y` be the result of [parsing a channel value] with value `options.y`.

  * Let `z` be the result of [parsing a channel value] with value `options.z`.

  * If `options.alpha` is not set, let `alpha` be `1`. Otherwise, let `alpha` be
    the result of [parsing a clamped channel value] with value `options.alpha`,
    `minimum` of 0, and `maximum` of 1.

  * Let `space` be the unquoted string value of `options.space`.

  * Set [`internal`] to the result of [`color(space x y z / alpha)`].

  * Return `this`.

[`color(space x y z / alpha)`]: ../../../proposal/color-4-new-spaces.md#color-1

```ts
constructor(options: {
  x: number | null;
  y: number | null;
  z: number | null;
  alpha?: number | null;
  space: ColorSpaceXyz;
});
```

#### `space`

Returns the name of [`internal`]'s space.

```ts
get space(): KnownColorSpace;
```

#### `toSpace`

* If `this.space` is equal to `space`, return `this`.

* Otherwise, return the result of [Converting a Color] with `this` as
  `origin-color` and `space` as `target-space`.

```ts
toSpace(space: KnownColorSpace): SassColor;
```

[Converting a Color]: ../../../proposal/color-4-new-spaces.md#converting-a-color

#### `isLegacy`

Returns whether [`internal`] is in a [legacy color space] (`rgb`, `hsl`, or
`hwb`).

```ts
get isLegacy(): boolean;
```

[legacy color space]: ../../../proposal/color-4-new-spaces.md#legacy-color

#### `isInGamut`

Returns the result of [`color.is-in-gamut(internal, space)`] as a JavaScript
boolean.

```ts
isInGamut(space?: KnownColorSpace): boolean;
```

[`color.is-in-gamut(internal, space)`]: ../../../proposal/color-4-new-spaces.md#coloris-in-gamut

#### `toGamut`

Returns the result of [`color.to-gamut(internal, space)`].

```ts
toGamut(space?: KnownColorSpace): SassColor;
```

[`color.to-gamut(internal, space)`]: ../../../proposal/color-4-new-spaces.md#colorto-gamut-1

#### `channelsOrNull`

Returns a list of channel values (excluding alpha) for [`internal`], with
[missing channels][missing components] converted to `null`.

* Let `space` be the value of [`this.space`].

* Let `components` be the list of channels in `space`.

* Let `channels` be an empty list.

* For each `component` in `components`:

  * Let `value` be the channel value in [`internal`] with name of `component`.

  * If `value` is `none`, let `value` be `null`.

  * Append `value` to `channels`.

* Return `channels`.

```ts
get channelsOrNull(): List<number | null>;
```

[missing components]: ../../../proposal/color-4-new-spaces.md#missing-components
[`this.space`]: #space

#### `channels`

This algorithm returns a list of channel values (excluding alpha) for
[`internal`], with [missing channels][missing components] converted to `0`.

* Let `channelsOrNull` be the result of [`this.channelsOrNull`].

* Let `channels` be an empty list.

* For each `channel` in `channelsOrNull`:

  * If `channel` equals `null`, let `value` be 0.

  * Append `value` to `channels`.

* Return `channels`.

[`this.channelsOrNull`]: #channelsornull

```ts
get channels(): List<number>;
```

#### `channel`

* Let `initialSpace` be the value of [`this.space`].

* Let `space` be `options.space` if it is defined, and the value of
  `initialSpace` otherwise.

* If `channel` is not "alpha" or a channel in `space`, throw an error.

* Let `color` be the result of [`this.toSpace(space)`].

* Let `value` be the channel value in `color` with name of `component`.

* If `value` is `null`, return 0.

* Otherwise, return `value`.

[`this.toSpace(space)`]: #tospace

```ts
channel(channel: ChannelName): number;
channel(channel: ChannelNameHsl, options: {space: ColorSpaceHsl}): number;
channel(channel: ChannelNameHwb, options: {space: ColorSpaceHwb}): number;
channel(channel: ChannelNameLab, options: {space: ColorSpaceLab}): number;
channel(channel: ChannelNameLch, options: {space: ColorSpaceLch}): number;
channel(channel: ChannelNameRgb, options: {space: ColorSpaceRgb}): number;
channel(channel: ChannelNameXyz, options: {space: ColorSpaceXyz}): number;
```

#### `alpha`

Returns the result of calling [`this.channel('alpha')`].

[`this.channel('alpha')`]: #channel

```ts
get alpha(): number;
```

#### `isChannelMissing`

Returns the result of [`color.is-missing(internal,
channel)`][color.is-missing()] as a JavaScript boolean.

```ts
isChannelMissing(channel: ChannelName): boolean;
```

[color.is-missing()]: ../../../proposal/color-4-new-spaces.md#coloris-missing-1

#### `isChannelPowerless`

Returns the result of [`color.is-powerless(internal, channel, space)`] as a
JavaScript boolean.

[`color.is-powerless(internal, channel, space)`]: ../../../proposal/color-4-new-spaces.md#coloris-powerless-1

```ts
isChannelPowerless(channel: ChannelName): boolean;
isChannelPowerless(
  channel: ChannelNameHsl,
  options?: {space: ColorSpaceHsl}
): boolean;
isChannelPowerless(
  channel: ChannelNameHwb,
  options?: {space: ColorSpaceHwb}
): boolean;
isChannelPowerless(
  channel: ChannelNameLab,
  options?: {space: ColorSpaceLab}
): boolean;
isChannelPowerless(
  channel: ChannelNameLch,
  options?: {space: ColorSpaceLch}
): boolean;
isChannelPowerless(
  channel: ChannelNameRgb,
  options?: {space: ColorSpaceRgb}
): boolean;
isChannelPowerless(
  channel: ChannelNameXyz,
  options?: {space: ColorSpaceXyz}
): boolean;
```

#### `interpolate`

* Let `space` be the value of [`this.space`].

* If `options.method` is set, let `interpolationMethod` be a space separated
  list containing the value of `space`, a space, and the value of
  `options.method`.

* Otherwise, if `space` is a rectangular color space, let `interpolationMethod`
  be `space`.

* Otherwise, let `interpolationMethod` be a space separated list containing the
  value of `space`, a space, and the string "shorter".

* Return the result of [`color.mix(internal, color2, options.weight, interpolationMethod)`][`color.mix()`].

```ts
interpolate(
  color2: SassColor,
  options?: {
    weight?: number;
    method?: HueInterpolationMethod;
  }
): SassColor;
```

[`color.mix()`]: ../../../proposal/color-4-new-spaces.md#colormix-1

#### `change`

This algorithm takes a JavaScript object `options` and returns a new SassColor
as the result of changing some of [`internal`]'s components.

> The `space` value defaults to the `space` of [`internal`], and the caller may
> specify any combination of channels and alpha in that space to be changed.
>
> If `space` is not a [legacy color space], a channel value of `null` will
> result in a [missing component][missing components] value for that channel.

* Let `initialSpace` be the value of [`this.space`].

* Let `spaceSetExplicitly` be `true` if `options.space` is defined, and `false`
  otherwise.

* Let `space` be `options.space` if `spaceSetExplicitly` is true, and the value
  of `initialSpace` otherwise.

* If `initialSpace` is a [legacy color space] and `spaceSetExplicitly` is false:

  * If `options.whiteness` or `options.blackness` is set, let `space` be `hwb`.

  * Otherwise, if `options.hue` is set and `initialSpace` is `hwb`, let space be
    `hwb`.

  * Otherwise, if `options.hue`, `options.saturation`, or `options.lightness` is
    set, let `space` be `hsl`.

  * Otherwise, if `options.red`, `options.green`, or `options.blue` is set, let
    `space` be `rgb`.

  * If `initialSpace` is not equal to `space`, emit a deprecation warning named
    `color-4-api`.

* Let `changes` be the object `options` without `space` and its value.

* Let `keys` be a list of the keys in `changes`.

* Let `components` be `"alpha"` and the names of the channels in `space`.

* If any key in `keys` is not the name of a channel in `components`, throw an
  error.

* If `options.alpha` is set, and isn't either null or a number between 0 and 1
  (inclusive and fuzzy), throw an error.

* If `options.lightness` is set, and isn't either null or a number between 0 and
  the maximum channel value for the space (inclusive and fuzzy), throw an error.

* Let `color` be the result of [`this.toSpace(space)`].

* Let `changedValue` be a function that takes a string argument for `channel`
  and calls the procedure [`Changing a Component Value`] with `changes` and
  `color` as `initial`.

* If `space` equals `hsl` and `spaceSetExplicitly` is `false`:

  * If any of `options.hue`, `options.saturation` or `options.lightness` equals
    `null`, emit a deprecation warning named `color-4-api`.

  * If `options.alpha` equals `null`, emit a deprecation warning named
    `null-alpha`.

  * Let `changedColor` be the result of:

    ```js
    new SassColor({
      hue: options.hue ?? color.channel('hue'),
      saturation: options.saturation ?? color.channel('saturation'),
      lightness: options.lightness ?? color.channel('lightness'),
      alpha: options.alpha ?? color.channel('alpha'),
      space: space
    })
    ```

* If `space` equals `hsl` and `spaceSetExplicitly` is `true`, let `changedColor`
  be the result of:

   ```js
  new SassColor({
    hue: changedValue('hue'),
    saturation: changedValue('saturation'),
    lightness: changedValue('lightness'),
    alpha: changedValue('alpha'),
    space: space
  })
  ```

* If `space` equals `hwb` and `spaceSetExplicitly` is `false`:

  * If any of `options.hue`, `options.whiteness` or `options.blackness` equals
    `null`, emit a deprecation warning named `color-4-api`.

  * If `options.alpha` equals `null`, emit a deprecation warning named
    `null-alpha`.

  * Let `changedColor` be the result of:

    ```js
    new SassColor({
      hue: options.hue ?? color.channel('hue'),
      whiteness: options.whiteness ?? color.channel('whiteness'),
      blackness: options.blackness ?? color.channel('blackness'),
      alpha: options.alpha ?? color.channel('alpha'),
      space: space
    })
    ```

* If `space` equals `hwb` and `spaceSetExplicitly` is `true`, let `changedColor`
  be the result of:

   ```js
  new SassColor({
    hue: changedValue('hue'),
    whiteness: changedValue('whiteness'),
    blackness: changedValue('blackness'),
    alpha: changedValue('alpha'),
    space: space
  })
  ```

* If `space` equals `rgb` and `spaceSetExplicitly` is `false`:

  * If any of `options.red`, `options.green` or `options.blue` equals `null`,
    emit a deprecation warning named `color-4-api`.

  * If `options.alpha` equals `null`, emit a deprecation warning named
    `null-alpha`.

  * Let `changedColor` be the result of:

    ```js
    new SassColor({
      red: options.red ?? color.channel('red'),
      green: options.green ?? color.channel('green'),
      blue: options.blue ?? color.channel('blue'),
      alpha: options.alpha ?? color.channel('alpha'),
      space: space
    })
    ```

* If `space` equals `rgb` and `spaceSetExplicitly` is `true`, let `changedColor`
  be the result of:

   ```js
  new SassColor({
    red: changedValue('red'),
    green: changedValue('green'),
    blue: changedValue('blue'),
    alpha: changedValue('alpha'),
    space: space
  })
  ```

* If `space` equals `lab` or `oklab`, let `changedColor` be the result of:

  ```js
  new SassColor({
    lightness: changedValue('lightness'),
    a: changedValue('a'),
    b: changedValue('b'),
    alpha: changedValue('alpha'),
    space: space
  })
  ```

* If `space` equals `lch` or `oklch`, let `changedColor` be the result of:

  ```js
  new SassColor({
    lightness: changedValue('lightness'),
    chroma: changedValue('chroma'),
    hue: changedValue('hue'),
    alpha: changedValue('alpha'),
    space: space
  })
  ```

* If `space` equals `a98-rgb`, `display-p3`, `prophoto-rgb`, `rec2020`, `srgb`,
  or `srgb-linear`, let `changedColor` be the result of:

  ```js
  new SassColor({
    red: changedValue('red'),
    green: changedValue('green'),
    blue: changedValue('blue'),
    alpha: changedValue('alpha'),
    space: space
  })
  ```

* If `space` equals `xyz`, `xyz-d50`, or `xyz-d65`, let `changedColor` be the
  result of:

  ```js
  new SassColor({
    y: changedValue('y'),
    x: changedValue('x'),
    z: changedValue('z'),
    alpha: changedValue('alpha'),
    space: space
  })
  ```

* Return the result of [`changedColor.toSpace(initialSpace)`].

[`changedColor.toSpace(initialSpace)`]: #tospace
[`Changing a Component Value`]: #changing-a-component-value

```ts
change(
  options: {
    [key in ChannelNameHsl]?: number | null;
  } & {
    space?: ColorSpaceHsl;
  }
): SassColor;

change(
  options: {
    [key in ChannelNameHwb]?: number | null;
  } & {
    space?: ColorSpaceHwb;
  }
): SassColor;

change(
  options: {
    [key in ChannelNameLab]?: number | null;
  } & {
    space?: ColorSpaceLab;
  }
): SassColor;

change(
  options: {
    [key in ChannelNameLch]?: number | null;
  } & {
    space?: ColorSpaceLch;
  }
): SassColor;

change(
  options: {
    [key in ChannelNameRgb]?: number | null;
  } & {
    space?: ColorSpaceRgb;
  }
): SassColor;

change(
  options: {
    [key in ChannelNameXyz]?: number | null;
  } & {
    space?: ColorSpaceXyz;
  }
): SassColor;
```

#### `red`

> This is deprecated in favor of the new [`channel`] function. This deprecation
> is called `color-4-api`.

[`channel`]: #channel

Returns [`internal`]'s red channel in the RGB color space.

```ts
get red(): number;
```

#### `green`

> This is deprecated in favor of the new [`channel`] function. This deprecation
> is called `color-4-api`.

Returns [`internal`]'s green channel in the RGB color space.

```ts
get green(): number;
```

#### `blue`

> This is deprecated in favor of the new [`channel`] function. This deprecation
> is called `color-4-api`.

Returns [`internal`]'s blue channel in the RGB color space.

```ts
get blue(): number;
```

#### `hue`

> This is deprecated in favor of the new [`channel`] function. This deprecation
> is called `color-4-api`.

Returns the value of the result of [`hue(internal)`].

[`hue(internal)`]: ../../built-in-modules/color.md#hue

```ts
get hue(): number;
```

#### `saturation`

> This is deprecated in favor of the new [`channel`] function. This deprecation
> is called `color-4-api`.

Returns the value of the result of [`saturation(internal)`].

[`saturation(internal)`]: ../../built-in-modules/color.md#saturation

```ts
get saturation(): number;
```

#### `lightness`

> This is deprecated in favor of the new [`channel`] function. This deprecation
> is called `color-4-api`.

Returns the value of the result of [`lightness(internal)`].

[`lightness(internal)`]: ../../built-in-modules/color.md#lightness

```ts
get lightness(): number;
```

#### `whiteness`

> This is deprecated in favor of the new [`channel`] function. This deprecation
> is called `color-4-api`.

Returns the value of the result of [`whiteness(internal)`].

[`whiteness(internal)`]: ../../built-in-modules/color.md#whiteness

```ts
get whiteness(): number;
```

#### `blackness`

> This is deprecated in favor of the new [`channel`] function. This deprecation
> is called `color-4-api`.

Returns the value of the result of [`blackness(internal)`].

[`blackness(internal)`]: ../../built-in-modules/color.md#blackness

```ts
get blackness(): number;
```

```ts
} // SassColor
```

## Procedures

### Parsing a Channel Value

This procedure takes a channel value `value`, and returns the special value
`none` if the value is `null`.

* If `value` is a number, return a Sass number with a value of `value`.

* If `value` is the Javascript value `null`, return the unquoted Sass string
  `none`.

### Parsing a Clamped Channel Value

This procedure takes a channel value `value` and an inclusive range of `minimum`
and `maximum`. It asserts the value is in the range, and returns the special
value `none` if the value is `null`.

* If `value` is fuzzy less-than `minimum`, throw an error.

* If `value` is fuzzy greater-than `maximum`, throw an error.

* Otherwise, return the result of [Parsing a Channel Value].

### Changing a Component Value

This procedure takes a `channel` name, an object `changes` and a SassColor
`initial` and returns the result of applying the change for `channel` to
`initial`.

* Let `initialValue` be the channel value in `initial` with name of `channel`.

* If `channel` is not a key in `changes`, return `initialValue`.

* Let `changedValue` be the value for `channel` in `changes`.

* If `changedValue` is `undefined` and not `null`, return `initialValue`.

* Otherwise, return `changedValue`.
