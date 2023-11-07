## Draft 1.6

* Simplify the type definition for `interpolate`, and make `options` argument
  optional.

## Draft 1.5

* Clarify that deprecated SassColor getters (e.g. `red`, `blue`, etc.) convert
  color to a legacy space before returning channel value.

## Draft 1.4

* In `change`, adjust algorithm for differentiating `hwb` from `hsl` when only
  `hue` and no `space` is specified.

* In `change` for legacy colors, emit a `color-4-api` warning if a non-alpha
  channel is explicitly null and no space is set.

* In procedure for Changing a Component Value, specify that `undefined` values
  should return the `initialValue`.

* `toSpace` uses `Converting a Color` algorithm instead of `color.to-space()` to
  avoid removing missing channels when converting to a legacy space.

* In `change` and constructors, throw an error for alpha and lightness values
  that are out of range.

## Draft 1.3

* Rename new Embedded Protocol message from `SassColor` to `Color`.

* Make `color2` a positional parameter of `interpolate`, not an option.

* Add `rec2020` color space.

## Draft 1.2

* Add "alpha" to all channel name types.

* Remove `isAlphaMissing` in favor of `isChannelMissing("alpha")`.

* Rename types using title-case for acronyms longer than two letters in
  camel-case identifiers (e.g. `ColorSpaceHsl` instead of `ColorSpaceHSL`).

* Remove generic `change` overload, and make `space` optional on others.

* Return `immutable` types for `channels` and `channelsOrNull`, and remove
  assumption of 3 channels.

## Draft 1.1

* Clarify values in `channels` and `channelsOrNull`.

* Throw an error if construction space can not be determined.

* Remove `alpha` from list of deprecated getters.

* Rename types: `ColorSpaceLAB` to `ColorSpaceLab`, `ChannelNameLAB` to
  `ChannelNameLab`.

* Use `Exclude<>` instead of `Omit<>` for union types.

* Make procedure for determining space backwards compatible when using `change`
  for legacy colors.

* Fix channel names for `change` with `oklch` and `lch`.

## Draft 1

* Initial draft
