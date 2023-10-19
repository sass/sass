## Draft 1.4

* In `change`, adjust algorithm for differentiating `hwb` from `hsl` when only
  `hue` and no `space` is specified.

* In procedure for Changing a Component Value, specify that `undefined` values
  should return the `initialValue`.

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
