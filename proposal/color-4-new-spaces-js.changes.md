## Draft 1.1

* Clarify values in `channels` and `channelsOrNull`.

* Throw an error if construction space can not be determined.

* Remove `alpha` from list of deprecated getters.

* Rename types: `ColorSpaceLAB` to `ColorSpaceLab`, `ChannelNameLAB` to
  `ChannelNameLab`.

* Use `Exclude<>` instead of `Omit<>` for union types.

* Update `change` for legacy colors:

  * Make procedure for determining space backwards compatible
  
  * If channels from multiple spaces are specified and no space is specified,
    emit a deprecation warning instead of throwing an error to prevent a
    breaking change.

* Fix channel names for `change` with `oklch` and `lch`.

* Throw an error if changing a non-legacy color without specifying a space.

## Draft 1

* Initial draft
