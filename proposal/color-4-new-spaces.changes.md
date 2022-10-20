## Draft 1.2

* Clamp `hsl` saturation & lightness when generating `hsl` colors, and as part
  of the color conversion process

* Ensure that color space names are unquoted strings, and normalize them before
  creating colors or comparing color space names.

* Allow channel adjustment values to be out-of-gamut, and then normalize the
  resulting channel values. This allows more flexibility, while ensuring that
  `hsl` or `hwb` clamp out-of-gamut results.

* Channel clamping and scaling for `hsl` and `hwb` colors is handled in the
  normalization process, rather than the individual functions. This also allows
  it to happen when normalizing the results of color manipulation or conversion.

* Throw an error in the color component parsing procedure if a known color space
  is one of the components, and has a function of it's own (e.g. `rgb` or
  `oklch`). Only custom color spaces and predefined spaces can be defined using
  the `color(<space> <channels>)` syntax.

* Add missing `$weight` to the `color.invert()` signature, and return early
  when the specified weight is `0%` or `100%`.

* Update the color interpolation procedure handling of `weight` values to error
  when `weight` is outside the `[0,1]` range, and return early when `weight` is
  equal to 0 or 1.

* Channel values are indexed like other Sass lists, allowing both positive and
  negative-indexed access in `color.channel()`.

* For backwards compatibility, the `color.change()`, `color.scale()`, and
  `color.adjust()` functions allow manipulating legacy colors in any legacy
  space, if the `$space` argument is not explicitly set.

* Remove `in` prefix from the color interpolation method syntax, since the Sass
  function syntax is already explicit about which parameter is where.

* `color.invert()` throws an error when `$weight` would require mixing in an
  invalid `color.mix()` _interpolation color space_.

## Draft 1.1

* Expand the summary section to describe more of the proposal.

## Draft 1

* Initial draft
