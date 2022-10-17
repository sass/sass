## Draft 1.2

* Clamp `hsl` saturation & lightness when generating `hsl` colors.

* Ensure that color space names are unquoted strings, and normalize them before
  creating colors or comparing color space names.

* Allow channel adjustment values to be out-of-gamut, and then normalize the
  resulting channel values. This allows more flexibility, while ensuring that
  `hsl` or `hwb` clamp out-of-gamut results.

* Channel clamping and scaling for `hsl` and `hwb` colors is handled in the
  normalization process, rather than the individual functions. This also allows
  it to happen when normalizing the results of color manipulation or conversion.

* Add missing `$weight` to the `color.invert()` signature, and return early
  when the specified weight is `0%` or `100%`.

## Draft 1.1

* Expand the summary section to describe more of the proposal.

## Draft 1

* Initial draft
