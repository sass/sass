## Draft 1.2

* Clamp `hsl` saturation & lightness when generating `hsl` colors.

* Ensure that color space names are unquoted strings, and normalize them before
  creating colors or comparing color space names.

* Allow channel adjustment values to be out-of-gamut, and then normalize the
  resulting channel values. This allows more flexibility, while ensuring that
  `hsl` or `hwb` clamp out-of-gamut results.

* Add missing `$weight` to the `color.invert()` signature, and return early
  when the specified weight is `0%` or `100%`.

## Draft 1.1

* Expand the summary section to describe more of the proposal.

## Draft 1

* Initial draft
