## Draft 1.4

* Colors conversion is only performed when necessary. Previously, colors could
  be converted into their current space.

* Color conversion procedure explicitly handles 'carrying forward' missing
  channels when converting to a space with an analogous component. This was
  previously only applied to interpolation.

* Allow all color spaces to be used for hue interpolation.

* Remove `specified` hue interpolation method, and normalize hues to be in the
  `[0,360]` range.

## Draft 1.3

* Deprecate the `color.alpha()` function along with the other legacy channel
  access functions.

* Require quoted strings for channel names in `color.is-powerless()` and
  `color.channel()`, to avoid syntax conflicts between `rgb` channel names and
  their respective named colors (e.g. `'red'` the channel vs `red` the color).

* Define how deprecated functions behave as alias functions during the
  deprecation process.

## Draft 1.2

* Clamp `hsl` saturation & lightness when generating `hsl` colors, and gamut-map
  when converting colors into either `hsl` or `hwb`, since those spaces cannot
  properly maintain out-of-gamut color values.

* Ensure that color space names are unquoted strings, and compared insensitive
  to case.

* Remove support for custom or unknown color spaces. There are too many open
  questions in the CSS spec, as browsers have not started to implement this
  feature yet.

* Remove channel indexing, and syntax to access channels by index, since all
  known color channels have names.

* Allow channel adjustment values to be out-of-gamut, and then normalize the
  resulting channel values. This allows more flexibility, while ensuring that
  `hsl` or `hwb` clamp out-of-gamut results.

* Channel clamping and scaling for `hsl` and `hwb` colors is handled in the
  normalization process, rather than the individual functions. This also allows
  it to happen when normalizing the results of color manipulation.

* Throw an error in the color component parsing procedure if a known color space
  is one of the components, and has a function of it's own (e.g. `rgb` or
  `oklch`). Only custom color spaces and predefined spaces can be defined using
  the `color(<space> <channels>)` syntax.

* Add missing `$weight` to the `color.invert()` signature, and return early
  when the specified weight is `0%` or `100%`.

* Update the color interpolation procedure handling of `weight` values to error
  when `weight` is outside the `[0,1]` range, and return early when `weight` is
  equal to 0 or 1.

* For backwards compatibility, the `color.change()`, `color.scale()`, and
  `color.adjust()` functions allow manipulating legacy colors in any legacy
  space, if the `$space` argument is not explicitly set.

* Remove `in` prefix from the color interpolation method syntax, since the Sass
  function syntax is already explicit about which parameter is where.

* `color.invert()` throws an error when `$weight` would require mixing in an
  invalid `color.mix()` _interpolation color space_.

* Allow scaling channels with a non-0 minimum value, such as the `a` and `b`
  channels in `lab()`/`oklab()`.

* Ensure that percentage and percentage-mapped number values are normalized
  before they are added together in `color.adjust()`.

* Clarify that channel values are stored as raw doubles, and add/remove units
  as necessary for normalization/serialization.

* Legacy colors with missing channels are serialized using the non-legacy
  serialization logic. When converting colors into legacy spaces with
  `color.to-space()`, all missing components are replaced with `0` for better
  legacy output.

* `color.channel()` returns `0` when the channel value is missing, rather than
  throwing an error.

* Added `color.is-missing($color, $channel)` to inspect if a channel is set to
  'none' (e.g. missing).

* Legacy colors using a space-separated syntax with special number values that
  are not adjacent to a `/` symbol are emitted using the legacy
  (comma-separated) CSS syntax. For example:

  * `hsl(20deg 5% var(--foo))` emits `hsl(20deg, 5%, var(--foo))`.

  * `hsl(20deg var(--foo) 5% / 0.5)` emits `hsl(20deg, var(--foo), 5%, 0.5)`.

  * `hsl(20deg 5% var(--foo) / 0.5)` emits `hsl(20deg 5% var(--foo)/0.5)`
    since the special value is adjacent to the slash.

## Draft 1.1

* Expand the summary section to describe more of the proposal.

## Draft 1

* Initial draft
