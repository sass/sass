## Draft 2.1

* Forbid `"and"` or `"or"` tokens at the beginning of `InterpolatedAnyValue` in
  `SupportsAnything`. This makes more explicit the fact that the
  `SupportsCondition` parsing takes precedence.

## Draft 2

* Mark the `InterpolatedAnyValue` productions as optional. According to Tab
  Atkins, this matches the intended (although not the written) syntax of the
  CSS spec.

* Add `Interpolation` as an option for `SupportsInParens`, for
  backwards-compatibility with existing Sass behavior.

## Draft 1

* Initial draft.
