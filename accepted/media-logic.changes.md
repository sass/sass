## Draft 1.1

* In the `MediaQuery` production, don't allow an `Interpolation` to be followed
  by `(MediaAnd* | MediaOr*)` since `Interpolation` is ambiguous with
  `MediaType`.

* Forbid whitespace in the `MediaNot`, `MediaAnd`, and `MediaOr` productions.

* Fix the link for `CssMediaQuery`.

## Draft 1

* Initial draft.
