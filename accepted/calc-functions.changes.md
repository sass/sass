## Draft 2.1

* Allow custom functions named like vendor-prefixed new CSS functions.

## Draft 2.0

* Explicitly forbid user-defined functions with the same names as CSS math
  functions.

* Add a deprecation process for gradually phasing out user-defined functions
  with name conflicts.

## Draft 1.5

* Fix the definition of `rem()` to use `result - modulus` rather than `result -
  dividend` in the case where the argument signs differ.

## Draft 1.4

* Don't exempt percentages from most functions' simplification logic, since
  those functions don't allow _any_ units in CSS, including percents. Now only
  `abs()`, `sign()`, `atan2()`, and `hypot()` check for known units because CSS
  allows percentages for them but they aren't linear so they can't be resolved
  in terms of percentages in Sass.

* Throw an error if `clamp()` or `hypot()` has known-incompatible arguments even
  if they aren't all numbers.

* Throw an error if any argument to `pow()` or `log()` is a number with units,
  even if the function can't otherwise be simplified.

* Add non-normative notes clarifying when units are implicitly disallowed by
  passing them on to built-in Sass functions.

* Clarify that `log()` can invoke the built-in Sass function with one or two
  arguments.

* Remove duplicated text checking for too few arguments for `pow()` and
  `atan2()`.

## Draft 1.3

* Update the definition of potentially slash-separated numbers to exclude
  calc functions that were previously evaluated as Sass global functions.

## Draft 1.2

* Fix behavior of `round` to ensure it is backwards-compatible with the existing
  global Sass function.

## Draft 1.1

* `mod`, `rem`, `atan2`, or `pow` should take two arguments instead of three.

## Draft 1

* Initial draft
