## Draft 2

* `$e` and `$pi` have 1 more digit of precision after the decimal.

* `Infinity` and `-Infinity`:
  * If any argument to `hypot()` equals `-Infinity`, it returns `Infinity`.
  * The `$exponent == Infinity` case in `pow()` also holds for
    `$exponent == -Infinity`.
  * The `$number == Infinity` cases in `cos()`, `sin()`, and `tan()` also hold
    for `$number == -Infinity`.

* Input units:
  * `clamp()`'s arguments must all have compatible units, or all be unitless.
  * `log()` does not error unless the input has units, and instead delegates
    edge cases to division.
  * `tan()` differentiates inputs in `rad` vs `deg`, since `90deg` cannot be
    precisely expressed in `rad`. E.g. `tan(90deg)` outputs `Infinity` whereas
    `tan(0.5rad * pi)` outputs `16331239353195370`.

* Output units:
  * For `acos()`, `asin()`, and `atan()`, and `atan2()`, all of their outputs
    are numbers in `deg`.

## Draft 1.1

* Added Background and Summary sections.

## Draft 1

* Initial draft.
