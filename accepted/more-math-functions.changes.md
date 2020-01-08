## Draft 2.2

* For consistency, all functions that have cases for `-0` also have cases for
  `0`. This includes `sqrt()`, `sin()`, `tan()`, `asin()`, and `atan()`.

* `hypot()`'s arguments are named `$numbers` for consistency.

## Draft 2.1

* atan2()'s arguments must all have compatible units, or all be unitless.

## Draft 2

* Variables
  * `$e` and `$pi` have 1 more digit of precision after the decimal.
  * Variables from built-in modules cannot be modified.

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

* Output units:
  * For `acos()`, `asin()`, and `atan()`, and `atan2()`, all of their outputs
    are numbers in `deg`.

## Draft 1.1

* Added Background and Summary sections.

## Draft 1

* Initial draft.
