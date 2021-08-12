## Draft 2

* Store interpolations as a separate data type so that they can be parenthesized
  when used in `CalculationOperation`s.

* Throw errors when combining units that are known to be incompatible.

* Allow variables in `CalcValue`s to return calculations.

* Define equality between calculations.

* Properly parenthesize the right-hand side of `a / (b * c)`.

* Return calculations from `meta.calc-args()` as calculations, not unquoted
  strings.

* Simplify `calc(a + -b)` to `calc(a - b)`.

* Fix some broken formatting.

* Remove TODOs about extra simplification.

## Draft 1

* Initial draft.
