## Draft 2

* Add a design decision section on the choice to return numbers from simplified
  calculations.

* Store interpolations as a separate data type so that they can be parenthesized
  when used in `CalculationOperation`s.

* Throw errors when combining units that are known to be incompatible.

* Allow variables in `CalcValue`s to return calculations.

* Define equality between calculations.

* Allow calculations in CSS color functions.

* Properly parenthesize the right-hand side of `a / (b * c)`.

* Return calculations from `meta.calc-args()` as calculations, not unquoted
  strings.

* Return an unquoted string with value `"calculation"` from `meta.type-of()` for
  calculations, rather than a quoted string with value `"calc"`.

* Allow calls to the global `min()` and `max()` functions in calculations.

* Allow `clamp(var(--three-args))`.

* Simplify `calc(a + -b)` to `calc(a - b)`.

* Explicitly specify how calculations are parsed in plain-CSS mode.

* Allow numbers returned by simplifying calculations to create potentially
  slash-separated numbers.

* Fix some broken formatting.

* Remove TODOs about extra simplification.

## Draft 1

* Initial draft.
