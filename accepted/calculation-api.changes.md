## Draft 3

* Make `CalculationOperation` and `CalculationInterpolation` concrete rather
  than abstract classes.
* Export `CalculationValue` and `CalculationOperator` types.
* Adjust `SassCalculation.clamp` to interpret comma-separated `min` values as
  valid input for `value` and `max`.

## Draft 2

* Simplify calculations at the point at which they're returned from the JS API,
  rather than eagerly when they're constructed.

## Draft 1

* Initial draft.
