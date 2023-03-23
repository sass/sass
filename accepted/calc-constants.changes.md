## Draft 1.1

* Throw an error when serializing a degenerate number with complex units in a
  calculation.

* Clarify that we're checking for degenerate _values_ in "Converting a Number to
  a Calculation".

* Use `UnquotedString`s to represent unknown calculation constants rather than
  `CalculationInterpolation`s/`CalculationRaw`s.

* Don't parenthesize the right-hand side of `"+"` or `"-"`
  `CalculationOperation`s when the right-hand side is a degenerate number with
  one or more units. Degenerate numbers are serialized to multiplication and
  division expressions, which always have higher precedence than addition and
  subtraction.

## Draft 1

* Initial draft.
