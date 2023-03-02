# Calculation Constants: Draft 1

*([Issue](https://github.com/sass/sass/issues/3258))*

This proposal adds support for constant names in CSS calculations.

## Table of Contents

* [foo]()

## Background

> This section is non-normative.

Since Sass now fully parses calculations and resolves them where necessary, it's
necessary to keep up with new syntactic features added in plain CSS. CSS Values
4 adds support for several [numeric constants], written as identifiers in the
calculation, which Sass should support as well.

[numeric constants]: https://www.w3.org/TR/css-values-4/#calc-constants

## Summary

Calculations may now contain arbitrary identifiers in any value position. The
specific identifiers `pi`, `e`, `infinity`, `-infinity`, and `NaN` will be
parsed to their corresponding double values.

In addition, this creates for the first time a clear and standard CSS way of
representing degenerate numeric constants like `NaN` and the infinities. This
allows us to serialize a Sass number with value `Infinity`, `-Infinity`, or
`NaN` to `calc(infinity)`, `calc(-inifinity)`, and `calc(NaN)`, respectively.

### Design Decisions

#### Arbitrary Identifiers

We're choosing to support arbitrary identifiers in calculations, rather than
just those specified in CSS Values 4, for forwards-compatibility. While it's
unclear whether the `<calc-constant>` production itself will be expanded, it's
likely that calculations will begin supporting additional non-constant values
depending on context (such as in relative color functions like `rgb(from
var(--color) r g calc(b * 1.5))`).

### Serializing Complex Units

The addition of the [Converting a Number to a Calculation] procedure gives a
possibility, in principle, for representing numbers with complex units in a
standard CSS format rather than throwing an "invalid CSS value" error. However,
no browser yet supports complex unit calculations, so this would likely just
silence real errors without providing any actual value.

[Converting a Number to a Calculation]: #converting-a-number-to-a-calculation

Whether and how we want to support this once browsers _can_ parse it is a
question for another time.

## Definitions

### Degenerate Number

The doubles `Infinity`, `-Infinity`, and `NaN` are _degenerate_.

A number is _degenerate_ if its value is degenerate.

## Syntax

### `CalculationExpression`

This proposal adds <code>| [\<ident-token>]</code> to the `CalcValue`
production.

[\<calc-constant>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram

## Types

### Calculations

Rename `CalculationInterpolation` to `CalculationRaw`.

> This reflects the fact that this will now be produced by identifiers that are
> not interpolated.

## Semantics

### `CalcValue`

Add the following to the existing semantics for `CalcValue`:

* If `value` is case-insensitively equal to `pi`, return 3.141592653589793.

  > This is the closest double approximation of the mathematical constant π.

* If `value` is case-insensitively equal to `e`, return 2.718281828459045.

  > This is the closest double approximation of the mathematical constant e.

* If `value` is case-insensitively equal to `infinity`, return the double
  `Infinity`.

* If `value` is case-insensitively equal to `-infinity`, return the double
  `-Infinity`.

* If `value` is case-insensitively equal to `nan`, return the double `NaN`.

* If `value` is any other `<identifier>`, return a `CalcuationRaw` with `value`.

## Serialization

### Calculations

#### `Number`

To serialize a `Number` within a `CalculationExpression`:

* If the number is [degenerate], [convert it to a calculation], then serialize
  the resulting calculation's sole argument.

  [degenerate]: #degenerate-number
  [convert it to a calculation]: #converting-a-number-to-a-calculation

* Otherwise, serialize the number as normal.

#### `CalculationOperation`

Add another possible condition for parenthesizing the right value:

* the operator is `"+"`, `"-"`, or `"/"` and the right value is a degenerate
  number with one or more units.

### Numbers

To serialize a number to CSS:

* If the number has more than one numerator unit, or more than zero denominator
  units, throw an error.

* If the number is degenerate, [convert it to a calculation] then serialize that
  to CSS.

* Otherwise:

  * Emit a string that can be parsed as a [`<number-token>`] with the
    same value as the number.

    [`<number-token>`]: https://www.w3.org/TR/css-syntax-3/#typedef-number-token

  * If the number has a numerator unit, emit that unit.

## Procedures

### Converting a Number to a Calculation

Given a number `number`, this procedure returns a CSS-compatible calculation
that represents the same numeric value.

* If `number` is `Infinity`, let `value` be a `CalculationRaw` whose `value` is
  `'infinity'`.

* Otherwise, if `number` is `-Infinity`, let `value` be a `CalculationRaw` whose
  `value` is `'-infinity'`.

* Otherwise, if `number` is `NaN`, let `value` be a `CalculationRaw` whose
  `value` is `'NaN`.

* Otherwise, let `value` be a `CalculationValue` whose value is `number` without
  units.

* For each unit `unit` in `number`'s numerator units:

  * Set `value` to a `CalculationOperation` with `operator` set to `'*'`, `left`
    set to `value`, and `right` set to a number with value 1 and unit `unit`.

* For each unit `unit` in `number`'s denominator units:

  * Set `value` to a `CalculationOperation` with `operator` set to `'/'`, `left`
    set to `value`, and `right` set to a number with value 1 and unit `unit`.

* Return a `Calculation` with `name` set to `'calc'` and arguments set to
  `[calculation]`.

> Currently the logic for serializing multiple numerator or denominator units is
> unused, but it's likely to be useful later when determining whether/how to
> serialize numbers with complex units.
