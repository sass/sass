# Numbers

## Table of Contents

* [Definitions](#definitions)
  * [Conversion Factors](#conversion-factors)
  * [Compatible Units](#compatible-units)
* [Procedures](#procedures)
  * [Converting a Number to a Unit](#converting-a-number-to-a-unit)

## Definitions

### Conversion Factors

Certain units have conversion factors that define how they can be converted to
and used with other related units. A conversion factor is itself a Sass number.
The following conversion factors are defined:

> These are intended to be kept in sync with the units in [CSS Values and Units]
> that are fixed in relation to each other. Each conversion factor is the
> equivalent of `1${unit}` in a canonical unit.

[CSS Values and Units]: https://www.w3.org/TR/css-values-3/

* `px`: 1 `px`
* `cm`: 96/2.54 `px`
* `mm`: 96/25.4 `px`
* `Q`: 96/101.6 `px`
* `in`: 96 `px`
* `pc`: 16 `px`
* `pt`: 4/3 `px`

* `deg`: 1 `deg`
* `grad`: 9/10 `deg`
* `rad`: 180/π `deg`
* `turn` 360 `deg`

* `ms`: 1 `ms`
* `s`: 1000 `ms`

* `Hz`: 1 `Hz`
* `kHz`: 1000 `Hz`

* `dppx`: 1 `dppx`
* `dpi`: 1/96 `dppx`
* `dpcm`: 2.54/96 `dppx`

### Compatible Units

Two units are said to be "compatible" if their [conversion factors] have the
same unit.

[conversion factors]: #conversion-factors

## Procedures

### Converting a Number to a Unit

This algorithm takes a SassScript number `number` and a unit `unit`. It's
written "convert `number` to `unit`" or "convert `number` to `unit` allowing
unitless". It returns a number with the given unit. The `unit` argument must
have a conversion factor.

* If `number` has no units:

  * If this procedure allow unitless, return `number` with unit `unit`.

  * Otherwise, throw an error.

* Let `number-factor` be `number`'s unit's conversion factor. If `number`
  doesn't have a unit or that unit doesn't have a conversion factor, throw an
  error.

* Let `unit-factor` be `unit`'s conversion factor.

* If `number-factor` and `unit-factor` don't have the same unit, throw an error.

* Let `result` be the result of multiplying `number`'s numeric component by
  `number-factor`'s, then dividing by `unit-factor`'s numeric component.

* Return `result` with unit `unit`.
