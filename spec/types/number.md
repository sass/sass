# Numbers

## Table of Contents

* [Definitions](#definitions)
  * [Conversion Factors](#conversion-factors)
  * [Compatible Units](#compatible-units)
  * [Possibly-Compatible Units](#possibly-compatible-units)
  * [Possibly-Compatible Numbers](#possibly-compatible-numbers)
  * [Potentially Slash-Separated Number](#potentially-slash-separated-number)
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

Two numbers' units are said to be *compatible* if:

* there's a one-to-one mapping between those numbers' numerator units such
  that each pair of units is either identical, or both units have a [conversion
  factor] and those two conversion factors have the same unit; and

* there's the same type of mapping between those numbers' denominator units.

[conversion factors]: #conversion-factors

Similarly, a number is *compatible with* a set of units if it's compatible with
a number that has those units; and two sets of units are *compatible* if a
number with one set is compatible with a number with the other.

### Possibly-Compatible Units

Two units are *possibly-compatible* with one another if and only if either both
units appear in the same row in the following table, or either unit doesn't
appear in the following table. Units are matched case-insensitively to determine
possible-compatibility.

> This is intended to be kept in sync with the unit types in [CSS Values and
> Units]. Note that all unknown units are possibly-compatible with all other
> units; this preserves forwards-compatibility with new units that are
> introduced in browsers over time.

[CSS Values and Units]: https://www.w3.org/TR/css-values-3/

| Type           | Units                                                                                        |
| -------------- | -------------------------------------------------------------------------------------------- |
| `<length>`     | `em`, `ex`, `ch`, `rem`, `vw`, `vh`, `vmin`, `vmax`, `cm`, `mm`, `Q`, `in`, `pt`, `pc`, `px` |
| `<angle>`      | `deg`, `grad`, `rad`, `turn`                                                                 |
| `<time>`       | `s`, `ms`                                                                                    |
| `<frequency>`  | `Hz`, `kHz`                                                                                  |
| `<resolution>` | `dpi`, `dpcm`, `dppx`                                                                        |

### Possibly-Compatible Numbers

Two numbers are *possibly-compatible* if there's a one-to-one mapping between
their numerator units, and another such mapping between their denominator units,
such that each pair of units is [possibly-compatible](#possibly-compatible-units).
Two numbers are *definitely-incompatible* if they are not possibly-compatible.

> The definition of definite-incompatibility captures the notion of numbers that
> can be determined at build time to be incompatible with one another, and thus
> erroneous to ever combine. This allows us to eagerly produce error messages
> for certain incompatible units rather than serving them to the browser where
> they're much more difficult to debug.
>
> For example, `1px` is possibly-compatible with `2em`. Unitless numbers are
> only possibly-compatible with other unitless numbers. In theory, this
> definition defines a notion of possible-compatiblity for numbers with more
> complex units, but in practice these numbers are already flagged as errors
> prior to any possible-compatibility checks.

### Potentially Slash-Separated Number

A Sass number may be *potentially slash-separated*. If it is, it is associated
with two additional Sass numbers, the *original numerator* and the *original
denominator*. A number that is not potentially slash-separated is known as
*slash-free*.

A potentially slash-separated number is created when a `ProductExpression` with
a `/` operator is evaluated and both operands are *syntactically* one of the
following:

* `Number`s,
* [`Calculation`]s, or
* `ProductExpression`s that can themselves create potentially slash-separated
  numbers.

[`Calculation`]: calculation.md#syntax
  
If both operands are evaluated as numbers, the resulting number is potentially
slash-separated. The first operand is the original numerator of the potentially
slash-separated number returned by the `/` operator, and the second is the
original denominator.

A potentially slash-separated number is converted to a slash-free number when:

* It is the value of a `ParenthesizedExpression`.

  > That is, it's in parentheses, such as in `(1 / 2)`. Note that if it's in a
  > list that's in parentheses, it's *not* converted to a slash-free number.

* It is stored in a Sass variable.

* It is passed to a function or mixin.

* It is returned by a function.

> Any expressions that normally produce a new number (such as other mathematical
> operations) always produce slash-free numbers, even when their arguments are
> slash-separated.
>
> When a potentially slash-separated number is "converted" to a slash-free
> number, a slash-free copy is made of the original. Sass values are always
> immutable.

When a potentially slash-separated number is converted to CSS, either when
converted to a string via interpolation or when included in a declaration's
value, it is written as the original numerator followed by `/` followed by the
original denominator. If either the original numerator or denominator are
themselves slash-separated, they're also written this way.

## Procedures

### Converting a Number to a Unit

This algorithm takes a SassScript number `number` and a unit `unit`. It's
written "convert `number` to `unit`" or "convert `number` to `unit` allowing
unitless". It returns a number with the given unit. The `unit` argument must
have a conversion factor.

* If `number` has no units:

  * If this procedure allows unitless, return `number` with unit `unit`.

  * Otherwise, throw an error.

* If `number`'s unit is `unit`, return `number` as-is.

* Let `number-factor` be `number`'s unit's conversion factor. If `number`
  doesn't have a unit or that unit doesn't have a conversion factor, throw an
  error.

* Let `unit-factor` be `unit`'s conversion factor.

* If `number-factor` and `unit-factor` don't have the same unit, throw an error.

* Let `result` be the result of multiplying `number`'s numeric component by
  `number-factor`'s, then dividing by `unit-factor`'s numeric component.

* Return `result` with unit `unit`.
