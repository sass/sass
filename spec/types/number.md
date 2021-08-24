# Numbers

## Table of Contents

* [Definitions](#definitions)
  * [Conversion Factors](#conversion-factors)
  * [Compatible Units](#compatible-units)
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

Two units are said to be "compatible" if they are identical or if their
[conversion factors] have the same unit.

[conversion factors]: #conversion-factors

### Potentially Slash-Separated Number

A Sass number may be *potentially slash-separated*. If it is, it is associated
with two additional Sass numbers, the *original numerator* and the *original
denominator*. A number that is not potentially slash-separated is known as
*slash-free*.

A potentially slash-separated number is created when a `ProductExpression` with
a `/` operator is evaluated and both operands are *syntactically* either literal
`Number`s or `ProductExpression`s that can themselves create potentially
slash-separated numbers. In this case, both operands are guaranteed to be
evaluated as numbers. The first operand is the original numerator of the
potentially slash-separated number returned by the `/` operator, and the second
is the original denominator.

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
