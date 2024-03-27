# Numbers

## Table of Contents

* [Definitions](#definitions)
  * [Double](#double)
  * [Degenerate Number](#degenerate-number)
  * [Conversion Factors](#conversion-factors)
  * [Set of Units](#set-of-units)
  * [Compatible Units](#compatible-units)
  * [Possibly-Compatible Units](#possibly-compatible-units)
  * [Possibly-Compatible Numbers](#possibly-compatible-numbers)
  * [Known Units](#known-units)
  * [Exact Equality](#exact-equality)
  * [Fuzzy Equality](#fuzzy-equality)
  * [Integer](#integer)
* [Types](#types)
  * [Operations](#operations)
    * [Equality](#equality)
    * [Greater Than or Equal To](#greater-than-or-equal-to)
    * [Less Than or Equal To](#less-than-or-equal-to)
    * [Greater Than](#greater-than)
    * [Less Than](#less-than)
    * [Addition](#addition)
    * [Subtraction](#subtraction)
    * [Multiplication](#multiplication)
    * [Modulo](#modulo)
    * [Negation](#negation)
  * [Serialization](#serialization)
* [Procedures](#procedures)
  * [Converting a Number to a Unit](#converting-a-number-to-a-unit)
  * [Matching Two Numbers' Units](#matching-two-numbers-units)
  * [Simplifying a Number](#simplifying-a-number)
  * [Converting a Number to a Calculation](#converting-a-number-to-a-calculation)

## Definitions

### Double

A *double* is a floating-point datum representable in a format with

* `b = 2`
* `p = 53`
* `emax = 1023`

as defined by [IEEE 754 2019], §3.2-3.3.

[IEEE 754 2019]: https://ieeexplore.ieee.org/document/8766229

> This is the standard 64-bit floating point representation, defined as
> `binary64` in [IEEE 754 2019], §3.6.

### Degenerate Number

The doubles `Infinity`, `-Infinity`, and `NaN` are *degenerate*.

A number is *degenerate* if its value is degenerate.

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

### Set of Units

A *set of units* is structure with:

* A list of strings called "numerator units".
* A list of strings called "denominator units".

When not otherwise specified, a single unit refers to numerator units containing
only that unit and empty denominator units.

### Compatible Units

Two numbers' units are said to be *compatible* if both:

* There's a one-to-one mapping between those numbers' numerator units such that
  each pair of units is either identical, or both units have a [conversion
  factor] and those two conversion factors have the same unit. This mapping is
  known as the numbers' *numerator compatibility map*.

* There's the same type of mapping between those numbers' denominator units.
  This mapping is known as the numbers' *denominator compatibility map*.

[conversion factor]: #conversion-factors

Similarly, a number is *compatible with* a [set of units] if it's compatible
with a number that has those units; and two sets of units are *compatible* if a
number with one set is compatible with a number with the other.

[set of units]: #set-of-units

### Possibly-Compatible Units

Two units are *possibly-compatible* with one another if and only if either both
units appear in the same row in the following table, or either unit doesn't
appear in the following table. Units are matched case-insensitively to determine
possible-compatibility.

> This is intended to be kept in sync with the unit types in [CSS Values and
> Units]. Note that all unknown units are possibly-compatible with all other
> units; this preserves forwards-compatibility with new units that are
> introduced in browsers over time.

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

### Known Units

A number has *known units* unless it has unit `%`.

> This is relevant for calculations, because in plain CSS they resolve
> percentages before doing their operations. This means that any non-linear
> operations involving percentages must be passed through to plain CSS rather
> than handled by Sass.
>
> More complex units involving percentages are allowed because any non-linear
> function will throw for complex units anyway.

### Exact Equality

Two [doubles] are said to be *exactly equal* if they are equal according to the
`compareQuietEqual` predicate as defined by [IEEE 754 2019], §5.11.

[doubles]: #double

> This is as opposed to [fuzzy equality].
>
> [fuzzy equality]: #fuzzy-equality

### Fuzzy Equality

Two [doubles] are said to be *fuzzy equal* to one another if either:

* They are equal according to the `compareQuietEqual` predicate as defined
  by [IEEE 754 2019], §5.11.

* They are both finite numbers and the mathematical numbers they represent
  produce the same value when rounded to the nearest 1e⁻¹¹ (with ties away from
  zero).

### Integer

A SassScript number `n` is said to be an *integer* if there exists a
mathematical integer `m` with an exact [double] representation and `n`'s value
[fuzzy equals] that double.

If `m` exists, we say that `n`'s *integer value* is the double that represents
`m`.

[double]: #double
[fuzzy equals]: #fuzzy-equality

> To avoid ambiguity, specification text will generally use the term
> "mathematical integer" when referring to the abstract mathematical objects.

## Types

The value type known as a *number* has three components:

* A [double] called its "value".
* A list of strings called *numerator units*.
* A list of strings called *denominator units*.

Several shorthands exist when referring to numbers:

* A number's *units* refers to the [set of units] containing its numerator units
  and denominator units.

* A number is *unitless* if its numerator and denominator units are both empty.

* A number is *in a given unit* (such as "in `px`") if it has that unit as its
  single numerator unit and has no denominator units.

### Operations

#### Equality

Let `n1` and `n2` be two numbers. To determine `n1 == n2`:

* Let `c1` and `c2` be the result of [matching units] for `n1` and `n2`. If this
  throws an error, return false.

  [matching units]: #matching-two-numbers-units

* Return true if `c1`'s value [fuzzy equals] `c2`'s and false otherwise.

#### Greater Than or Equal To

Let `n1` and `n2` be two numbers. To determine `n1 >= n2`:

* Let `c1` and `c2` be the result of [matching units] for `n1` and `n2` allowing
  unitless.

* Return true if `c1`'s value [fuzzy equals] `c2`'s, or if
  `compareQuietGreaterEqual(c1.value, c2.value)` returns `true` as defined by
  [IEEE 754 2019], §5.11. Return false otherwise.

#### Less Than or Equal To

Let `n1` and `n2` be two numbers. To determine `n1 <= n2`:

* Let `c1` and `c2` be the result of [matching units] for `n1` and `n2` allowing
  unitless.

* Return true if `c1`'s value [fuzzy equals] `c2`'s, or if
  `compareQuietLessEqual(c1.value, c2.value)` returns `true` as defined by [IEEE
  754 2019], §5.11. Return false otherwise.

#### Greater Than

Let `n1` and `n2` be two numbers. To determine `n1 > n2`, return `n1 >= n2 and
n1 != n2`.

#### Less Than

Let `n1` and `n2` be two numbers. To determine `n1 < n2`, return `n1 <= n2 and
n1 != n2`.

#### Addition

Let `n1` and `n2` be two numbers. To determine `n1 + n2`:

* Let `c1` and `c2` be the result of [matching units] for `n1` and `n2` allowing
  unitless.

* Return a number whose value is the result of `addition(c1.value, c2.value)` as defined by
  [IEEE 754 2019], §5.4.1; and whose units are the same as `c1`'s.

#### Subtraction

Let `n1` and `n2` be two numbers. To determine `n1 - n2`:

* Let `c1` and `c2` be the result of [matching units] for `n1` and `n2` allowing
  unitless.

* Return a number whose value is the result of `subtraction(c1.value, c2.value)`
  as defined by [IEEE 754 2019], §5.4.1; and whose units are the same as `c1`'s.

#### Multiplication

Let `n1` and `n2` be two numbers. To determine `n1 * n2`:

* Let `product` be a number whose value is the result of
  `multiplication(n1.value, n2.value)` as defined by [IEEE 754 2019], §5.4.1;
  whose numerator units are the concatenation of `n1`'s and `n2`'s numerator
  units; and whose denominator units are the concatenation of `n1`'s and `n2`'s
  denominator units.

* Return the result of [simplifying] `product`.

  [simplifying]: #simplifying-a-number

#### Modulo

Let `n1` and `n2` be two numbers. To determine `n1 % n2`:

* Let `c1` and `c2` be the result of [matching units] for `n1` and `n2` allowing
  unitless.

* If `c2` is infinity and has a different sign than `c1` (including
  oppositely-signed zero), return NaN with the same units as `c1`.

  > This matches the behavior of CSS's `mod()` function.

* Let `remainder` be a number whose value is the result of `remainder(c1.value,
  c2.value)` as defined by [IEEE 754 2019], §5.3.1; and whose units are the same
  as `c1`'s.

* If `c2`'s value is less than 0 and `remainder`'s value isn't [exactly equal]
  to `0`, return `remainder - c2`.

  [exactly equal]: #exact-equality

  > This is known as [floored division]. It differs from the standard IEEE 754
  > specification, but matches the behavior of CSS's `mod()` function.
  >
  > Note: These comparisons are not the same as `c2 < 0` or `remainder == 0`,
  > because they don't do fuzzy equality.

  [floored division]: https://en.wikipedia.org/wiki/Modulo_operation#Variants_of_the_definition

* Otherwise, return `remainder`.

#### Negation

Let `number` be a number. To determine `-number`, return a number whose value is
the result of `negate(number)` as defined by [IEEE 754 2019], §5.5.1; and whose
units are the same as `number`'s.

### Serialization

To serialize a number to CSS:

* If the number has more than one numerator unit, or more than zero denominator
  units, throw an error.

* If the number is degenerate, [convert it to a calculation] then serialize that
  to CSS.

  [convert it to a calculation]: #converting-a-number-to-a-calculation

* Otherwise:

  * Emit a string that can be parsed as a [`<number-token>`] with the
    same value as the number.

  * If the number has a numerator unit, emit that unit.

  [`<number-token>`]: https://www.w3.org/TR/css-syntax-3/#typedef-number-token

## Procedures

### Converting a Number to a Unit

This algorithm takes a SassScript number `number` and a [set of units] `units`.
It returns a number with the given units. It's written "convert `number` to
`units`" or "convert `number` to `units` allowing unitless".

* If `number` is unitless and this procedure allows unitless, return
  `number` with `units`.

* Otherwise, if `number`'s units aren't [compatible with] `units`, throw an
  error.

  [compatible with]: #compatible-units

* Let `value` be `number`'s value.

* For each pair of units `u1`, `u2` in the [numerator compatibility
  map] between `number` and `units` such that `u1 != u2`:

  [numerator compatibility map]: #compatible-units

  * Let `v1` and `v2` be the values of `u1` and `u2`'s [conversion factors].

  * Set `value` to `division(multiplication(value, v1), v2)` as defined by
    [IEEE 754 2019], §5.4.1.

  [conversion factors]: #conversion-factors

* For each pair of units `u1`, `u2` in the [denominator compatibility map]
  between `number` and `units` such that `u1 != u2`:

  [denominator compatibility map]: #compatible-units

  * Let `v1` and `v2` be the values of `u1` and `u2`'s [conversion factors].

  * Set `value` to `division(multiplication(value, v2), v1)` as defined by
    [IEEE 754 2019], §5.4.1.

* Return a number with value `value` and units `units`.

### Matching Two Numbers' Units

This algorithm takes two SassScript numbers `n1` and `n2` and returns two
numbers. It's written "match units for `n1` and `n2`" or "match units for `n1`
and `n2` allowing unitless".

* If `n1` is unitless and this procedure allows unitless, return `n1`
  with the same units as `n2` and `n2`.

* Otherwise, if `n2` is unitless and this procedure allows unitless, return `n1`
  and `n2` with the same units as `n1`.

* Return `n1` and the result of [converting `n2` to `n1`'s units].

  [converting `n2` to `n1`'s units]: #converting-a-number-to-a-unit

### Simplifying a Number

This algorithm takes a SassScript number `number` and returns an equivalent
number with simplified units.

* Let `mapping` be a one-to-one mapping between `number`'s numerator units and
  its denominator units such that each pair of units is either identical, or
  both units have a [conversion factor] and those two conversion factors have
  the same unit.

* Let `newUnits` be a copy of `number`'s units without any of the units in
  `mapping`.

  > `newUnits` for `1px*px/px` is `px`, because only one of the numerator `px`
  > is included in the mapping.

* Return the result of [converting `number` to `newUnits`].

  [converting `number` to `newUnits`]: #converting-a-number-to-a-unit

### Converting a Number to a Calculation

Given a number `number`, this procedure returns a CSS-compatible calculation
that represents the same numeric value.

* If `number`'s value is `Infinity`, let `value` be an `UnquotedString` whose
  `value` is `'infinity'`.

* Otherwise, if `number`'s value is `-Infinity`, let `value` be an
  `UnquotedString` whose `value` is `'-infinity'`.

* Otherwise, if `number`'s value is `NaN`, let `value` be an `UnquotedString`
  whose `value` is `'NaN'`.

* Otherwise, let `value` be a `CalculationValue` whose value is `number` without
  units.

* For each unit `unit` in `number`'s numerator units:

  * Set `value` to a `CalculationOperation` with `operator` set to `'*'`, `left`
    set to `value`, and `right` set to a number with value 1 and unit `unit`.

* For each unit `unit` in `number`'s denominator units:

  * Set `value` to a `CalculationOperation` with `operator` set to `'/'`, `left`
    set to `value`, and `right` set to a number with value 1 and unit `unit`.

* Return a `Calculation` with `name` set to `'calc'` and arguments set to
  `[value]`.

> Currently the logic for serializing multiple numerator or denominator units is
> unused, but it's likely to be useful later when determining whether/how to
> serialize numbers with complex units.
