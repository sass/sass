# Floating Point Numbers: Draft 1.1

*([Issue](https://github.com/sass/sass/issues/2892))*

This proposal standardizes Sass on using 64-bit floating-point numbers.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Potentially-Breaking Changes](#potentially-breaking-changes)
  * [Design Decisions](#design-decisions)
    * [Math Function Special Cases](#math-function-special-cases)
* [Definitions](#definitions)
  * [Double](#double)
  * [Set of Units](#set-of-units)
  * [Fuzzy Equality](#fuzzy-equality)
  * [Integer](#integer)
  * [Compatible Units](#compatible-units)
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
    * [Negation](#negation)
* [Procedures](#procedures)
  * [Converting a Number to Units](#converting-a-number-to-units)
  * [Matching Two Numbers' Units](#matching-two-numbers-units)
  * [Simplifying a Number](#simplifying-a-number)
* [Variables](#variables)
  * [`$e`](#e)
  * [`$pi`](#pi)
  * [`$epsilon`](#epsilon)
  * [`$max-safe-integer`](#max-safe-integer)
  * [`$min-safe-integer`](#min-safe-integer)
  * [`$max-number`](#max-number)
  * [`$min-number`](#min-number)
* [Functions](#functions)
  * [Bounding Functions](#bounding-functions)
    * [`math.ceil()`](#mathceil)
    * [`math.floor()`](#mathfloor)
    * [`math.round()`](#mathround)
  * [Distance Functions](#distance-functions)
    * [`math.abs()`](#mathabs)
  * [Exponential Functions](#exponential-functions)
    * [`math.log()`](#mathlog)
    * [`math.pow()`](#mathpow)
    * [`math.sqrt()`](#mathsqrt)
  * [Trigonometric Functions](#trigonometric-functions)
    * [`math.acos()`](#mathacos)
    * [`math.asin()`](#mathasin)
    * [`math.atan()`](#mathatan)
    * [`math.atan2()`](#mathatan2)
    * [`math.cos()`](#mathcos)
    * [`math.sin()`](#mathsin)
    * [`math.tan()`](#mathtan)
  * [Other Functions](#other-functions)
    * [`math.div()`](#mathdiv)

## Background

> This section is non-normative.

In the original Ruby Sass implementation, numbers were represented using Ruby's
numeric stack. If a number was written without a decimal point in Sass (or
returned by an integer-valued function like `red()`), it would be represented as
an arbitrary-sized integer type that would transparently support integers of
arbitrary size. If it was written with a decimal point (or returned by a
float-valued function like `random()`), it used Ruby's floating-point
representation whose size varied based on how Ruby was compiled.

LibSass varied from this behavior by representing all numbers as 64-bit
floating-point numbers.

Dart Sass initially matched Ruby Sass's implementation by virtue of the fact
that Dart versions before 2.0.0 supported a similar transparently-updating
integer stack. However, when Dart 2.0.0 was released its integer representation
instead became fixed-size, and only guaranteed to be fully accurate up to 53
bits.

In addition to the specific details of numeric representation, Ruby Sass papered
over floating-point numbers' [accuracy issues] by defining a heuristic for
determining when similar numbers were considered equivalent to Sass's logic.
This heuristic has persisted relatively unchanged through to modern
implementations, but it introduces a problematic [intransitivity] in Sass's
equality semantics: `1 == 1.000000000005` and `1.000000000005 ==
1.000000000010`, but `1 != 1.000000000010`. This also means that the hashing
Sass uses for its map keys is inherently flawed when dealing with numbers with
very small variations.

[intransitivity]: https://en.wikipedia.org/wiki/Transitive_relation

In practice, these changes rarely come up in practice because CSS tends to
involve numbers within the well-behaved ranges almost exclusively. However,
inconsistent edge cases can lead to severely bad user experiences as well as
difficulty writing truly robust library code.

## Summary

> This section is non-normative.

This proposal standardizes Dart Sass on 64-bit IEEE 754 floating-point numbers,
like Dart, Java, and C#'s `double` type and—most pertinently—like JavaScript's
`Number` type. There will no longer be a separate representation of integers and
floating-point numbers, again similarly to JavaScript. In practice this is not a
large change, because Sass has always treated integer-like floating-point
numbers interchangeably with integers anyway.

This proposal also rationalizes Sass's numeric equality heuristic to make it
transitive. In particularly, two numbers will be considered equivalent if they
round to the same 1e-11. Using the example above, this will mean that `1 !=
1.000000000005`, `1.000000000005 == 1.000000000010`, and `1 != 1.000000000010`.

This proposal also adds numeric constants to the `sass:math` module that
represent various boundaries when dealing with floating-point values:

* `math.$epsilon`: The difference between 1 and the smallest floating-point
  number greater than 1.

* `math.$max-safe-integer`: The maximum integer that can be represented "safely"
  in Sass—that is, the maximum integer `n` such that `n` and `n + 1` both have a
  precise representation.

* `math.$min-safe-integer`: The minimum integer that can be represented "safely"
  in Sass—that is, the minimum integer `n` such that `n` and `n - 1` both have a
  precise representation.

* `math.$max-number`: The maximum numeric value representable in Sass.

* `math.$min-number`: The smallest positive numeric value representable in Sass.

### Potentially-Breaking Changes

This proposal introduces changes that cause observable behavioral differences
which could, in principle, break existing Sass code. However, these differences
are only observable in extremely large and extremely small numbers, or numbers
that have extremely small differences between them. It's unlikely that this
comes up often in practice.

Even more importantly, the existing behavior is clearly undesirable. Integer
overflow depending on the internal state of a number object is user-hostile
behavior, as is an intransitive equality operation. To the extent that these
behaviors *are* observed by users, it's highly likely that they're seen as bugs
where a change would be welcome.

Finally, there's not a realistic way for us to provide deprecation messaging for
this change without dire performance implications. Given that, this proposal
immediately changes the behavior of the language without a deprecation period.

### Design Decisions

#### Math Function Special Cases

The existing spec for Sass's suite of math functions carves out a number of
special cases where the mathematical functions have asymptotic behavior around a
particular integer argument. For example, since the tangent function tends to
infinity as its input approaches `π/4 ± 2πn`, Sass defined `math.tan()` to
return `Infinity` for any input that fuzzy-equals `90deg +/- 360deg * n`.

However, this has a number of problems:

* It's inconsistent with `math.div()`, which does *not* do this special-casing
  for divisors very close to 0.

* It's inconsistent with [CSS Values and Units 4], which uses standard
  floating-point operations everywhere.

* Most importantly, it runs the risk of losing information if the small
  differences between values are semantically meaningful.

[CSS Values and Units 4]: https://www.w3.org/TR/css-values-4/#math

Given these, we decided to introduce a rule of thumb. A number is always treated
as a standard double except for:

* explicit Sass-level equality comparisons (including map access),
* rounding RGB color channels (until we support Color Level 4),
* and serializing a number to CSS.

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

### Set of Units

A *set of units* is structure with:

* A list of strings called "numerator units".
* A list of strings called "denominator units".

When not otherwise specified, a single unit refers to numerator units containing
only that unit and empty denominator units.

### Fuzzy Equality

Two [doubles] are said to be *fuzzy equal* to one another if either:

[doubles]: #double

* They are equal according to the `compareQuietEqual` predicate as defined
  by [IEEE 754 2019], §5.11.

* They are both finite numbers and the mathematical numbers they represent
  produce the same value when rounded to the nearest 1e⁻¹¹ (with ties away from
  zero).

### Integer

A SassScript number `n` is said to be an *integer* if there exists an integer
`m` with an exact [double] representation and `n` [fuzzy equals] that double.

If `m` exists, we say that `n`'s *integer value* is the double that represents
`m`.

[fuzzy equals]: #fuzzy-equality

> To avoid ambiguity, specification text will generally use the term
> "mathematical integer" when referring to the abstract mathematical objects.

### Compatible Units

Update the definition of compatible units as follows:

Two numbers' units are said to be *compatible* if both:

* There's a one-to-one mapping between those numbers' numerator units such that
  each pair of units is either identical, or both units have a [conversion
  factor] and those two conversion factors have the same unit. This mapping is
  known as the numbers' *numerator compatibility map*.

* There's the same type of mapping between those numbers' denominator units.
  This mapping is known as the numbers' *denominator compatibility map*.

[conversion factor]: ../spec/types/number.md#conversion-factors

Similarly, a number is *compatible with* a [set of units] if it's compatible
with a number that has those units; and two sets of units are *compatible* if a
number with one set is compatible with a number with the other.

[set of units]: #set-of-units

> This is not a functional change, it just makes it easier to refer to the
> details of compatibility between the two numbers.

## Types

Define the value type known as a *number* as three components:

* A [double] called its "value".
* A list of strings called *numerator units*.
* A list of strings called *denominator units*.

[double]: #doubles

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

#### Negation

Let `number` be a number. To determine `-number`, return a number whose value is
the result of `negate(number)` as defined by [IEEE 754 2019], §5.5.1; and whose
units are the same as `number`'s.

## Procedures

### Converting a Number to Units

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

    [conversion factors]: ../spec/types/number.md#conversion-factors

  * Set `value` to `division(multiplication(value, v1), v2)` as defined by
    [IEEE 754 2019], §5.4.1.

* For each pair of units `u1`, `u2` in the [denominator compatibility map]
  between `number` and `units` such that `u1 != u2`:

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

  [converting `n1` to `n2`'s units]: #converting-a-number-to-units

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

  [converting `number` to `newUnits`]: #converting-a-number-to-units

## Variables

### `$e`

A unitless number whose value is the closest possible [double] approximation of
the [mathematical constant e].

[mathematical constant e]: https://en.wikipedia.org/wiki/E_(mathematical_constant)

> This is `2.718281828459045`.

### `$pi`

A unitless number whose value is the closest possible [double] approximation of
the [mathematical constant π].

[mathematical constant π]: https://en.wikipedia.org/wiki/Pi

> This is `3.141592653589793`.

### `$epsilon`

A unitless number whose value is the difference between 1 and the smallest
[double] greater than 1.

> This is `2.220446049250313e-16`.

### `$max-safe-integer`

A unitless number whose value represents the maximum mathematical integer `n`
such that `n` and `n + 1` both have an exact [double] representation.

> This is `9007199254740991`.

### `$min-safe-integer`

A unitless number whose value represents the minimum mathematical integer `n`
such that `n` and `n - 1` both have an exact [double] representation.

> This is `-9007199254740991`.

### `$max-number`

A unitless number whose value represents the greatest finite number that can be
represented by a [double].

> This is `1.7976931348623157e+308`.

### `$min-number`

A unitless number whose value represents the least positive number that can be
represented by a [double].

> This is `5e-324`.

## Functions

### Bounding Functions

#### `math.ceil()`

Replace this function's procedure with:

* Return a number whose value is the result of
  `convertToIntegerTowardPositive($number.value)` as defined by [IEEE 754 2019],
  §5.8; and whose units are the same as `$number`'s.

#### `math.floor()`

Replace this function's procedure with:

* Return a number whose value is the result of
  `convertToIntegerTowardNegative($number.value)` as defined by [IEEE 754 2019],
  §5.8; and whose units are the same as `$number`'s.

#### `math.round()`

Replace this function's procedure with:

* Return a number whose value is the result of
  `convertToIntegerTiesToAway($number.value)` as defined by [IEEE 754 2019],
  §5.8; and whose units are the same as `$number`'s.

### Distance Functions

#### `math.abs()`

Replace this function's procedure with:

* Return a number whose value is the result of `abs($number.value)` as defined
  by [IEEE 754 2019], §5.5.1; and whose units are the same as `$number`'s.

### Exponential Functions

#### `math.log()`

Replace this function's procedure with:

* If `$number` has units, throw an error.

* Return a unitless number whose value is the result of `log($number.value)` as
  defined by [IEEE 754 2019], §9.2.

> This is the [natural logarithm].
>
> [natural logarithm]: https://en.wikipedia.org/wiki/Natural_logarithm

#### `math.pow()`

Replace this function's procedure with:

* If `$base` or `$exponent` has units, throw an error.

* Return a unitless number whose value is the result of `pow($number.value)` as
  defined by [IEEE 754 2019], §9.2.

#### `math.sqrt()`

Replace this function's procedure with:

* If `$number` has units, throw an error.

* Return a unitless number whose value is the result of `rootn($number.value,
  2)` as defined by [IEEE 754 2019], §9.2.

### Trigonometric Functions

#### `math.acos()`

Replace this function's procedure with:

* If `$number` has units, throw an error.

* Let `result` be a number in `rad` whose value is the result of
  `acos($number.value)` as defined by [IEEE 754 2019], §9.2.

* Return the result of [converting `result` to `deg`].

  [converting `result` to `deg`]: ../spec/types/number.md#converting-a-number-to-a-unit

#### `math.asin()`

Replace this function's procedure with:

* If `$number` has units, throw an error.

* Let `result` be a number in `rad` whose value is the result of
  `asin($number.value)` as defined by [IEEE 754 2019], §9.2.

* Return the result of [converting `result` to `deg`].

#### `math.atan()`

Replace this function's procedure with:

* If `$number` has units, throw an error.

* Let `result` be a number in `rad` whose value is the result of
  `atan($number.value)` as defined by [IEEE 754 2019], §9.2.

* Return the result of [converting `result` to `deg`].

#### `math.atan2()`

Replace the last line of this function's procedure with:

* Let `result` be a number in `rad` whose value is the result of
  `atan2($y.value, $x.value)` as defined by [IEEE 754 2019], §9.2.

* Return the result of [converting `result` to `deg`].

#### `math.cos()`

Replace this function's procedure with:

* Let `double` be the value of [converting `$number` to `rad`] allowing
  unitless.

  [converting `$number` to `rad`]: #converting-a-number-to-units

* Return a unitless number whose value is the result of `cos(double)` as defined
  by [IEEE 754 2019], §9.2.

#### `math.sin()`

Replace this function's procedure with:

* Let `double` be the value of [converting `$number` to `rad`] allowing
  unitless.

  [converting `$number` to `rad`]: #converting-a-number-to-units

* Return a unitless number whose value is the result of `sin(double)` as defined
  by [IEEE 754 2019], §9.2.

#### `math.tan()`

Replace this function's procedure with:

* Let `double` be the value of [converting `$number` to `rad`] allowing
  unitless.

  [converting `$number` to `rad`]: #converting-a-number-to-units

* Return a unitless number whose value is the result of `tan(double)` as defined
  by [IEEE 754 2019], §9.2.

### Other Functions

#### `math.div()`

Replace the line

* Its value is the result of dividing `$number1`'s value by `$number2`'s value.

with

* Its value is the result of `divide($number1.value, $number2.value)` as defined
  by [IEEE 754 2019], §5.4.1.
