# Calculation Functions: Draft 1.5

*([Issue](https://github.com/sass/sass/issues/3504))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Changing Mod Infinity Behavior](#changing-mod-infinity-behavior)
* [Definitions](#definitions)
  * [Exact Equality](#exact-equality)
  * [Known Units](#known-units)
  * [Potentially Slash-Separated Number](#potentially-slash-separated-number)
* [Syntax](#syntax)
  * [`FunctionExpression`](#functionexpression)
  * [`CalculationExpression`](#calculationexpression)
  * [`CssRound`](#cssround)
  * [`CssAbs`](#cssabs)
* [Operations](#operations)
  * [Modulo](#modulo)
* [Procedures](#procedures)
  * [Simplifying a Calculation](#simplifying-a-calculation)
* [Semantics](#semantics)
  * [Calculation Expressions](#calculation-expressions)
* [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

Sass added support for first-class calculation objects [recently], but this
initial support only included the `calc()`, `min()`, `max()`, and `clamp()`
expressions since these were the only ones supported in browsers at the time.
Since then, between Firefox and Safari browser support has landed for the rest
of the expressions listed in [Values and Units 4].

[recently]: ../accepted/first-class-calc.md
[Values and Units 4]: https://drafts.csswg.org/css-values-4/#math

## Summary

> This section is non-normative.

This proposal parses the full range of functions defined in [Values and Units 4]
as calculation values in Sass: `round()`, `mod()`, `rem()`, `sin()`, `cos()`,
`tan()`, `asin()`, `acos()`, `atan()`, `atan2()`, `pow()`, `sqrt()`, `hypot()`,
`log()`, `exp()`, `abs()`, and `sign()`.

Since Sass already defines top-level functions named `round()` and `abs()`,
these will fall back to Sass function calls in a similar way to how `min()` and
`max()` already work.

This poses a small compatibility problem: in Sass, `abs(10%)` will always return
`10%` because the number is positive, but in plain CSS it could return the
equivalent of `-10%` since percentages are resolved before calculations. To
handle this, we'll deprecate the global `abs()` function with a percentage and
recommend users explicitly write `math.abs()` or `abs(#{})` instead.

### Design Decisions

#### Changing Mod Infinity Behavior

This proposal changes the behavior of the `%` operation when the right-hand side
is infinite _and_ has a different sign than the left-hand side. Sass used to
return the right-hand side in accordance with the floating point specification,
but it now returns NaN to match CSS's `mod()` function.

Although this is technically a breaking change, we think it's highly unlikely
that it will break anyone in practice, so we're not going to do a deprecation
process for it.

## Definitions

### Exact Equality

Two [doubles] are said to be *exactly equal* if they are equal according to the
`compareQuietEqual` predicate as defined by [IEEE 754 2019], §5.11.

[doubles]: ../spec/types/number.md#double
[IEEE 754 2019]: https://ieeexplore.ieee.org/document/8766229

> This is as opposed to [fuzzy equality].
>
> [fuzzy equality]: ../spec/types/number.md#fuzzy-equality

### Known Units

A number has *known units* unless it has unit `%`.

> This is relevant for calculations, because in plain CSS they resolve
> percentages before doing their operations. This means that any non-linear
> operations involving percentages must be passed through to plain CSS rather
> than handled by Sass.
>
> More complex units involving percentages are allowed because any non-linear
> function will throw for complex units anyway.

### Potentially Slash-Separated Number

Replace [the definition of `Potentially Slash-Separated Number`] with the
following:

[the definition of `Potentially Slash-Separated Number`]: ../spec/types/number.md#potentially-slash-separated-number

A Sass number may be *potentially slash-separated*. If it is, it is associated
with two additional Sass numbers, the *original numerator* and the *original
denominator*. A number that is not potentially slash-separated is known as
*slash-free*.

A potentially slash-separated number is created when a `ProductExpression` with
a `/` operator is evaluated and each operand is *syntactically* one of the
following:

* a `Number`,
* a [`Calculation`] **whose name is not `abs`, `max`, `min`, or `round`**, or
* a `ProductExpression` that can itself create potentially slash-separated
  numbers.

> We exclude these four calc functions from producing potentially
> slash-separated numbers to ensure that existing code like `1 / round(1.5)`
> continues to be evaluated as division when the function goes from being
> evaluated as a Sass global function to being evaluated as a calc function.

[`Calculation`]: ../spec/types/calculation.md#syntax

If both operands are evaluated as numbers, the resulting number is potentially
slash-separated. The first operand is the original numerator of the potentially
slash-separated number returned by the `/` operator, and the second is the
original denominator.

## Syntax

### `FunctionExpression`

Replace [the definition of `FunctionExpression`] with the following:

[the definition of `FunctionExpression`]: ../spec/functions.md#syntax

<x><pre>
**FunctionExpression**¹ ::= [CssMinMax]
&#32;                     | [CssRound]
&#32;                     | [CssAbs]
&#32;                     | [SpecialFunctionExpression]
&#32;                     | [CalculationExpression]
&#32;                     | EmptyFallbackVar
&#32;                     | FunctionCall
**EmptyFallbackVar**    ::= 'var('² Expression ',' ')'
**FunctionCall**³       ::= [NamespacedIdentifier] ArgumentInvocation
</pre></x>

[CssMinMax]: ../spec/types/calculation.md#cssminmax
[CssRound]: #cssround
[CssAbs]: #cssabs
[SpecialFunctionExpression]: ../spec/syntax.md#specialfunctionexpression
[CalculationExpression]: ../spec/types/calculation.md#calculationexpression
[NamespacedIdentifier]: ../spec/modules.md#syntax

1: `CssMinMax`, `CssRound`, `CssAbs`, and `EmptyFallbackVar` all take precedence
   over `FunctionCall` if either could be consumed.

2: `'var('` is matched case-insensitively.

3: `FunctionCall` may not have any whitespace between the `NamespacedIdentifier`
   and the `ArgumentInvocation`. It may not start with [`SpecialFunctionName`],
   [`UnaryCalcName`], [`BinaryCalcName`], `'hypot('`, or `'clamp('`
   (case-insensitively).

[`SpecialFunctionName`]: ../spec/syntax.md#specialfunctionexpression
[`UnaryCalcName`]: #calculationexpression
[`BinaryCalcName`]: #calculationexpression

### `CalculationExpression`

Replace [the definition of `CalculationExpression`] with:

[the definition of `CalculationExpression`]: ../spec/types/calculation.md#calculationexpression

<x><pre>
**CalculationExpression** ::= UnaryCalcExpression
&#32;                       | BinaryCalcExpression
&#32;                       | ClampExpression
&#32;                       | HypotExpression
**UnaryCalcExpression**   ::= UnaryCalcName CalcArgument ')'
**BinaryCalcExpression**  ::= BinaryCalcName CalcArgument (',' CalcArgument)? ')'
**HypotExpression**       ::= 'hypot('¹ CalcArgument (',' CalcArgument)\* ')'
**UnaryCalcName**¹        ::= 'calc(' | 'sin(' | 'cos(' | 'tan(' | 'asin('
&#32;                       | 'acos(' | 'atan(' | 'sqrt(' | 'exp(' | 'sign('
**BinaryCalcName**¹       ::= 'mod(' | 'rem(' | 'atan2(' | 'pow(' | 'log('
</pre></x>

1: The strings `hypot(`, `clamp(`, and `var(` are matched case-insensitively, as
are the productions `UnaryCalcName` and `BinaryCalcName`.

Remove the existing definition of `CalcExpression`.

> Note: we aren't adding `| CssRound | CssAbs` to the definition of `CalcValue`
> because existing Sass releases already allow the global `round()` and `abs()`
> functions in calculations.

### `CssRound`

Add the following production:

<x><pre>
**CssRound** ::= 'round('¹ CalcArgument (',' CalcArgument){2} ')'
</pre></x>

1: The string `round(` is matched case-insensitively.

> Although the three-argument `round()` function only allows a few values in its
> first argument, for simplicity those are checked at evaluation time rather
> than parse time.

### `CssAbs`

Add the following production:

<x><pre>
**CssAbs** ::= 'abs('¹ CalcArgument ')'
</pre></x>

1: The string `abs(` is matched case-insensitively.

## Operations

### Modulo

Replace [the definition of modulo for numbers] with the following:

[the definition of modulo for numbers]: ../spec/types/number.md#modulo

> Differences are highlighted in bold.

Let `n1` and `n2` be two numbers. To determine `n1 % n2`:

* Let `c1` and `c2` be the result of [matching units] for `n1` and `n2` allowing
  unitless.

  [matching units]: ../spec/types/number.md#matching-two-numbers-units

* **If `c2` is infinity and has a different sign than `c1` (including
  oppositely-signed zero), return NaN with the same units as `c1`.**

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

## Procedures

### Simplifying a Calculation

Replace [the definition of "Simplifying a Calculation"] with the following:

[the definition of "Simplifying a Calculation"]: ../spec/types/calculation.md#simplifying-a-calculation

This algorithm takes a calculation `calc` and returns a number or a calculation.

> This algorithm is intended to return a value that's CSS-semantically identical
> to the input.

* If `calc` was parsed from an expression within a `SupportsDeclaration`'s
  `Expression`, but outside any interpolation, return a `calc` as-is.

* Let `arguments` be the result of [simplifying] each of `calc`'s arguments.

  [simplifying]: ../spec/types/calculation.md#simplifying-a-calculationvalue

* If `calc`'s name is `"calc"` and `arguments` contains exactly a single number
  or calculation, return it.

* If `calc`'s name is `"mod"`, `"rem"`, `"atan2"`, or `"pow"`; `arguments` has
  fewer than two elements; and none of those are unquoted strings or
  `CalculationInterpolation`s, throw an error.

  > It's valid to write `pow(var(--two-args))` or `pow(#{"2, 3"})`, but
  > otherwise calculations' arguments must match the expected number.

* If `calc`'s name is `"sin"`, `"cos"`, `"tan"`, `"asin"`, `"acos"`, `"atan"`,
  `"sqrt"`, `"log"`, or `"round"` and `arguments` contains exactly a single
  number, return the result of passing that number to the function in
  [`sass:math`] whose name matches `calc`'s.

  [`sass:math`]: ../spec/built-in-modules/math.md

  > The `sass:math` functions will check units here for the functions that
  > require specific or no units.

* If `calc`'s name is `"abs"` and `arguments` contains exactly a single number
  with [known units], return the result of passing that number to the function
  in [`sass:math`] whose name matches `calc`'s.

  [known units]: #known-units

* If `calc`'s name is `"exp"` and `arguments` contains exactly a single number
  `number`, return the result of calling `math.pow(math.$e, number)`.

  > This will throw an error if the argument has units.

* If `calc`'s name is `"sign"` and `arguments` contains exactly a single number
  `number` with [known units]:

  * If `number`'s value is positive, return `1`.
  * If `number`'s value is negative, return `-1`.
  * Otherwise, return a unitless number with the same value as `number`.

    > In this case, `number` is either `+0`, `-0`, or NaN.

  > To match CSS's behavior, these computations _don't_ use fuzzy comparisons.

* If `calc`'s name is `"log"`:

  * If any argument is a number with units, throw an error.

  * Otherwise, if `arguments` contains exactly two numbers, return the result of
    passing its arguments to the [`log()` function] in [`sass:math`].

  [`log()` function]: ../spec/built-in-modules/math.md#log

* If `calc`'s name is `"pow"`:

  * If any argument is a number with units, throw an error.

  * Otherwise, if `arguments` contains exactly two numbers, return the result of
    passing those numbers to the [`pow()` function] in [`sass:math`].

  [`pow()` function]: ../spec/built-in-modules/math.md#pow

* If `calc`'s name is `"atan2"` and `arguments` contains two numbers which both
  have [known units], return the result of passing those numbers to the
  [`atan2()` function] in [`sass:math`].

  > This will throw an error if either argument has units.
  >
  > `atan2()` passes percentages along to the browser because they may resolve
  > to negative values, and `atan2(-x, -y) != atan2(x, y)`.

  [`atan2()` function]: ../spec/built-in-modules/math.md#atan2

* If `calc`'s name is `"mod"` or `"rem"`:

  * If `arguments` has only one element and it's not an unquoted string or a
    `CalculationInterpolation`, throw an error.

  * Otherwise, if `arguments` contains exactly two numbers `dividend` and
    `modulus`:

    * If `dividend` and `modulus` are [definitely-incompatible], throw an error.

    * If `dividend` and `modulus` are mutually [compatible]:

      * Let `result` be the result of `dividend % modulus`.

      * If `calc`'s name is `"rem"`, and if `dividend` is positive and `modulus`
        is negative or vice versa:

        * If `modulus` is infinite, return `dividend`.
        * If `result` [exactly equals] 0, return `-result`.
        * Otherwise, return `result - modulus`.

      * Otherwise, return `result`.

  [definitely-incompatible]: ../spec/types/number.md#possibly-compatible-numbers
  [exactly equals]: #exact-equality

* If `calc`'s name is `"round"`:

  * If `arguments` has exactly three elements, set `strategy`, `number`, and
    `step` to those arguments respectively.

  * Otherwise, if `arguments` has exactly two elements:

    * If the first element is an unquoted string or interpolation with value
      `"nearest"`, `"up"`, `"down"`, or `"to-zero"`, and the second argument
      isn't an unquoted string or `CalculationInterpolation`, throw an error.

      > Normally we allow unquoted strings anywhere in a calculation, but this
      > helps catch the likely error of a user accidentally writing `round(up,
      > 10px)` without realizing that it needs a third argument.

    * Otherwise, set `number` and `step` to the two arguments respectively and
      `strategy` to an unquoted string with value `"nearest"`.

  * Otherwise, if the single argument isn't an unquoted string or
    `CalculationInterpolation`, throw an error.

  * If `strategy`, `number`, and `step` are set:

    * If `strategy` isn't a [special variable string], nor is it an unquoted
      string or interpolation with value `"nearest"`, `"up"`, `"down"`, or
      `"to-zero"`, throw an error.

    * If `strategy` is an unquoted string or interpolation and both `number` and
      `step` are numbers:

      * If `number` and `step` are [definitely-incompatible], throw an error.

      * If `number` and `step` are mutually [compatible]:

        * If `number`'s and `step`'s values are both infinite, if `step` is
          [exactly equal] to 0, or if either `number`'s or `step`'s values are
          NaN, return NaN with the same units as `number`.

        * If `number`'s value is infinite, return `number`.

        * If `step`'s value is infinite:

          * If `strategy`'s value is `"nearest"` or `"to-zero"`, return `+0` if
            `number`'s value is positive or `+0`, and `-0` otherwise.

          * If `strategy`'s value is `"up"`, return positive infinity if
            `number`'s value is positive, `+0` if `number`'s value is `+0`, and
            `-0` otherwise.

          * If `strategy`'s value is `"down"`, return negative infinity if
            `number`'s value is negative, `-0` if `number`'s value is `-0`, and
            `+0` otherwise.

        * Set `number` and `step` to the result of [matching units] for `number`
          and `step`.

        * If `number`'s value is [exactly equal] to `step`'s, return `number`.

        * Let `upper` and `lower` be the two integer multiples of `step` which
          are closest to `number` such that `upper` is greater than `lower`. If
          `upper` would be 0, it's specifically `-0`; if `lower` would be zero,
          it's specifically `-0`.

        * If `strategy`'s value is `"nearest"`, return whichever of `upper` and
          `lower` has the smallest absolute distance from `number`. If both have
          an equal difference, return `upper`.

        * If `strategy`'s value is `"up"`, return `upper`.

        * If `strategy`'s value is `"down"`, return `lower`.

        * If `strategy`'s value is `"to-zero"`, return whichever of `upper` and
          `lower` has the smallest absolute difference from 0.

  [special variable string]: ../spec/functions.md#special-variable-string

* If `calc`'s name is `"clamp"`:

  * If `arguments` has fewer than three elements, and none of those are unquoted
    strings or `CalculationInterpolation`s, throw an error.

  * Otherwise, if any two elements of `arguments` are [definitely-incompatible]
    numbers, throw an error.

  * Otherwise, if `arguments` are all mutually [compatible] numbers, return the
    result of calling `math.clamp()` with those arguments.

* If `calc`'s name is `"hypot"`:

  * If any two elements of `arguments` are [definitely-incompatible] numbers,
    throw an error.

  * Otherwise, if all `arguments` are all numbers with [known units] that are
    mutually [compatible], return the result of calling `math.hypot()` with
    those arguments.

    > `hypot()` has an exemption for percentages because it squares its inputs,
    > so `hypot(-x, -y) != -hypot(x, y)`.

* If `calc`'s name is `"min"` or `"max"` and `arguments` are all numbers:

  * If the arguments with units are all mutually [compatible], call
    [`math.min()`] or [`math.max()`] (respectively) with those arguments. If
    this doesn't throw an error, return its result.

    > `min()` and `max()` allow unitless numbers to be mixed with units because
    > they need to be backwards-compatible with Sass's old global `min()` and
    > `max()` functions.

  * Otherwise, if any two of those arguments are [definitely-incompatible],
    throw an error.

  [`math.min()`]: ../spec/built-in-modules/math.md#min
  [`math.max()`]: ../spec/built-in-modules/math.md#max

* Otherwise, return a calculation with the same name as `calc` and `arguments`
  as its arguments.

## Semantics

### Calculation Expressions

Replace the semantics for `CalcExpression`, `ClampExpression`, and `CssMinMax`
with the following:

To evaluate a `UnaryCalcExpression`, `BinaryCalcExpression`, `HypotExpression`,
`CssMinMax`, `CssRound`, or `CssAbs`:

* Let `name` be the lower-case value of the expression's first token without the
  trailing parenthesis.

* Let `calc` be a calculation whose name is `name` and whose arguments are the
  results of [evaluating the expression's `CalcArgument`s].

  [evaluating the expression's `CalcArgument`s]: ../spec/types/calculation.md#calcargument

* Return the result of [simplifying] `calc`.

## Deprecation Process

Before this specification is applied in full force, it will be applied with the
following modification:

* When simplifying a calculation named `"abs"` whose sole argument is a number
  _without_ [known units], return the result of calling `math.abs()` with that
  number and emit a deprecation warning named `abs-percent`.
