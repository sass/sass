# Calculation Functions: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/3504))*

## Table of Contents

* Foo

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

## Definitions

### Known Units

A number has *known units* unless it has unit `%`.

> This is relevant for calculations, because in plain CSS they resolve
> percentages before doing their operations. This means that any non-linear
> operations involving percentages must be passed through to plain CSS rather
> than handled by Sass.
>
> More complex units involving percentages are allowed because any non-linear
> function will throw for complex units anyway.

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

[`SpecialFunctionName`]: #specialfunctionexpression
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

## Procedures

### Simplifying a Calculation

Replace [the definition of "Simplifying a Calculation"] with the following:

[the definition of "Simplifying a Calculation"]: ../spec/types/calculation.md#simplifying-a-calculation

This algorithm takes a calculation `calc` and returns a number or a calculation.

> This algorithm is intended to return a value that's CSS-semantically identical
> to the input.

* If `calc` was parsed from an expression within a `SupportsDeclaration`'s
  `Expression`, but outside any interpolation, return a `calc` as-is.

* Let `arguments` be the result of [simplifying](#simplifying-a-calculationvalue) each
  of `calc`'s arguments.

* If `calc`'s name is `"calc"` and `arguments` contains exactly a single number
  or calculation, return it.

* If `calc`'s name is `"mod"`, `"rem"`, `"atan2"`, or `"pow"`; `arguments` has
  fewer than three elements; and none of those are unquoted strings or
  `CalculationInterpolation`s, throw an error.

  > It's valid to write `pow(var(--two-args))` or `pow(#{"2, 3"})`, but
  > otherwise calculations' arguments must match the expected number.

* If `calc`'s name is `"sin"`, `"cos"`, `"tan"`, `"asin"`, `"acos"`, `"atan"`,
  `"sqrt"`, `"abs"`, or `"log"` and `arguments` contains exactly a single number
  with [known units], return the result of passing that number to the function
  in [`sass:math`] whose name matches `calc`'s.

  [known units]: #known-units
  [`sass:math`]: ../spec/built-in-modules/math.md

* If `calc`'s name is `"exp"` and `arguments` contains exactly a single number
  `number` with [known units], return the result of calling `math.pow(math.$e,
  number)`.

* If `calc`'s name is `"sign"` and `arguments` contains exactly a single number
  `number` with [known units]:

  * If `number`'s value is positive, return `1`.
  * If `number`'s value is negative, return `-1`.
  * Otherwise, return a unitless number with the same value as `number`.
  
    > In this case, `number` is either positive zero, negative zero, or NaN.
  
  > To match CSS's behavior, these computations _don't_ use fuzzy comparisons.

* If `calc`'s name is `"atan2"`, `"pow"`, or `"log"`:

  * If `calc`'s name isn't `"log"`, `arguments` has only one element, and it's
    not an unquoted string or a `CalculationInterpolation`, throw an error.
  
  * Otherwise, if `arguments` contains exactly two numbers with [known units],
    return the result of passing that number to the function in [`sass:math`]
    whose name matches `calc`'s.

* If `calc`'s name is `"mod"` or `"rem"`:

  * If `arguments` has only one element and it's not an unquoted string or a
    `CalculationInterpolation`, throw an error.

  * Otherwise, if `arguments` contains exactly two numbers `dividend` and
    `modulus`:

    > These numbers need not have known units, since mod and rem are linear
    > functions so they'll work equivalently for percentages as for the numbers
    > they represent.

    * If `dividend` and `modulus` are [definitely-incompatible], throw an error.

      [definitely-incompatible]: ../spec/types/number.md#possibly-compatible-numbers

    * If `dividend` and `modulus` are mutually [compatible], let `result` be the
      result of `dividend % modulus`.

      [compatible]: ../spec/types/number.md#compatible-units

    * If `calc`'s name is `"rem"`, and if `dividend` is positive and `modulus`
      is negative or vice versa, set `result` to `mod - dividend`.

    * Return `result`.

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

        * Let `upper` and `lower` be the two integer multiples of `step` which
          are closest to `number` such that `upper` is greater than `lower`. If
          `upper` would be 0, it's specifically positive zero; if `lower` would
          be zero, it's specifically negative zero.

        * If `strategy`'s value is `"nearest"`, return whichever of `upper` and
          `lower` has the smallest absolute distance from `number`. If both have
          an equal difference, return `upper`.

        * If `strategy`'s value is `"up"`, return `upper`.

        * If `strategy`'s value is `"down"`, return `lower`.

        * If `strategy`'s value is `"to-zero"`, return whichever of `upper` and
          `lower` has the smallest absolute difference from 0.

* If `calc`'s name is `"clamp"`:

  * If `arguments` has fewer than three elements, and none of those are unquoted
    strings or `CalculationInterpolation`s, throw an error.

  * Otherwise, if `arguments` are all numbers:

    * If those arguments are mutually [compatible], return the result of calling
      `math.clamp()` with those arguments.

    * Otherwise, if any two of those arguments are [definitely-incompatible],
      throw an error.

* If `calc`'s name is `"hypot"` and `arguments` are all numbers:
  
  * If those arguments are mutually [compatible], return the result of calling
    `math.hypot()` with those arguments.

  * Otherwise, if any two of those arguments are [definitely-incompatible],
    throw an error.

* If `calc`'s name is `"min"` or `"max"` and `arguments` are all numbers:

  * If the arguments with units are all mutually [compatible], call
    [`math.min()`] or [`math.max()`] (respectively) with those arguments. If
    this doesn't throw an error, return its result.

    [`math.min()`]: ../spec/built-in-modules/math.md#min
    [`math.max()`]: ../spec/built-in-modules/math.md#max

    > `min()` and `max()` allow unitless numbers to be mixed with units because
    > they need to be backwards-compatible with Sass's old global `min()` and
    > `max()` functions.

  * Otherwise, if any two of those arguments are [definitely-incompatible],
    throw an error.

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
  results of [evaluating the expression's `CalcArgument`s](#calcargument).

* Return the result of [simplifying] `calc`.

## Deprecation Process

Before this specification is applied in full force, it will be applied with the
following modification:

* When simplifying a calculation named `"abs"` whose sole argument is a number
  _without_ [known units], return the result of calling `math.abs()` with that
  number and emit a deprecation warning named `abs-percent`.
