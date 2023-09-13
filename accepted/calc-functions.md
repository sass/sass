# Calculation Functions: Draft 3.2

*([Issue](https://github.com/sass/sass/issues/3504), [Changelog](calc-functions.changes.md))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Merged Syntax](#merged-syntax)
    * [Changing Mod Infinity Behavior](#changing-mod-infinity-behavior)
* [Definitions](#definitions)
  * [Calculation-Safe Expression](#calculation-safe-expression)
  * [Exact Equality](#exact-equality)
  * [Known Units](#known-units)
  * [Potentially Slash-Separated Number](#potentially-slash-separated-number)
* [Syntax](#syntax)
  * [`FunctionExpression`](#functionexpression)
  * [`CssMinMax`](#cssminmax)
  * [`CalculationExpression`](#calculationexpression)
* [Types](#types)
  * [Calculation](#calculation)
* [Operations](#operations)
  * [Modulo](#modulo)
* [Procedures](#procedures)
  * [Evaluating a `FunctionCall` as a Calculation](#evaluating-a-functioncall-as-a-calculation)
  * [Evaluating an Expression as a Calculation Value](#evaluating-an-expression-as-a-calculation-value)
  * [Simplifying a Calculation](#simplifying-a-calculation)
  * [Simplifying a `CalculationValue`](#simplifying-a-calculationvalue)
* [Semantics](#semantics)
  * [`FunctionCall`](#functioncall)
  * [Calculations](#calculations)
    * [`FunctionExpression` and `Variable`](#functionexpression-and-variable)
    * [`SumExpression` and `ProductExpression`](#sumexpression-and-productexpression)
  * [`SpaceListExpression`](#spacelistexpression)
    * [`ParenthesizedExpression`](#parenthesizedexpression)
    * [`InterpolatedIdentifier`](#interpolatedidentifier)
* [Interaction with Forward Slash as a Separator](#interaction-with-forward-slash-as-a-separator)
  * [Adjusting Slash Precedence](#adjusting-slash-precedence)
  * [`SlashListExpression`](#slashlistexpression)
* [API](#api)
  * [Types](#types-1)
    * [`CalculationInterpolation`](#calculationinterpolation)
      * [`internal`](#internal)
      * [Constructor](#constructor)
      * [`value`](#value)
      * [`equals`](#equals)
      * [`hashCode`](#hashcode)
* [Embedded Protocol](#embedded-protocol)
  * [`CalculationValue.value.interpolation`](#calculationvaluevalueinterpolation)
* [Deprecation Process](#deprecation-process)
  * [`abs-percent`](#abs-percent)

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

This also expands calculation parsing to allow constructs like `calc(1
var(--plus-two))` (where for example `--plus-two: + 2`) which are valid CSS but
weren't supported by the old first-class calculation parsing.

### Design Decisions

#### Merged Syntax

This proposal substantially changes the way calculations are parsed, merging the
syntax with the standard Sass expression syntax. Now the only difference between
a calculation and a normal Sass function is how it's *evaluated*. This has the
notable benefit of allowing calculations to coexist with user-defined Sass
functions of the same name, preserving backwards-compatibility.

Because this overlap is always going to be somewhat confusing for readers, we
considered simply disallowing Sass functions whose names matched CSS
calculations after a suitable deprecation period. However, in addition to the
intrinsic value of avoiding breaking changes, the function name `rem()` in
particular is widely used in Sass libraries as a means of converting pixel
widths to relative ems, so this is a fairly substantial breaking change in
practice.

This does also require its own breaking change to the way interpolation is
handled in calculations—`calc(#{"1px +"} 1%)` was formerly valid but is no
longer. However, this is likely to break many fewer users in practice, and is
relatively easy to continue supporting in a deprecated state in the short term.

#### Changing Mod Infinity Behavior

This proposal changes the behavior of the `%` operation when the right-hand side
is infinite *and* has a different sign than the left-hand side. Sass used to
return the right-hand side in accordance with the floating point specification,
but it now returns NaN to match CSS's `mod()` function.

Although this is technically a breaking change, we think it's highly unlikely
that it will break anyone in practice, so we're not going to do a deprecation
process for it.

## Definitions

### Calculation-Safe Expression

An expression is "calculation-safe" if it is one of:

* A [`FunctionExpression`].
* A `ParenthesizedExpression` whose contents is calculation-safe.
* A `SumExpression` whose operands are calculation-safe.
* A `ProductExpression` whose operator is `*` or `/` and whose operands are
  calculation-safe.
* A `Number`.
* A `Variable`.
* An `InterpolatedIdentifier`.
* An unbracketed `SpaceListExpression` with more than one element, whose
  elements are all calculation-safe.

[`FunctionExpression`]: ../spec/functions.md#syntax

> Because calculations have special syntax in CSS, only a subset of SassScript
> expressions are valid (and these are interpreted differently than elsewhere).

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
* a [`FunctionCall`], or
* a `ProductExpression` that can itself create potentially slash-separated
  numbers.
  
[`FunctionCall`]: ../spec/functions.md#functioncall

If the result of evaluating the `ProductExpression` is a number, that number is
potentially slash-separated if all of the following are true:

* the results of evaluating both operands were numbers, and
* if either operand was a `FunctionCall`, it was [evaluated as a calculation]
  and its name was not `"abs"`, `"max"`, `"min"`, or `"round"`.

[evaluated as a calculation]: #evaluating-a-functioncall-as-a-calculation

If both of these are true, the first operand is the original numerator of the
potentially slash-separated number returned by the `/` operator, and the second
is the original denominator.

## Syntax

> Calculations are no longer parsed differently than other Sass productions.
> Instead, they're *evaluated* differently at runtime. This allows them to
> coexist with user-defined Sass functions even when their names overlap.

### `FunctionExpression`

Remove `CssMinMax` and `CalculationExpression` from [the definition of
`FunctionExpression`].

[the definition of `FunctionExpression`]: ../spec/functions.md#syntax

### `CssMinMax`

Remove the `CssMinMax` production.

### `CalculationExpression`

Remove the `CalculationExpression` production.

## Types

### Calculation

Delete the `CalculationInterpolation` type and remove all references to it.

> This type only existed to track where we needed to defensively insert
> parentheses. Now that we track parentheses as part of the calculation AST,
> this is no longer necessary

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

### Evaluating a `FunctionCall` as a Calculation

This algorithm takes a [`FunctionCall`] `call` whose name is a plain identifier
and returns a number or a calculation.

* If `call`'s `ArgumentInvocation` contains one or more `KeywordArgument`s or
  one or more `RestArgument`s, throw an error.

* Let `calc` be a calculation whose name is the lower-case value of `call`'s
  name and whose arguments are the result of evaluating each `Expression` in
  `call`'s `ArgumentInvocation` [as a calculation value].

  [as a calculation value]: #evaluating-an-expression-as-a-calculation-value

* Return the result of [simplifying] `calc`.

### Evaluating an Expression as a Calculation Value

This algorithm takes an expression `expression` and returns a
`CalculationValue`.

* If `expression` isn't [calculation-safe], throw an error.

* Otherwise, evaluate `expression` using the semantics defined in the
  [Calculations] specification if there are any, or the standard semantics
  otherwise.

  [Calculations]: #calculations

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
  fewer than two elements; and none of those are unquoted strings, throw an
  error.

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

  > To match CSS's behavior, these computations *don't* use fuzzy comparisons.

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

  * If `arguments` has only one element and it's not an unquoted string, throw
    an error.

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

  [compatible]: ../spec/types/number.md#compatible-units
  [definitely-incompatible]: ../spec/types/number.md#possibly-compatible-numbers
  [exactly equals]: #exact-equality

* If `calc`'s name is `"round"`:

  * If `arguments` has exactly three elements, set `strategy`, `number`, and
    `step` to those arguments respectively.

  * Otherwise, if `arguments` has exactly two elements:

    * If the first element is an unquoted string or interpolation with value
      `"nearest"`, `"up"`, `"down"`, or `"to-zero"`, and the second argument
      isn't an unquoted string, throw an error.

      > Normally we allow unquoted strings anywhere in a calculation, but this
      > helps catch the likely error of a user accidentally writing `round(up,
      > 10px)` without realizing that it needs a third argument.

    * Otherwise, set `number` and `step` to the two arguments respectively and
      `strategy` to an unquoted string with value `"nearest"`.

  * Otherwise, if the single argument isn't an unquoted string, throw an error.

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
    strings, throw an error.

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

### Simplifying a `CalculationValue`

Replace the block "If `value` is a calculation" in the procedure for
[simplifying a `CalculationValue`] with the following:

[simplifying a `CalculationValue`]: ../spec/types/calculation.md#simplifying-a-calculationvalue

* If `value` is a calculation:

  * Let `result` be the result of [simplifying] `value`.

  * If `result` isn't a calculation whose name is `"calc"`, return `result`.

  * If `result`'s argument isn't an unquoted string, return `result`.

  * If `result`'s argument begins case-insensitively with `"var("`; or if it
    contains whitespace, `"/"`, or `"*"`; return `"(" +` result's argument `+
    ")"` as an unquoted string.

    > This is ensures that values that could resolve to operations end up
    > parenthesized if used in other operations. It's potentially a little
    > overzealous, but that's unlikely to be a major problem given that the
    > output is still smaller than including the full `calc()` and we don't want
    > to encourage users to inject calculations with interpolation anyway.

## Semantics

### `FunctionCall`

Add the following to [the semantics for `FunctionCall`] before checking for a
global function:

[the semantics for `FunctionCall`]: ../spec/functions.md#functioncall

* If `function` is null; `name` is case-insensitively equal to `"min"`, `"max"`,
  `"round"`, or `"abs"`; `call`'s `ArgumentInvocation` doesn't have any
  `KeywordArgument`s or `RestArgument`s; and all arguments in `call`'s
  `ArgumentInvocation` are [calculation-safe], return the result of evaluating
  `call` [as a calculation].

  [calculation-safe]: #calculation-safe-expression
  [as a calculation]: #evaluating-a-functioncall-as-a-calculation

  > For calculation functions that overlap with global Sass function names, we
  > want anything Sass-specific like this to end up calling the Sass function.
  > For all other calculation functions, we want those constructs to throw an
  > error (which they do when evaluating `call` [as a calculation]).

* If `function` is null and `name` is case-insensitively equal to `"calc"`,
  `"clamp"`, `"hypot"`, `"sin"`, `"cos"`, `"tan"`, `"asin"`, `"acos"`, `"atan"`,
  `"sqrt"`, `"exp"`, `"sign"`, `"mod"`, `"rem"`, `"atan2"`, `"pow"`, or `"log"`,
  return the result of evaluating `call` [as a calculation].

### Calculations

Remove all prior [semantics for Calculations]. The following semantics apply
only when evaluating expressions [as calculation values].

[semantics for Calculations]: ../spec/types/calculation.md#semantics
[as calculation values]: #evaluating-an-expression-as-a-calculation-value

#### `FunctionExpression` and `Variable`

To evaluate a `FunctionExpression` or a `Variable` as a calculation value,
evaluate it using the standard semantics. If the result is a number, an unquoted
string, or a calculation, return it. Otherwise, throw an error.

> Allowing variables to return unquoted strings here supports referential
> transparency, so that `$var: fn(); calc($var)` works the same as `calc(fn())`.

#### `SumExpression` and `ProductExpression`

To evaluate a `SumExpresssion` or a `ProductExpression` as a calculation value:

* Let `left` be the result of evaluating the first operand as a calculation
  value.

* For each remaining `"+"`, `"-"`, `"*"`, or `"/"` token `operator` and operand
  `operand`:

  * Let `right` be the result of evaluating `operand` as a calculation value.

  * Set `left` to a `CalcOperation` with `operator`, `left`, and `right`.

* Return `left`.

### `SpaceListExpression`

To evaluate a `SpaceListExpresssion` as a calculation value:

* Let `elements` be the results of evaluating each element as a calculation
  value.

* If `elements` has two adjacent elements that aren't unquoted strings, throw an
  error.

  > This ensures that valid CSS constructs like `calc(1 var(--plus-two))` and
  > similar Sass constructs like `calc(1 #{"+ 2"})` work while preventing clear
  > errors like `calc(1 2)`.
  >
  > This does allow errors like `calc(a b)`, but the complexity of verifying
  > that the unquoted strings could actually be a partial operation isn't worth
  > the benefit of eagerly producing an error in this edge case.

* Let `serialized` be an empty list.

* For each `element` of `elements`:

  * Let `css` be the result of [serializing] `element`.

    [serializing]: ../spec/types/calculation.md#serialization

  * If `element` is a `CalcOperation` that was produced by evaluating a
    `ParenthesizedExpression`, set `css` to `"(" + css + ")"`.

  * Append `css` to `serialized`.

* Return an unquoted strings whose contents are the elements of `serialized`
  separated by `" "`.

#### `ParenthesizedExpression`

> If a `var()` or an interpolation is written directly within parentheses, it's
> necessary to preserve those parentheses. CSS resolves `var()` by literally
> replacing the function with the value of the variable and *then* parsing the
> surrounding context.
>
> For example, if `--ratio: 2/3`, `calc(1 / (var(--ratio)))` is parsed as
> `calc(1 / (2/3)) = calc(3/2)` but `calc(1 / var(--ratio))` is parsed as
> `calc(1 / 2/3) = calc(1/6)`.

To evaluate a `ParenthesizedExpression` with contents `expression` as a
calculation value:

* Let `result` be the result of evaluating `expression` as a calculation value.

* If `result` is an unquoted string, return `"(" + result + ")"` as an unquoted
  string.

* Otherwise, return `result`.

#### `InterpolatedIdentifier`

To evaluate an `InterpolatedIdentifier` `ident` as a calculation value:

* If `ident` is case-insensitively equal to `pi`, return 3.141592653589793.

  > This is the closest double approximation of the mathematical constant π.

* If `ident` is case-insensitively equal to `e`, return 2.718281828459045.

  > This is the closest double approximation of the mathematical constant e.

* If `ident` is case-insensitively equal to `infinity`, return the double
  `Infinity`.

* If `ident` is case-insensitively equal to `-infinity`, return the double
  `-Infinity`.

* If `ident` is case-insensitively equal to `nan`, return the double `NaN`.

* Otherwise, return the result of evaluating `ident` using standard semantics.

  > This will be an `UnquotedString`.

## Interaction with Forward Slash as a Separator

Although the [Forward Slash as a Separator proposal] has not yet been integrated
into the canonical spec, it will affect some of the constructs modified by this
proposal. This section defines additional modifications to the spec *as it will
exist* when that proposal is integrated.

[Forward Slash as a Separator proposal]: slash-separator.md

Remove "or `/`" from the definition of a calculation-safe `ProductExpression`.
Add "An unbracketed `SlashListExpression` with more than one element, all of
which are calculation-safe" to the list of calculation-safe expressions.

Replace "evaluating each `Expression`" with "[adjusting slash precedence] in and
then evaluating each `Expression`" in [evaluting a `FunctionCall` as a
calculation].

[adjusting slash precedence]: #adjusting-slash-precedence
[evaluting a `FunctionCall` as a calculation]: #evaluating-a-functioncall-as-a-calculation

### Adjusting Slash Precedence

This algorithm takes a calculation-safe expression `expression` and returns
another calculation-safe expression with the precedence of
`SlashListExpression`s adjusted to match division precedence.

* Return a copy of `expression` except, for each `SlashListExpression`:

  * Let `left` be the first element of the list.

  * For each remaining element `right`:

    * If `left` and `right` are both `SumExpression`s:

      * Let `last-left` be the last operand of `left` and `first-right` the
        first operand of `right`.

      * Set `left` to a `SumExpression` that begins with all operands and
        operators of `left` except `last-left`, followed by a
        `SlashListExpression` with elements `last-left` and `first-right`,
        followed by all operators and operands of `right` except `first-right`.

        > For example, `slash-list(1 + 2, 3 + 4)` becomes `1 + (2 / 3) + 4`.

    * Otherwise, if `left` is a `SumExpression`:

      * Let `last-left` be the last operand of `left`.

      * Set `left` to a `SumExpression` that begins with all operands and
        operators of `left` except `last-left`, followed by a
        `SlashListExpression` with elements `last-left` and `right`.

        > For example, `slash-list(1 + 2, 3)` becomes `1 + (2 / 3)`.

    * Otherwise, if `right` is a `SumExpression` or a `ProductExpression`:

      * Let `first-right` be the first operand of `right`.

      * Set `left` to an expression of the same type as `right` that begins a
        `SlashListExpression` with elements `left` and `first-right`, followed
        by operators and operands of `right` except `first-right`.

        > For example, `slash-list(1, 2 * 3)` becomes `(1 / 2) * 3`.

    * Otherwise, if `left` is a slash-separated list, add `right` to the end.

    * Otherwise, set `left` to a slash-separated list containing `left` and
      `right`.

  * Replace each element in `left` with the result of adjusting slash precedence
    in that element.

  * Replace the `SlashListExpression` with `left` in the returned expression.

### `SlashListExpression`

To evaluate a `SlashListExpression` as a calculation value:

* Let `left` be the result of evaluating the first element of the list as a
  calculation value.

* For each remaining element `element`:

  * Let `right` be the result of evaluating `element` as a calculation value.

  * Set `left` to a `CalcOperation` with operator `"/"`, `left`, and `right`.

* Return `left`.

## API

### Types

#### `CalculationInterpolation`

Replace the definition of this class, other than its TypeScript API, with the
following:

A deprecated alternative JS API representation of an unquoted Sass string that's
always surrounded by parentheses. It's never returned by the Sass compiler, but
for backwards-compatibility users may still construct it and pass it to the Sass
compiler.

> `CalculationInterpolation`s are no longer generated by the Sass compiler,
> because it can now tell at evaluation time whether an interpolation was
> originally surrounded by parentheses. However, until we make a breaking
> revision of the JS API, users may continue to pass `CalculationInterpolation`s

##### `internal`

A private property like [`Value.internal`] that refers to a Sass string.

##### Constructor

Creates a `CalculationInterpolation` with its `internal` set to an unquoted Sass
string with text `"(" + value + ")"` and returns it.

##### `value`

Returns [`internal`](#internal)'s `value` field's text, without the leading and
trailing parentheses.

##### `equals`

Whether `other` is a `CalculationInterpolation` and [`internal`](#internal) is
equal to `other.internal` in Sass.

##### `hashCode`

Returns the same number for any two `CalculationInterpolation`s that are equal
according to [`equals`](#equals).

## Embedded Protocol

### `CalculationValue.value.interpolation`

Add the following to this field's documentation:

The compiler must treat this as identical to a `string` option whose value is
`"(" + interpolation + ")"`.

This field is deprecated and hosts should avoid using it.

## Deprecation Process

This proposal causes two breaking changes, each of which will be mitigated by
supporting something very close to the old behavior with a deprecation warning
until the next major version release.

### `abs-percent`

> Under this proposal, if a number with unit `%` is passed to the global `abs()`
> function, it will be emitted as a plain CSS `abs()` rather than returning the
> absolute value of the percentage itself.

During the deprecation period, when simplifying a calculation named `"abs"`
whose sole argument is a number *without* [known units], return the result of
calling `math.abs()` with that number and emit a deprecation warning named
`abs-percent`.
