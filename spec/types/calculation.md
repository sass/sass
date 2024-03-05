# Calculations

## Table of Contents

* [Definitions](#definitions)
  * [Calculation-Safe Expression](#calculation-safe-expression)
* [Types](#types)
  * [Operations](#operations)
    * [Equality](#equality)
  * [Serialization](#serialization)
    * [Calculation](#calculation)
    * [`CalculationOperation`](#calculationoperation)
    * [`Number`](#number)
* [Procedures](#procedures)
  * [Evaluating a `FunctionCall` as a Calculation](#evaluating-a-functioncall-as-a-calculation)
  * [Adjusting Slash Precedence](#adjusting-slash-precedence)
  * [Evaluating an Expression as a Calculation Value](#evaluating-an-expression-as-a-calculation-value)
  * [Simplifying a Calculation](#simplifying-a-calculation)
  * [Simplifying a `CalculationValue`](#simplifying-a-calculationvalue)
* [Semantics](#semantics)
  * [`FunctionExpression` and `Variable`](#functionexpression-and-variable)
  * [`SumExpression` and `ProductExpression`](#sumexpression-and-productexpression)
  * [`SlashListExpression`](#slashlistexpression)
  * [`SpaceListExpression`](#spacelistexpression)
  * [`ParenthesizedExpression`](#parenthesizedexpression)
  * [`InterpolatedIdentifier`](#interpolatedidentifier)

## Definitions

### Calculation-Safe Expression

An expression is "calculation-safe" if it is one of:

* A [`FunctionExpression`].
* A `ParenthesizedExpression` whose contents is calculation-safe.
* A `SumExpression` whose operands are calculation-safe.
* A `ProductExpression` whose operator is `*` and whose operands are
  calculation-safe.
* A `Number`.
* A `Variable`.
* An `InterpolatedIdentifier`.
* An unbracketed `SpaceListExpression` or `SlashListExpression` with more than
  one element, whose elements are all calculation-safe.

[`FunctionExpression`]: ../functions.md#syntax

> Because calculations have special syntax in CSS, only a subset of SassScript
> expressions are valid (and these are interpreted differently than elsewhere).

## Types

The value type known as a "calculation" has the following structure:

```ts
interface Calculation {
  name: string;
  arguments: CalculationValue[];
}

type CalculationValue =
  | Number
  | UnquotedString
  | CalculationOperation
  | Calculation;

interface CalculationOperation {
  operator: '+' | '-' | '*' | '/';
  left: CalculationValue;
  right: CalculationValue;
}
```

### Operations

A calculation follows the default behavior of all SassScript operations, except
that it throws an error if used as an operand of a:

* unary or binary `-` operation,
* unary `+` operation,
* binary `+` operation where the other operand is not a string,

and equality is defined as below.

> This helps ensure that if a user expects a number and receives a calculation
> instead, it will throw an error quickly rather than propagating as an unquoted
> string. Binary `+` with a string is allowed specifically for
> backwards-compatibility with the `$variable + ""` pattern for converting a
> value to a string to dynamically inspect it.

#### Equality

Two calculations are considered equal if their names are equal, they have the
same number of arguments, and each argument in one calculation is equal to the
corresponding argument in the other.

`CalculationOperation` values are equal if each field in one value is equal to
the corresponding field in the other.

### Serialization

#### Calculation

To serialize a calculation, emit its name followed by "(", then each of its arguments
separated by ",", then ")".

#### `CalculationOperation`

To serialize a `CalculationOperation`:

* Let `left` and `right` be the result of serializing the left and right values,
  respectively.

* If the operator is `"*"` or `"/"` and the left value is a
  `CalculationOperation` with operator `"+"` or `"-"`, emit `"("` followed by
  `left` followed by `")"`. Otherwise, emit `left`.

* Emit `" "`, then the operator, then `" "`.

* If either:

  * the operator is `"*"` or `"-"` and the right value is a
    `CalculationOperation` with operator `"+"` or `"-"`, or
  * the operator is `"/"` and the right value is a `CalculationOperation`,
  * the operator is `"/"` and the right value is a degenerate number with one or
    more units.

  emit `"("` followed by `right` followed by `")"`. Otherwise, emit `right`.

#### `Number`

To serialize a `Number` within a `CalculationExpression`:

* If the number is [degenerate]:

  * If the number has more than one numerator unit, or more than zero denominator
    units, throw an error.

  * Otherwise, [convert the number to a calculation], then serialize the
    resulting calculation's sole argument.

  [degenerate]: number.md#degenerate-number
  [convert the number to a calculation]: number.md#converting-a-number-to-a-calculation

* Otherwise, serialize the number as normal.

## Procedures

### Evaluating a `FunctionCall` as a Calculation

This algorithm takes a [`FunctionCall`] `call` whose name is a plain identifier
and returns a number or a calculation.

* If `call`'s `ArgumentInvocation` contains one or more `KeywordArgument`s or
  one or more `RestArgument`s, throw an error.

* Let `calc` be a calculation whose name is the lower-case value of `call`'s
  name and whose arguments are the result of "[adjusting slash precedence] in
  and then evaluating each `Expression`" in `call`'s `ArgumentInvocation` [as a
  calculation value].

  [adjusting slash precedence]: #adjusting-slash-precedence
  [as a calculation value]: #evaluating-an-expression-as-a-calculation-value

* Return the result of [simplifying](#simplifying-a-calculation) `calc`.

### Adjusting Slash Precedence

This algorithm takes a calculation-safe expression `expression` and returns
another calculation-safe expression with the precedence of
[`SlashListExpression`]s adjusted to match division precedence.

[`SlashListExpression`]: list.md#slashlistexpression

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

### Evaluating an Expression as a Calculation Value

This algorithm takes an expression `expression` and returns a
`CalculationValue`.

* If `expression` isn't [calculation-safe], throw an error.

* Otherwise, evaluate `expression` using the semantics defined in the
  [Semantics] section if available, or the standard semantics otherwise.

  [Semantics]: #semantics

### Simplifying a Calculation

This algorithm takes a calculation `calc` and returns a number or a calculation.

> This algorithm is intended to return a value that's CSS-semantically identical
> to the input.

* If `calc` was parsed from an expression within a `SupportsDeclaration`'s
  `Expression`, but outside any interpolation, return a `calc` as-is.

* Let `arguments` be the result of [simplifying] each of `calc`'s arguments.

  [simplifying]: #simplifying-a-calculationvalue

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

  [`sass:math`]: ../built-in-modules/math.md

  > The `sass:math` functions will check units here for the functions that
  > require specific or no units.

* If `calc`'s name is `"abs"` and `arguments` contains exactly a single number
  with [known units], return the result of passing that number to the function
  in [`sass:math`] whose name matches `calc`'s.

  [known units]: number.md#known-units

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

  [`log()` function]: ../built-in-modules/math.md#log

* If `calc`'s name is `"pow"`:

  * If any argument is a number with units, throw an error.

  * Otherwise, if `arguments` contains exactly two numbers, return the result of
    passing those numbers to the [`pow()` function] in [`sass:math`].

  [`pow()` function]: ../built-in-modules/math.md#pow

* If `calc`'s name is `"atan2"` and `arguments` contains two numbers which both
  have [known units], return the result of passing those numbers to the
  [`atan2()` function] in [`sass:math`].

  > This will throw an error if either argument has units.
  >
  > `atan2()` passes percentages along to the browser because they may resolve
  > to negative values, and `atan2(-x, -y) != atan2(x, y)`.

  [`atan2()` function]: ../built-in-modules/math.md#atan2

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

  [compatible]: number.md#compatible-units
  [definitely-incompatible]: number.md#possibly-compatible-numbers
  [exactly equals]: number.md#exact-equality

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

  [special variable string]: ../functions.md#special-variable-string

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

  [`math.min()`]: ../built-in-modules/math.md#min
  [`math.max()`]: ../built-in-modules/math.md#max

* Otherwise, return a calculation with the same name as `calc` and `arguments`
  as its arguments.

### Simplifying a `CalculationValue`

This algorithm takes a `CalculationValue` `value` and returns a
`CalculationValue`.

> This algorithm is intended to return a value that's CSS-semantically identical
> to the input.

* If `value` is a number or unquoted string, return it as-is.

* If `value` is a calculation:

  * Let `result` be the result of [simplifying] `value`.

  * If `result` isn't a calculation whose name is `"calc"`, return `result`.

  * If `result`'s argument isn't an unquoted string, return `result`'s argument.

  * If `result`'s argument begins case-insensitively with `"var("`; or if it
    contains whitespace, `"/"`, or `"*"`; return `"(" +` result's argument `+
    ")"` as an unquoted string.

    > This is ensures that values that could resolve to operations end up
    > parenthesized if used in other operations. It's potentially a little
    > overzealous, but that's unlikely to be a major problem given that the
    > output is still smaller than including the full `calc()` and we don't want
    > to encourage users to inject calculations with interpolation anyway.

* Otherwise, `value` must be a `CalculationOperation`. Let `left` and `right` be
  the result of simplifying `value.left` and `value.right`, respectively.

* Let `operator` be `value.operator`.

* If `operator` is `"+"` or `"-"`:

  * If `left` and `right` are both numbers with [compatible] units, return
    `left + right` or `left - right`, respectively.

  * Otherwise, if `left` and `right` are both numbers, the `name` of the
    innermost `Calculation` that contains `value` is `"min"` or `"max"`, and
    either `left` or `right` is unitless, return `left + right` or `left -
    right`, respectively.

    > This preserves backwards-compatibility with Sass's old global `min()` and
    > `max()` functions, most of which are now parsed as `CssMinMax`es.

  * Otherwise, if either `left` or `right` is a number with more than one
    numerator unit or more than zero denominator units, throw an error.

  * Otherwise, if `left` and `right` are [definitely-incompatible] numbers,
    throw an error.

  * If `right` is a number whose value is fuzzy-less-than zero, set `right` to
    `right * -1` and set `operator` to `"-"` or `"+"`, respectively.

  * Return a `CalculationOperation` with `operator`, `left`, and `right`.

* If `operator` is `"*"` or `"/"`:

  * If `left` and `right` are both numbers, return `left * right` or
    `math.div(left, right)`, respectively.

  * Otherwise, return a `CalculationOperation` with `operator`, `left`, and
    `right`.

## Semantics

The following semantics only apply when evaluating expressions [as calculation
values].

[as calculation values]: #evaluating-an-expression-as-a-calculation-value

### `FunctionExpression` and `Variable`

To evaluate a `FunctionExpression` or a `Variable` as a calculation value,
evaluate it using the standard semantics. If the result is a number, an unquoted
string, or a calculation, return it. Otherwise, throw an error.

> Allowing variables to return unquoted strings here supports referential
> transparency, so that `$var: fn(); calc($var)` works the same as `calc(fn())`.

### `SumExpression` and `ProductExpression`

To evaluate a `SumExpresssion` or a `ProductExpression` as a calculation value:

* Let `left` be the result of evaluating the first operand as a calculation
  value.

* For each remaining `"+"`, `"-"`, `"*"`, or `"/"` token `operator` and operand
  `operand`:

  * Let `right` be the result of evaluating `operand` as a calculation value.

  * Set `left` to a `CalcOperation` with `operator`, `left`, and `right`.

* Return `left`.

### `SlashListExpression`

To evaluate a `SlashListExpression` as a calculation value:

* Let `left` be the result of evaluating the first element of the list as a
  calculation value.

* For each remaining element `element`:

  * Let `right` be the result of evaluating `element` as a calculation value.

  * Set `left` to a `CalcOperation` with operator `"/"`, `left`, and `right`.

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

    [serializing]: #serialization

  * If `element` is a `CalcOperation` that was produced by evaluating a
    `ParenthesizedExpression`, set `css` to `"(" + css + ")"`.

  * Append `css` to `serialized`.

* Return an unquoted strings whose contents are the elements of `serialized`
  separated by `" "`.

### `ParenthesizedExpression`

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

### `InterpolatedIdentifier`

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
