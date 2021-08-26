# First-Class `calc()`: Draft 2

*([Issue](https://github.com/sass/sass/issues/818),
[Changelog](first-class-calc.changes.md))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * ["Contagious" Calculations](#contagious-calculations)
    * [Returning Numbers](#returning-numbers)
    * [Interpolation in `calc()`](#interpolation-in-calc)
    * [Vendor Prefixed `calc()`](#vendor-prefixed-calc)
    * [Complex Simplification](#complex-simplification)
* [Definitions](#definitions)
  * [Possibly-Compatible Units](#possibly-compatible-units)
  * [Possibly-Compatible Numbers](#possibly-compatible-numbers)
  * [Special Number](#special-number)
  * [Potentially Slash-Separated Number](#potentially-slash-separated-number)
* [Syntax](#syntax)
  * [`SpecialFunctionExpression`](#specialfunctionexpression)
  * [`CalcExpression`](#calcexpression)
  * [`CssMinMax`](#cssminmax)
* [Types](#types)
  * [Operations](#operations)
    * [Equality](#equality)
  * [Serialization](#serialization)
    * [Calculation](#calculation)
    * [`CalculationOperation`](#calculationoperation)
    * [`CalculationInterpolation`](#calculationinterpolation)
* [Procedures](#procedures)
  * [Simplifying a Calculation](#simplifying-a-calculation)
  * [Simplifying a `CalculationValue`](#simplifying-a-calculationvalue)
* [Semantics](#semantics)
  * [`CalcExpression`](#calcexpression-1)
  * [`ClampExpression`](#clampexpression)
  * [`CssMinMax`](#cssminmax-1)
  * [`CalcArgument`](#calcargument)
  * [`CalcSum`](#calcsum)
  * [`CalcProduct`](#calcproduct)
  * [`CalcValue`](#calcvalue)
* [Functions](#functions)
  * [`meta.type-of()`](#metatype-of)
  * [`meta.calc-name()`](#metacalc-name)
  * [`meta.calc-args()`](#metacalc-args)

## Background

> This section is non-normative.

CSS's [`calc()`] syntax for mathematical expressions has existed for a long
time, and it's always represented a high-friction point in its interactions with
Sass. Sass currently treats `calc()` expressions as fully opaque, allowing
almost any sequence of tokens within the parentheses and evaluating it to an
unquoted string. Interpolation is required to use Sass variables in `calc()`
expressions, and once an expression is created it can't be inspected or
manipulated in any way other than using Sass's string functions.

[`calc()`]: https://drafts.csswg.org/css-values-3/#calc-notation

As `calc()` and related mathematical expression functions become more widely
used in CSS, this friction is becoming more and more annoying. In addition, the
move towards using [`/` as a separator] makes it desirable to use `calc()`
syntax as a way to write expressions using mathematical syntax that can be
resolved at compile-time.

[`/` as a separator]: ../accepted/slash-separator.md

## Summary

> This section is non-normative.

This proposal changes `calc()` (and other supported mathematical functions) from
being parsed as unquoted strings to being parsed in-depth, and sometimes
(although not always) producing a new data type known as a "calculation". This
data type represents mathematical expressions that can't be resolved at
compile-time, such as `calc(10% + 5px)`, and allows those expressions to be
combined gracefully within further mathematical functions.

To be more specific: a `calc()` expression will be parsed according to the [CSS
syntax], with additional support for Sass variables, functions, and (for
backwards compatibility) interpolation. Sass will perform as much math as is
possible at compile-time, and if the result is a single number it will return
that number. Otherwise, it will return a calculation that represents the
(simplified) expression that can be resolved in the browser.

[CSS syntax]: https://drafts.csswg.org/css-values-3/#calc-syntax

For example:

* `calc(1px + 10px)` will return the number `11px`.

* Similarly, if `$length` is `10px`, `calc(1px + $length)` will return `11px`.

* However, `calc(1px + 10%)` will return the calc `calc(1px + 10%)`.

* If `$length` is `calc(1px + 10%)`, `calc(1px + $length)` will return
  `calc(2px + 10%)`.

* Sass functions can be used directly in `calc()`, so `calc(1% +
  math.round(15.3px))` returns `calc(1% + 15px)`.

Note that calculations cannot generally be used in place of numbers. For
example, `1px + calc(1px + 10%)` will produce an error, as will
`math.round(calc(1px + 10%))`.

For backwards compatibility, `calc()` expressions that contain interpolation
will continue to be parsed using the old highly-permissive syntax, although this
behavior will eventually be deprecated and removed. These expressions will still
return calculation values, but they'll never be simplified or resolve to plain
numbers.

### Design Decisions

#### "Contagious" Calculations

In this proposal, calculation objects throw errors if they're used with normal
SassScript level math operations (`+`, `-`, `*`, and `%`). Another option would
have been to make calculations "contagious", so that performing these operations
with at least one calculation operand would produce another calculation as a
result. For example, instead of throwing an error `1px + calc(100px + 10%)`
would produce `calc(101px + 10%)` (or possibly just `calc(1px + 100px + 10%)`).

We chose not to do this because calculations aren't *always* interchangeable
with plain numbers, so making them contagious in this way could lead to
situations where a calculation entered a set of functions that only expected
numbers and ended up producing an error far away in space or time from the
actual source of the issue. For example:

* Miriam publishes a Sass library with a function, `frobnicate()`, which does a
  bunch of arithmetic on its argument and returns a result.

* Jina tries calling `frobnicate(calc(100px + 10%))`. This works, so she commits
  it and ships to production.

* Miriam updates the implementation of `frobnicate()` to call `math.log()`,
  which does not support calculations. She doesn't realize this is a breaking
  change, since she was only ever expecting numbers to be passed.

* Jina updates to the newest version of Miriam's library and is unexpectedly
  broken.

To avoid this issue, we've made it so that the only operations that support
calculations are those within `calc()` expressions. This follows Sass's broad
principle of "don't design for users using upstream stylesheets in ways they
weren't intended to be used".

Going back to the example above, if Miriam *did* want to support calculations,
she could simply wrap `calc()` around any mathematical expressions she writes.
This will still return plain numbers when given compatible numbers as inputs,
but it will also make it clear that `calc()`s are supported and that Miriam
expects to support them on into the future.

#### Returning Numbers

In plain CSS, the expression `calc(<number>)` is not strictly equivalent to the
same `<number>` on its own (and same for `calc(<dimension>)`). In certain
property contexts, a `calc()`'s value can be rounded or clamped, so for example
`width: calc(-5px)` and `z-index: calc(1.2)` are equivalent to `width: 0` and
`z-index: 1`.

In this proposal, rather than preserving calculations whose arguments are plain
numbers or dimensions as `calc()` expressions, we convert them to Sass numbers.
This is technically a slight violation of CSS compatibility, because it avoids
the rounding/clamping behavior described above. However, we judge this slight
incompatibility to be worthwhile for a number of reasons:

* We get a lot of value from allowing calculations to simplify to numbers. In
  addition to making it easier to work with `calc()` for its own sake, this
  simplification makes it possible to use `calc()` to write division expressions
  using `/`. Since `/`-as-division is otherwise deprecated due to `/` being used
  as a separator in CSS, this provides a substantial ergonomic benefit to users.

* Any situation where a *build-time calculation* could produce a number that
  needs to be clamped or rounded in order to be valid is likely to be a result
  of user error, and we generally have lower compatibility requirements for
  errors than we do for valid and useful CSS. We know of no use-case for writing
  CSS like `width: calc(-5px)` instead of `width: 0`. The use-case for CSS's
  clamping and rounding behavior is for browse-time calculations like
  `calc(20px - 3em)`, and these will continue to be emitted as `calc()`
  expressions.

* It's very easy to explicitly preserve the CSS behavior if it's desired. A
  `CalculationInterpolation` will always produce a `calc()` expression, so
  `calc(#{-5px})` can be used to force a calculation that won't return a number.
  In addition, the `clamp()` syntax and `math.round()` function can be used to
  do build-time clamping and rounding if that's desired.

#### Interpolation in `calc()`

Historically, interpolation has been the only means of injecting SassScript
values into `calc()` expressions, so for backwards compatibility, we must
continue to support it to some degree. Exactly to what degree and how it
integrates with first-class calculation is a question with multiple possible
answers, though.

The answer we settled on was to handle interpolation in a similar way to how we
handled backwards-compatibility with Sass's [`min()` and `max()` functions]: by
parsing `calc()` expressions using the old logic if they contain any
interpolation and continuing to treat those values as opaque strings, and only
using the new parsing logic for calculations that contain no interpolation. This
is maximally backwards-compatible and it doesn't require interpolated
calculations to be reparsed after interpolation.

[`min()` and `max()` functions]: ../accepted/min-max.md

Here are some alternatives we considered:

1. Re-parsing a calculation that contains interpolation once the interpolation
   has been resolved, and using the result as a calculation object rather than
   an unquoted string. For example, `calc(#{"1px + 2px"})` would return `3px`
   rather than `calc(1px + 2px)`. However, doing another parse at
   evaluation-time would add substantial complexity and some amount of runtime
   overhead. The return-on-investment would also be inherently limited, since
   we're planning on gradually transitioning users away from interpolation in
   `calc()` anyway.

2. Treating interpolation another type of [`CalcValue`] that participates in the
   normal parsing flow of a [`CalcArgument`]. This is a simpler and more
   efficient method since it doesn't require parser lookahead, and it supports
   common cases like `calc(#{$var} + 10%)` well. However, it doesn't support
   cases like `calc(1px #{$op} 10%)` which are currently supported. This
   backwards-incompatibility is likely to cause real user pain for a feature as
   widely-used as `calc()`.

   [`CalcValue`]: #calcexpression
   [`CalcArgument`]: #calcexpression

#### Vendor Prefixed `calc()`

Although `calc()` is now widely supported in all modern browsers, older versions
of Firefox, Chrome, and Safari supported it only with a vendor prefix. Sass in
turn supported those browsers by handling `calc()`'s special function parsing
with arbitrary vendor prefixes as well. However, time has passed, those browser
versions have essentially no usage any more, and we don't anticipate anyone is
looking to write new stylesheets that target them.

As such, this proposal only adds first-class calculation support for the
`calc()` function without any prefixes. For backwards-compatibility,
vendor-prefixed `calc()` expressions will continue to be parsed as opaque
special functions the way they always have, but they will not be interoperable
with any of the new calculation features this proposal adds.

#### Complex Simplification

Since this spec does have support for simplifying calculations to some degree,
it would make some sense for it to try to minimize the output size of all
`calc()` and related expressions it emits to CSS. However, as currently written,
it only simplifies enough to ensure that if the entire calculation reduces to a
single number that number can be returned.

For example, the current specification doesn't simplify expressions like
`calc(1px + var(--length) + 1px)` to `calc(2px + var(--length))` or `calc(-1 *
(10% + 5px))` to `calc(-10% - 5px)`. This is for ease of specification and
implementation: simplifications of these sorts are highly complex and would make
designing, testing, and implementing this spec substantially more difficult.

It's possible a future proposal will add support for this advanced
simplification logic later on. Until then, it's probably better to leave it to
post-processors that are dedicated to CSS minification.

## Definitions

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

### Special Number

Replace the definition of [special number string] with the following definition:

[special number string]: ../spec/functions.md#special-number-string

A *special number* is either:

* a calculation, or
* an unquoted string that CSS will recognize as a function that may return a
  number. For the purposes of Sass, this is any unquoted string that begins with
  `calc(`, `var(`, `env(`, `clamp(`, `min(`, or `max(`. This matching is
  case-insensitive.

In addition, replace all references to special number strings with references to special
numbers.

### Potentially Slash-Separated Number

Add `CalcExpression`s, `ClampExpression`s, `CssMinMax`es to the list of operands
of the `/` operator that can create a [potentially slash-separated number].

[potentially slash-separated number]: ../spec/types/number.md#potentially-slash-separated-number

## Syntax

### `SpecialFunctionExpression`

This proposal replaces the definition of [`SpecialFunctionName`] with the
following:

[`SpecialFunctionName`]: ../spec/syntax.md#specialfunctionexpression

<x><pre>
**SpecialFunctionName**¹      ::= VendorPrefix? ('element(' | 'expression(')
&#32;                           | VendorPrefix 'calc('
</pre></x>

1: The string `calc(` is matched case-insensitively.

### `CalcExpression`

This proposal defines a new production `CalcExpression`. This expression is
parsed in a SassScript context when an expression is expected and the input
stream starts with an identifier with value `calc` (ignoring case) followed
immediately by `(`.

The grammar for this production is:

<x><pre>
**CalcExpression** ::= 'calc('¹ CalcArgument ')'
**ClampExpression** ::= 'clamp('¹ CalcArgument ( ',' CalcArgument ){2} ')'
**CalcArgument**²  ::= InterpolatedDeclarationValue† | CalcSum
**CalcSum**     ::= CalcProduct (('+' | '-')³ CalcProduct)\*
**CalcProduct** ::= CalcValue (('\*' | '/') CalcValue)\*
**CalcValue**   ::= '(' CalcArgument ')'
&#32;             | CalcExpression
&#32;             | ClampExpression
&#32;             | MinMaxExpression
&#32;             | FunctionExpression⁴
&#32;             | Number
&#32;             | Variable†
</pre></x>

1: The strings `calc(` and `clamp(` are matched case-insensitively.

2: A `CalcArgument` is only parsed as an `InterpolatedDeclarationValue` if it
includes interpolation, unless that interpolation is within a region bounded by
parentheses (a `FunctionExpression` counts as parentheses).

3: Whitespace is required around these `"+"` and `"-"` tokens.

4: This `FunctionExpression` cannot begin with `min(`, `max(`, or `clamp(`,
case-insensitively.

†: These productions are invalid in plain CSS syntax.

[`<function-token>`]: https://drafts.csswg.org/css-syntax-3/#ref-for-typedef-function-token%E2%91%A0

> The `CalcArgument` production provides backwards-compatibility with the
> historical use of interpolation to inject SassScript values into `calc()`
> expressions. Because interpolation could inject any part of a `calc()`
> expression regardless of syntax, for full compatibility it's necessary to
> parse it very expansively.

### `CssMinMax`

This proposal replaces the reference to `CalcValue` in the definition of
[`CssMinMax`] with `CalcArgument`.

[`CssMinMax`]: ../spec/syntax.md#minmaxexpression

> Note that this increases the number of cases where a `MinMaxExpression` will
> be parsed as a `CssMinMax` rather than a `FunctionExpression` (for example,
> `min($foo, $bar)` is now a valid `CssMinMax` where it wasn't before).
> Fortunately, this is backwards-compatible, since all such `MinMaxExpression`s
> that were already valid will be simplified down into the same number they
> returned before.

## Types

This proposal introduces a new value type known as a "calculation", with the
following structure:

```ts
interface Calculation {
  name: string;
  arguments: CalculationValue[];
}

type CalculationValue =
  | Number
  | UnquotedString
  | CalculationInterpolation
  | CalculationOperation
  | Calculation;
  
interface CalculationInterpolation {
  value: string;
}

interface CalculationOperation {
  operator: '+' | '-' | '*' | '/';
  left: CalculationValue;
  right: CalculationValue;
}
```

Unless otherwise specified, when this specification creates a calculation, its
name is "calc".

### Operations

A calculation follows the default behavior of all SassScript operations, except
that it throws an error if used as an operand of a unary or binary `+` or `-`
operation, and equality is defined as below.

> This helps ensure that if a user expects a number and receives a calculation
> instead, it will throw an error quickly rather than propagating as an
> unquoted string.

#### Equality

Two calculations are considered equal if their names are equal, they have the
same number of arguments, and each argument in one calculation is equal to the
corresponding argument in the other.

`CalculationOperation` and `CalculationInterpolation` values are equal if each
field in one value is equal to the corresponding field in the other.

### Serialization

#### Calculation

To serialize a calculation, emit its name followed by "(", then each of its arguments
separated by ",", then ")".

#### `CalculationOperation`

To serialize a `CalculationOperation`:

* Let `left` and `right` be the result of serializing the left and right values,
  respectively.

* If either:

  * the left value is a `CalculationInterpolation`, or
  * the operator is `"*"` or `"/"` and the left value is a
    `CalculationOperation` with operator `"+"` or `"-"`,

  emit `"("` followed by `left` followed by `")"`. Otherwise, emit `left`.

* Emit `" "`, then the operator, then `" "`.

* If either:

  * the right value is a `CalculationInterpolation`, or
  * the operator is `"*"` and the right value is a `CalculationOperation` with
    operator `"+"` or `"-"`, or
  * the operator is `"/"` and the right value is a `CalculationOperation`,
  
  emit `"("` followed by `right` followed by `")"`. Otherwise, emit `right`.

#### `CalculationInterpolation`

To serialize a `CalculationInterpolation`, emit its `value`.

## Procedures

### Simplifying a Calculation

This algorithm takes a calculation `calc` and returns a number or a calculation.

> This algorithm is intended to return a value that's CSS-semantically identical
> to the input.

* Let `arguments` be the result of [simplifying](#simplifying-a-calculationvalue) each
  of `calc`'s arguments.

* If `calc`'s name is `"calc"`, the syntax guarantees that `arguments` contain
  only a single argument. If that argument is a number or calculation, return
  it.

* If `calc`'s name is `"clamp"`, `arguments` has fewer than three elements, and
  none of those are unquoted strings or `CalculationInterpolation`s, throw an
  error.

  > It's valid to write `clamp(var(--three-args))` or `clamp(#{"1, 2, 3"})`, but
  > otherwise `clamp()` has to have three physical arguments.

* If `calc`'s name is `"min"`, `"max"`, or `"clamp"` and `arguments` are all
  numbers:

  * If those arguments' units are mutually [compatible], return the result of
    calling [`math.min()`], [`math.max()`], or `math.clamp()` (respectively)
    with those arguments.

    [compatible]: ../spec/types/number.md#compatible-units
    [`math.min()`]: ../spec/built-in-modules/math.md#min
    [`math.max()`]: ../spec/built-in-modules/math.md#max

  * Otherwise, if any two of those arguments are [definitely-incompatible],
    throw an error.

    [definitely-incompatible]: #possibly-compatible-numbers

* Otherwise, return a calculation with the same name as `calc` and `arguments`
  as its arguments.

### Simplifying a `CalculationValue`

This algorithm takes a `CalculationValue` `value` and returns a
`CalculationValue`.

> This algorithm is intended to return a value that's CSS-semantically identical
> to the input.

* If `value` is a number, unquoted string, or `CalculationInterpolation`, return
  it as-is.

* If `value` is a calculation:

    * Let `result` be the result of [simplifying] `value`.

    * If `result` is a calculation whose name is `"calc"`, return `result`'s
      single argument.

    * Otherwise, return `result`.

  [simplifying]: #simplifying-a-calculation

* Otherwise, `value` must be a `CalculationOperation`. Let `left` and `right` be
  the result of simplifying `value.left` and `value.right`, respectively.

* Let `operator` be `value.operator`.

* If `operator` is `"+"` or `"-"`:

  * If `left` and `right` are both numbers with [compatible] units, return
    `left + right` or `left - right`, respectively.

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

### `CalcExpression`

To evaluate a `CalcExpression`:

* Let `calc` be a calculation whose name is `"calc"` and whose only argument is
  the result of [evaluating the expression's `CalcArgument`](#calcargument).

* Return the result of [simplifying] `calc`.

### `ClampExpression`

To evaluate a `ClampExpression`:

* Let `clamp` be a calculation whose name is `"clamp"` and whose arguments are the
  results of [evaluating the expression's `CalcArgument`s](#calcargument).

* Return the result of [simplifying] `clamp`.

### `CssMinMax`

To evaluate a `CssMinMax`:

* Let `calc` be a calculation whose name is `"min"` or `"max"` according to the
  `CssMinMax`'s first token, and whose arguments are the results of [evaluating
  the expression's `CalcArgument`s](#calcargument).

* Return the result of [simplifying] `calc`.

### `CalcArgument`

To evaluate a `CalcArgument` production `argument` into a `CalculationValue` object:

* If `argument` is an `InterpolatedDeclarationValue`, evaluate it and return a
  `CalculationInterpolation` whose `value` is the resulting string.

* Otherwise, return the result of [evaluating `argument`'s
  `CalcValue`](#calcvalue).

### `CalcSum`

To evaluate a `CalcSum` production `sum` into a `CalculationValue` object:

* Left `left` be the result of evaluating the first `CalcProduct`.

* For each remaining "+" or "-" token `operator` and `CalcProduct` `product`:

  * Let `right` be the result of evaluating `product`.

  * Set `left` to a `CalcOperation` with `operator`, `left`, and `right`.

* Return `left`.

### `CalcProduct`

To evaluate a `CalcProduct` production `product` into a `CalculationValue`
object:

* Left `left` be the result of evaluating the first `CalcValue`.

* For each remaining "*" or "/" token `operator` and `CalcValue` `value`:

  * Let `right` be the result of evaluating `value`.

  * Set `left` to a `CalcOperation` with `operator`, `left`, and `right` as its
    values.

* Return `left`.

### `CalcValue`

To evaluate a `CalcValue` production `value` into a `CalculationValue` object:

* If `value` is a `CalcArgument`, `CssMinMax`, or `Number`, return the result of
  evaluating it.

* If `value` is a `FunctionExpression` or `Variable`, evaluate it. If the result
  is a number, an unquoted string, or a calculation, return it. Otherwise, throw
  an error.

  > Allowing variables to return unquoted strings here supports referential
  > transparency, so that `$var: fn(); calc($var)` works the same as
  > `calc(fn())`.

## Functions

### `meta.type-of()`

Add the following clause to the [`meta.type-of()`] function and the top-level
`type-of()` function:

[`meta.type-of()`]: ../spec/built-in-modules/meta.md#type-of

* If `$value` is a calculation, return an unquoted string with value
  `"calculation"`.

### `meta.calc-name()`

This is a new function in the `sass:meta` module.

```
meta.calc-name($calc)
```

* If `$calc` is not a calculation, throw an error.

* Return `$calc`'s name as a quoted string.

### `meta.calc-args()`

This is a new function in the `sass:meta` module.

```
meta.calc-args($calc)
```

* If `$calc` is not a calculation, throw an error.

* Let `args` be an empty list.

* For each argument `arg` in `$calc`'s arguments:

  * If `arg` is a number or a calculation, add it to `args`.

  * Otherwise, [serialize](#serialization) `arg` and add the result to `args` as
    an unquoted string.

* Return `args` as an unbracketed comma-separated list.
