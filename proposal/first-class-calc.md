# First-Class `calc()`: Draft 1

*([Issue](https://github.com/sass/sass/issues/818))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * ["Contagious" Calcs](#contagious-calcs)
    * [Interpolation in `calc()`](#interpolation-in-calc)
    * [Vendor Prefixed `calc()`](#vendor-prefixed-calc)
* [Syntax](#syntax)
  * [`SpecialFunctionExpression`](#specialfunctionexpression)
  * [`CalcExpression`](#calcexpression)
  * [`CssMinMax`](#cssminmax)
* [Types](#types)
  * [Operations](#operations)
  * [Serialization](#serialization)
    * [Calc](#calc)
    * [`CalcOperation`](#calcoperation)
* [Procedures](#procedures)
  * [Simplifying a Calc](#simplifying-a-calc)
  * [Simplifying a `CalcValue`](#simplifying-a-calcvalue)
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
(although not always) producing a new data type known as a "calc". This data
type represents mathematical expressions that can't be resolved at compile-time,
such as `calc(10% + 5px)`, and allows those expressions to be combined
gracefully within further mathematical functions.

To be more specific: a `calc()` expression will be parsed according to the [CSS
syntax], with additional support for Sass variables, functions, and (for
backwards compatibility) interpolation. Sass will perform as much math as is
possible at compile-time, and if the result is a single number it will return
that number. Otherwise, it will return a calc that represents the (simplified)
expression that can be resolved in the browser.

[CSS syntax]: https://drafts.csswg.org/css-values-3/#calc-syntax

For example:

* `calc(1px + 10px)` will return the number `11px`.

* Similarly, if `$length` is `10px`, `calc(1px + $length)` will return `11px`.

* However, `calc(1px + 10%)` will return the calc `calc(1px + 10%)`.

* If `$length` is `calc(1px + 10%)`, `calc(1px + $length)` will return
  `calc(2px + $length)`.

* Sass functions can be used directly in `calc()`, so `calc(1% +
  math.round(15.3px))` returns `calc(1% + 15px)`.

Note that calcs cannot generally be used in place of numbers. For example,
`1px + calc(1px + 10%)` will produce an error, as will `math.round(calc(1px +
10%))`.

For backwards compatibility, `calc()` expressions that contain interpolation
will continue to be parsed using the old highly-permissive syntax, although this
behavior will eventually be deprecated and removed. These expressions will still
return calc values, but they'll never be simplified or resolve to plain numbers.

### Design Decisions

#### "Contagious" Calcs

In this proposal, calc objects throw errors if they're used with normal
SassScript level math operations (`+`, `-`, `*`, `/`, and `%`). Another option
would have been to make calcs "contagious", so that performing these operations
with at least one calc operand would produce another calc as a result. For
example, instead of throwing an error `1px + calc(100px + 10%)` would produce
`calc(101px + 10%)` (or possibly just `calc(1 + 100px + 10%)`).

We chose not to do this because calcs aren't *always* interchangeable with plain
numbers, so making them contagious in this way could lead to situations where a
calc entered a set of functions that only expected numbers and ended up
producing an error far away in space or time from the actual source of the
issue. For example:

* Miriam publishes a Sass library with a function, `frobnicate()`, which does a
  bunch of arithmetic on its argument and returns a result.

* Jina tries calling `frobnicate(calc(100px + 10%))`. This works, so she commits
  it and ships to production.

* Miriam updates the implementation of `frobnicate()` to call `math.log()`,
  which does not support calcs. She doesn't realize this is a breaking change,
  since she was only ever expecting numbers to be passed.

* Jina updates to the newest version of Miriam's library and is unexpectedly
  broken.

To avoid this issue, we've made it so that the only operations that support
calcs are those within `calc()` expressions. This follows Sass's broad principle
of "don't design for users using upstream stylesheets in ways they weren't
intended to be used".

Going back to the example above, if Miriam *did* want to support calcs, she
could simply wrap `calc()` around any mathematical expressions she writes. This
will still return plain numbers when given compatible numbers as inputs, but it
will also make it clear that `calc()`s are supported and that Miriam expects to
support them on into the future.

#### Interpolation in `calc()`

Historically, interpolation has been the only means of injecting SassScript
values into `calc()` expressions, so for backwards compatibility, we must
continue to support it to some degree. Exactly to what degree and how it
integrates with first-class calc is a question with multiple possible answers,
though.

The answer we settled on was to handle interpolation in a similar way to how we
handled backwards-compatibility with Sass's [`min()` and `max()` functions]: by
parsing `calc()` expressions using the old logic if they contain any
interpolation and continuing to treat those values as opaque strings, and only
using the new parsing logic for calcs that contain no interpolation. This is
maximally backwards-compatible and it doesn't require interpolated calcs to be
reparsed after interpolation.

[`min()` and `max()` functions]: ../accepted/min-max.md

Here are some alternatives we considered:

1. Re-parsing a calc that contains interpolation once the interpolation has been
   resolved, and using the result as in the calc object rather than an unquoted
   string. For example, `calc(#{"1px + 2px"})` would return `3px` rather than
   `calc(1px + 2px)`. However, doing another parse at evaluation-time would add
   substantial complexity and some amount of runtime overhead. The
   return-on-investment would also be inherently limited, since we're planning
   on gradually transitioning users away from interpolation in `calc()` anyway.

2. Treating interpolation as a `CalcValue` that participates in the normal
   parsing flow of a `CalcArgument`. This is a simpler and more efficient method
   since it doesn't require parser lookahead, and it supports common cases like
   `calc(#{$var} + 10%)` well. However, it doesn't support cases like `calc(1px
   #{$op} 10%)` which are currently supported. This backwards-incompatibility is
   likely to cause real user pain for a feature as widely-used as `calc()`.

#### Vendor Prefixed `calc()`

Although `calc()` is now widely supported in all modern browsers, older versions
of Firefox, Chrome, and Safari supported it only with a vendor prefix. Sass in
turn supported those browsers by handling `calc()`'s special function parsing
with arbitrary vendor prefixes as well. However, time has passed, those browser
versions have essentially no usage any more, and we don't anticipate anyone is
looking to write new stylesheets that target them.

As such, this proposal only adds first-class calc support for the `calc()`
function without any prefixes. For backwards-compatibility, vendor-prefixed
`calc()` expressions will continue to be parsed as opaque special functions the
way they always have, but they will not be interoperable with any of the new
calc features this proposal adds.

## Syntax

### `SpecialFunctionExpression`

This proposal replaces the definition of [`SpecialFunctionName`] with the
following:

[`SpecialFunctionName`]: ../spec/syntax.md#specialfunctionexpression

<x><pre>
**SpecialFunctionName**¹      ::= VendorPrefix? ('element(' | 'expression(')
&#32;                           | VendorPrefix `calc('
</pre></x>

1: The string `calc(` is matched case-insensitively.

### `CalcExpression`

This proposal defines a new production `CalcExpression`. This expression is
parsed in a SassScript context when an expression is expected and the input
stream starts with an identifier with value `calc` (ignoring case) followed
immediately by `(`.

The grammar for this production is:

<x><pre>
**CalcExpression** ::= `calc(`¹ CalcArgument ')'
**ClampExpression** ::= `clamp(`¹ CalcArgument ',' CalcArgument ',' CalcArgument ')'
**CalcArgument**²  ::= InterpolatedDeclarationValue | CalcSum
**CalcSum**     ::= CalcProduct (('+' | '-')³ CalcProduct)?
**CalcProduct** ::= CalcValue (('*' | '/') CalcValue)?
**CalcValue**   ::= '(' CalcArgument ')
&#32;             | CalcExpression
&#32;             | ClampExpression
&#32;             | CssMinMax
&#32;             | FunctionExpression⁴
&#32;             | Number
&#32;             | Variable
</pre></x>

1: The strings `calc(` and `clamp(` are matched case-insensitively.

2: A `CalcArgument` is only parsed as an `InterpolatedDeclarationValue` if it
includes interpolation, unless that interpolation is within a region bounded by
parentheses (a `FunctionExpression` counts as parentheses).

3: Whitespace is required around these `"+"` and `"-"` tokens.

4: This `FunctionExpression` cannot begin with `min(`, `max(`, or `clamp(`,
case-insensitively.

[`<function-token>`]: https://drafts.csswg.org/css-syntax-3/#ref-for-typedef-function-token%E2%91%A0

> The `CalcArgument` production provides backwards-compatibility with the
> historical use of interpolation to inject SassScript values into `calc()`
> expressions. Because interpolation could inject any part of a `calc()`
> expression regardless of syntax, for full compatibility it's necessary to
> parse it very expansively.
>
> Note that the interpolation in the definition of `CalcValue` is only reachable
> from a `CssMinMax` production, *not* from `CalcExpression`. This is
> intentional, for backwards-compatibility with the existing `CssMinMax` syntax.

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

This proposal introduces a new value type known as a "calc", with the following
structure:

```ts
interface Calc {
  name: string;
  arguments: CalcValue[];
}

type CalcValue =
  | Number
  | UnquotedString
  | CalcOperation
  | Calc;

interface CalcOperation {
  operator: '+' | '-' | '*' | '/';
  left: CalcValue;
  right: CalcValue;
}
```

Unless otherwise specified, when this specification creates a calc, its name is
"calc".

### Operations

A calc follows the default behavior of all SassScript operations, except that it
throws an error if used as an operand of a unary or binary `+` or `-` operation.

> This helps ensure that if a user expects a number and receives a calc instead,
> it will throw an error quickly rather than propagating as an unquoted string.

### Serialization

#### Calc

To serialize a calc, emit its name followed by "(", then each of its arguments
separated by ",", then ")".

#### `CalcOperation`

To serialize a `CalcOperation`:

* Let `left` and `right` be the result of serializing the left and right values,
  respectively.

* If the operator is `"*"` or `"/"` and the left value is a `CalcOperation` with
  operator `"+"` or `"-"`, emit `"("` followed by `left` followed by `")"`.
  Otherwise, emit `left`.

* Emit `" "`, then the operator, then `" "`.

* If the operator is `"*"` or `"/"` and the right value is a `CalcOperation`
  with operator `"+"` or `"-"`, emit `"("` followed by `right` followed by
  `")"`. Otherwise, emit `right`.

  > TODO: If one of the operands is a result of an interpolated expression, it
  > may need parentheses. However, we *don't* want to add parentheses for
  > unquoted strings from e.g. `var()` expressions. We need to find a way to
  > distinguish the two.

## Procedures

### Simplifying a Calc

This algorithm takes a calc `calc` and returns a number or a calc.

> This algorithm is intended to return a value that's CSS-semantically identical
> to the input.

* Let `arguments` be the result of [simplifying](#simplifying-a-calcvalue) each
  of `calc`'s arguments.

* If `calc`'s name is `"calc"`, the syntax guarantees that `arguments` contain
  only a single argument. If that argument is a number or calc, return it.

* If `calc`'s name is `min`, `max`, or `clamp` and `arguments` are all numbers
  whose units are mutually [compatible], return the result of calling
  [`math.min()`], [`math.max()`], or `math.clamp()` (respectively) with those
  arguments.

  [compatible]: ../spec/types/number.md#compatible-units
  [`math.min()`]: ../spec/built_in_modules/math.md#min
  [`math.max()`]: ../spec/built_in_modules/math.md#max

* Otherwise, return a calc with the same name as `calc` and `arguments` as its
  arguments.

### Simplifying a `CalcValue`

This algorithm takes a `CalcValue` `value` and returns a `CalcValue`.

> This algorithm is intended to return a value that's CSS-semantically identical
> to the input.

* If `value` is a number or unquoted string, return it as-is.

* If `value` is a calc:

    * Let `result` be the result of [simplifying] `value`.

    * If `result` is a calc whose name is `"calc"`, return `result`'s single
      argument.

      > TODO: If `result` was created via interpolation, it may not be
      > syntactically sound to just strip the `calc()`. For example, `calc(-1 *
      > calc(#{"1px + 10%"})` should return `calc(-1 * (1px + 10%))` rather than
      > `calc(-1 * 1px + 10%)`.

    * Otherwise, return `result`.

  [simplifying]: #simplifying-a-calc

* Otherwise, `value` must be a `CalcOperation`. Let `left` and `right` be the
  result of simplifying `value.left` and `value.right`, respectively.

* If `value.operator` is `"+"` or `"-"`:

  * If `left` and `right` are both numbers with [compatible] units, return
    `left + right` or `left - right`, respectively.

    > TODO: Should we throw an error here for units we can prove are actively
    > incompatible? For example, unitless numbers are always incompatible with
    > numbers of any units, and numbers across different known types (length +
    > time) are also invalid.

    > TODO: Should we try to simplify `calc(1px + 1% + 1px)`?

  * Otherwise, return a `CalcOperation` with `value.operator`, `left`, and
    `right`.

* If `value.operator` is `"*"` or `"/"`:

  * If `left` and `right` are both numbers, return `left * right` or
    `math.div(left, right)`, respectively.

    > TODO: Should we try to simplify `calc(2 * var(--foo) * 2)`? What about
    > `calc(-1 * (var(--foo) + 1px)`?

  * Otherwise, return a `CalcOperation` with `value.operator`, `left`, and
    `right`.

## Semantics

### `CalcExpression`

To evaluate a `CalcExpression`:

* Let `calc` be a calc whose name is `"calc"` and whose only argument is the
  result of [evaluating the expression's `CalcArgument`](#calcargument).

* Return the result of [simplifying] `calc`.

### `ClampExpression`

To evaluate a `ClampExpression`:

* Let `clamp` be a calc whose name is `"clamp"` and whose arguments are the
  results of [evaluating the expression's `CalcArgument`s](#calcargument).

* Return the result of [simplifying] `clamp`.

### `CssMinMax`

To evaluate a `CssMinMax`:

* Let `calc` be a calc whose name is `"min"` or `"max"` according to the
  `CssMinMax`'s first token, and whose arguments are the results of [evaluating
  the expression's `CalcArgument`s](#calcargument).

* Return the result of [simplifying] `calc`.

### `CalcArgument`

To evaluate a `CalcArgument` production `argument` into a `CalcValue` object:

* If `argument` is an `InterpolatedDeclarationValue`, evaluate it and return the
  resulting unquoted string.

* Otherwise, return the result of [evaluating `argument`'s
  `CalcValue`](#calcvalue).

### `CalcSum`

To evaluate a `CalcSum` production `sum` into a `CalcValue` object:

* Left `left` be the result of evaluating the first `CalcProduct`.

* For each remaining "+" or "-" token `operator` and `CalcProduct` `product`:

  * Let `right` be the result of evaluating `product`.

  * Set `left` to a `CalcOperation` with `operator`, `left`, and `right`.

* Return `left`.

### `CalcProduct`

To evaluate a `CalcProduct` production `product` into a `CalcValue` object:

* Left `left` be the result of evaluating the first `CalcValue`.

* For each remaining "*" or "/" token `operator` and `CalcValue` `value`:

  * Let `right` be the result of evaluating `value`.

  * Set `left` to a `CalcOperation` with `operator`, `left`, and `right` as its
    values.

* Return `left`.

### `CalcValue`

To evaluate a `CalcValue` production `value` into a `CalcValue` object:

* If `value` is a `CalcArgument`, `CssMinMax`, or `Number`, return the result of
  evaluating it.

* If `value` is a `FunctionExpression` or `Variable`, evaluate it. If the result
  is a number or an unquoted string, return it. Otherwise, throw an error.

  > Allowing variables to return unquoted strings here supports referential
  > transparency, so that `$var: fn(); calc($var)` works the same as
  > `calc(fn())`.

## Functions

### `meta.type-of()`

Add the following clause to the [`meta.type-of()`] function and the top-level
`type-of()` function:

[`meta.type-of()`]: ../spec/built_in_modules/meta.md#type-of

* If `$value` is a calc, return `"calc"`.

### `meta.calc-name()`

This is a new function in the `sass:meta` module.

```
meta.calc-name($calc)
```

* If `$calc` is not a calc, throw an error.

* Return `$calc`'s name as a quoted string.

### `meta.calc-args()`

This is a new function in the `sass:meta` module.

```
meta.calc-args($calc)
```

* If `$calc` is not a calc, throw an error.

* Let `args` be an empty list.

* For each argument `arg` in `$calc`'s arguments:

  * If `arg` is a number, add it to `args`.

  * Otherwise, [serialize](#serialization) `arg` and add the result to `args` as
    an unquoted string.

* Return `args` as an unbracketed comma-separated list.
