# Plain CSS `min()` and `max()`: Draft 3

*([Issue](https://github.com/sass/sass/issues/2378), [Changelog](min-max.changes.md))*

This proposal defines how Sass handles CSS's `min()` and `max()`
[math functions][].

[math functions]: https://drafts.csswg.org/css-values/#math-function

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Syntax](#syntax)

## Background

> This section is non-normative.

Since Ruby Sass 3.2, Sass has provided `min()` and `max()` functions that return
the minimum or maximum values among a set of SassScript numbers. Later, CSS
Values and Units Level 4 added support for additional [math functions][] with
special syntax like that in `calc()`, among which were `min()` and `max()`.

This presents a problem for Sass: to retain backwards-compatibility with
existing Sass stylesheets, it must support `min()` and `max()` as Sass
functions. However, to provide compatibility with CSS, it must also support them
as math functions with special syntax.

Support for CSS's `min()` and `max()` has landed in real browsers and
[Sass users want to use it][], so this should be solved with some urgency.

[Sass users want to use it]: https://github.com/sass/sass/issues/2378#issuecomment-367490840

## Summary

> This section is non-normative.

Sass will support a combined syntax for `min()` and `max()` that will parse to
either a SassScript function call or a CSS math function, depending on the
syntax of the arguments. If all arguments to a function named `min()` or `max()`
are valid arguments for CSS math functions (possibly including use of the
`var()` or `env()` functions), it's parsed as a math function. Otherwise, it's
parsed as a SassScript function.

### Design Decisions

Another possible solution to this problem would be to rename the `min()` and
`max()` functions to something that doesn't conflict with CSS, or to add partial
support for [the proposed module system](module-system.md) to allow the
functions to be used with a namespace. Both of these solutions would require the
existing function invocations to be deprecated, though, and for all existing
stylesheets that use them to be migrated.

This deprecation would add a substantial amount of time before support for CSS's
math functions could be added, and the eventual removal of the SassScript
functions would probably create substantial migration pain for our users for a
long time.

Supporting both syntaxes does run the risk of escalating users' typos or
misunderstandings of syntax into confusing errors or even busted output.
However, because the CSS syntax is relatively narrow, it's likely that errors
will cause functions to be interpreted as SassScript where unit mismatches or
type errors will quickly be brought to the user's attention.

It's also conceivable that users are using SassScript's `min()` and `max()` in
ways that are now valid CSS. This seems very unlikely, though, since any such
invocation would either be useless or fail at runtime. Those invocations that
don't have type errors will also be compiled to semantically-identical (although
possibly less-compatible) CSS, so this is likely not to be a meaningful concern.

## Syntax

This proposal defines a new production, `MinMaxExpression`. This expression
should be parsed in a SassScript context when an expression is expected and the
input stream starts with an identifier with value `min` or `max` (ignoring case)
followed immediately by `(`.

The grammar for this production is:

<x><pre>
**MinMaxExpression** ::= CssMinMax | FunctionExpression
**CssMinMax**        ::= ('min(' | 'max(') CalcValue (',' CalcValue)* ')'
**CalcValue**        ::= CalcValue (('+' | '-' | '*' | '/') CalcValue)+
&#32;                  | '(' CalcValue ')'
&#32;                  | ('calc(' | 'env(' | 'var(') InterpolatedDeclarationValue ')'
&#32;                  | CssMinMax
&#32;                  | Interpolation
&#32;                  | Number
</pre></x>

If a `MinMaxExpression` is parsed as a `CssMinMax`, it should return an unquoted
interpolated string expression that would be identical to the source text
according to CSS semantics for all possible interpolated strings. If it's parsed
as a `FunctionExpression`, it should be returned as a function expression.
Parsing a `CssMinMax` takes precedence over parsing a `FunctionExpression` in
cases where either would apply.

> Note that in practice *all* `CssMinMax` productions would also be valid
> `FunctionExpression`s. However, any `CssMinMax` that's likely to be used in
> practice would produce a `FunctionExpression` that would fail at runtime.
