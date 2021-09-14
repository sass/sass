# `clamp()`: Draft 1

*([Issue](https://github.com/sass/sass/issues/2860))*

This proposal adds support for `clamp()` as a CSS function with special parsing
support akin to `min()`, `max()`, and `calc()`.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Definitions](#definitions)
  * [Special Number String](#special-number-string)
* [Syntax](#syntax)
  * [`SpecialFunctionName`](#specialfunctionname)
  * [`CalcValue`](#calcvalue)
* [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

CSS Values and Units 4 has introduced the `clamp()` function as a way of
representing mathematical expressions that are evaluated by the browser at
render-time. Syntactically, it's closely-related to the existing `calc()`
function, which Sass has long supported as a special syntactic form that allows
almost any text within its parentheses. This proposal extends that syntax to
cover `clamp()` as well.

According to [caniuse], browser support for `clamp()` first landed in Chrome in
December 2019, and at time of writing is supported in Edge, Firefox, Safari, and
Opera, covering 86.8% of users. Despite its wide availability, its use in Sass
doesn't seem too widespread yet judging by the lack of support requests and only
two 👍s on the issue in the issue tracker.

[caniuse]: https://caniuse.com/mdn-css_types_clamp

## Summary

This proposal makes `clamp()` essentially a synonym of `calc()`, so that its
contents are parsed in the same liberal manner with interpolation as the only
valid use of Sass within them.

### Design Decisions

This proposal implies that invocations like `clamp($foo)` will not evaluate Sass
variables. This does represent a potential backwards-incompatibility for users
who have started using `clamp()` with Sass's default function syntax, which
interprets all arguments as SassScript expressions. However, outside of obvious
cases like a single variable being used as an argument, it's difficult to
disambiguate Sass expressions and plain-CSS math expressions, so we'd like to
avoid needing to do so if at all possible.

It's worth noting that there is prior art for this disambiguation. When adding
support for [plain-CSS `min()` and `max()`] functions, we decided to
disambiguate the plain CSS versions from the Sass-syntax versions by first
parsing as the former and falling back to the latter if that parse failed. This
proposal intentionally avoids that approach for several reasons:

* The disambiguation was necessary for `min()` and `max()` because those
  functions had existed as global Sass functions for many years. `clamp()`, on
  the other hand, has only been usefully usable in CSS for less than a year, and
  it's not a built-in Sass functions so there's much less reason to pass Sass
  variables to it directly. In other words, the potential impact of a breaking
  change is low.

* Even with `min()` and `max()`, we're concerned that the double-parsing will be
  confusing to users who expect the same outer syntax to imply the same inner
  parsing. We'd like to avoid extending that confusion any more broadly than
  necessary.

* Attempting to parse a production one way and falling back on a different parse
  method is expensive in the parser. That said, Dart Sass's parser is generally
  not a bottleneck so this is a relatively smaller concern.

[plain-CSS `min()` and `max()`]: ../accepted/min-max.md

## Definitions

### Special Number String

`clamp(` is added to the list of possible prefixes for a [special number
string].

[special number string]: ../spec/functions.md#special-number

## Syntax

### `SpecialFunctionName`

The [`SpecialFunctionName`] production will be changed to the following:

[`SpecialFunctionName`]: ../spec/syntax.md#specialfunctionexpression

<x><pre>
**SpecialFunctionName**¹ ::= VendorPrefix? ('calc(' | 'element(' | 'expression(')
                           | 'clamp('
</pre></x>

1: `SpecialFunctionName` is matched case-insensitively and may not contain
   whitespace.

> Note that vendor prefixes are *not* supported for `clamp()` because no browser
> has ever shipped support for it guarded by a prefix.

### `CalcValue`

The [`CalcValue`] production will be changed to the following:

[`CalcValue`]: ../spec/syntax.md#minmaxexpression

<x><pre>
**CalcValue**         ::= CalcValue (('+' | '-' | '*' | '/') CalcValue)+
&#32;                   | '(' CalcValue ')'
&#32;                   | CalcFunctionName InterpolatedDeclarationValue ')'
&#32;                   | CssMinMax
&#32;                   | Interpolation
&#32;                   | Number
**CalcFunctionName**¹ ::= 'calc(' | 'env(' | 'var(' | 'clamp('
</pre></x>

1: `CalcFunctionName` is matched case-insensitively.

## Deprecation Process

This proposal changes the way `clamp()` function calls that are passed
SassScript expressions are parsed, which is backwards-incompatible. Despite
this, it does not call for a deprecation process. Because `clamp()` is so young
and the use-cases for SassScript arguments so narrow, the impact of the
backwards-incompatibility is likely to be relatively minor. In addition,
delaying the release of full syntactic support for the duration of a deprecation
period is likely to cause substantial user pain as more users attempt to use
`clamp()` going forward.

As such, I propose we treat this change as though it were a potentially-breaking
bug fix rather than a full-fledged breaking change.
