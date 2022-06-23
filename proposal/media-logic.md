# Media Logic: Draft 1

*([Issue](https://github.com/sass/sass/issues/2538))*

This proposal adds support for the full [Media Queries Level 4] syntax for media
conditions, including arbitrary boolean logic using `and`, `or`, and `not`.

[Media Queries Level 4]: https://www.w3.org/TR/mediaqueries-4/#media-conditions

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Syntax](#syntax)
  * [`MediaQuery`](#mediaquery)
  * [`CssMediaQuery`](#cssmediaquery)
* [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

For historical reasons, Sass fully parses media queries and allows SassScript to
be embedded directly in them, as in `@media ($query: $value)`, in contrast to
most other at-rules in which SassScript can only be injected using
interpolation. This means that as CSS adds new media query syntax, Sass is
obligated to update its specification to accommodate it.

[Media Queries Level 4] adds support for arbitrary boolean logic in media
queries, such as `@media ((width >= 100px) and (width <= 800px)) or (grid)`.
Sass must therefore update its syntax accordingly.

## Summary

> This section is non-normative.

The proposal is relatively straightforward: it adds the new syntax to Sass's
grammar. It is worth noting, though, that this will require a few breaking
changes. These are unlikely to affect many real-world stylesheets, but they're
worth highlighting nevertheless.

The new syntax allows any [`<media-condition>`] to appear inside a
[`<media-in-parens>`]. This means that queries beginning with `(not ` or `((`
must be parsed as nested media queries, rather than SassScript expressions as
they have historically been parsed. We'll issue a short deprecation period for
the SassScript expressions in question, recommending users migrate them to
interpolation instead, then drop support and begin parsing them as media queries
for CSS compatibility.

[`<media-condition>`]: https://drafts.csswg.org/mediaqueries-4/#typedef-media-condition
[`<media-in-parens>`]: https://drafts.csswg.org/mediaqueries-4/#typedef-media-in-parens

## Syntax

### `MediaQuery`

Replace the definition of the [`MediaQuery`] production with the following (with
all identifiers matched case-insensitively):

[`MediaQuery`]: ../spec/at-rules/media.md#sass

<x><pre>
**MediaQuery**     ::= MediaNot
&#32;                | MediaOrInterp (MediaAnd* | MediaOr*)
&#32;                | MediaType ('and' MediaNot | MediaAnd*)
**MediaType**      ::= [InterpolatedIdentifier] [InterpolatedIdentifier]¹?
**MediaNot**       ::= 'not' MediaOrInterp
**MediaAnd**       ::= 'and' MediaOrInterp
**MediaOr**        ::= 'or' MediaOrInterp
**MediaOrInterp**  ::= Interpolation | MediaInParens
**MediaInParens**  ::= '(' Expression² ')'
&#32;                | '(' Expression² [\<mf-comparison>] Expression² ')'
&#32;                | '(' Expression² [\<mf-lt>] Expression² [\<mf-lt>] Expression² ')'
&#32;                | '(' Expression² [\<mf-gt>] Expression² [\<mf-gt>] Expression² ')'
&#32;                | '(' MediaNot ')'
&#32;                | '(' MediaInParens (MediaAnd* | MediaOr*) ')'
</pre></x>

[InterpolatedIdentifier]: ../syntax.md#interpolatedidentifier
[\<mf-comparison>]: https://drafts.csswg.org/mediaqueries-4/#typedef-mf-comparison
[\<mf-lt>]: https://drafts.csswg.org/mediaqueries-4/#typedef-mf-lt
[\<mf-gt>]: https://drafts.csswg.org/mediaqueries-4/#typedef-mf-gt

1. This `InterpolatedIdentifier` may not be the identifier `"and"`.

2. These `Expression`s may not:

   * Contain binary operator expressions with the operators `=`, `>`, `>=`, `<`,
     or `<=`, except within parentheses (including function calls and map
     literals) and square brackets.

   * Begin with the case-insensitive identifier `"not"`.

   * Begin with the character `"("`.

### `CssMediaQuery`

Replace the definition of the [`CssMediaQuery`] production with the following (with
all identifiers matched case-insensitively):

[`MediaQuery`]: ../spec/at-rules/media.md#css

<x><pre>
**CssMediaQuery**     ::= CssMediaCondition
&#32;                   | CssMediaType ('and' CssMediaNot | CssMediaAnd*)
**CssMediaType**      ::= [\<ident-token>] [\<ident-token>]¹?
**CssMediaCondition** ::= CssMediaNot | CssMediaInParens (CssMediaAnd* | CssMediaOr*)
**CssMediaNot**       ::= 'not' CssMediaInParens
**CssMediaAnd**       ::= 'and' CssMediaInParens
**CssMediaOr**        ::= 'or' CssMediaInParens
**CssMediaInParens**  ::= '(' [\<declaration-value>] ')'
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
[\<declaration-value>]: https://drafts.csswg.org/css-syntax-3/#typedef-declaration-value

1. This `<ident-token>` may not be the identifier `"and"`.

## Deprecation Process

Before this specification is applied in full force, it will be applied with the
following modifications:

* [`MediaInParens`](#mediaquery) will not allow the productions `'(' MediaNot
  ')'` or `'(' MediaInParens (MediaAnd* | MediaOr*) ')'`.

* If the first `Expression` in a `MediaInParens` production begins with the
  case-insensitive identifier `"not"` or the character `"("`, emit a deprecation
  warning.
