# Range-Context Media Features: Draft 3.1

*([Issue](https://github.com/sass/sass/issues/1864), [Changelog](media-ranges.changes.md))*

This proposal defines how Sass handles media queries with features written in a
[range context][].

[range context]: https://www.w3.org/TR/mediaqueries-4/#mq-range-context

## Table of Contents

* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Syntax](#syntax)
  * [Sass](#sass)
  * [CSS](#css)

## Summary

> This section is non-normative.

Media features written in a range context will be parsed by Sass, with full
SassScript expressions allowed for the values except in cases where Sass's
operators would be ambiguous with range operators. Range-context media features
will be merged as all media features have been up to this point, by adding each
feature to the media query's `"and"`-separated list.

### Design Decisions

While it would be possible to merge features more intelligently—for example,
`(width > 200px) and (width < 600px)` could be merged into
`(200px > width > 600px)`—doing so in general would add a great deal of
complexity to media merging, for very limited benefits in terms of output size
and readability.

The values of media features with the "range" type are heterogeneous, including
a [`<ratio>`][] type value type that Sass has no existing knowledge of. If Sass
were to support intelligent merging of these features, it would need to keep
abreast of any new value types supported by "range"-type media features. This
would violate Sass's general design principle of knowing as little about CSS as
possible.

[`<ratio>`]: https://www.w3.org/TR/mediaqueries-4/#typedef-ratio

## Syntax

Sass parses media queries twice. The first time is part of parsing the Sass
stylesheet, at which point the queries may contain SassScript expressions and
interpolation. The second parses the result of evaluating the SassScript as
plain CSS.

### Sass

This proposal defines a new syntax for media queries in Sass stylesheets. It is
intended to replace the existing syntax.

> Other than support for the [range context][] syntax, this syntax is designed
> to represent the current behavior of all Sass implementations.

<x><pre>
**MediaQueryList** ::= MediaQuery (',' MediaQuery)*
**MediaQuery**     ::= MediaType | (MediaType 'and')? MediaFeature ('and' MediaFeature)*
**MediaType**      ::= InterpolatedIdentifier InterpolatedIdentifier¹?
**MediaFeature**   ::= Interpolation
&#32;                 | '(' Expression² ')'
&#32;                 | '(' Expression² ':' Expression ')'
&#32;                 | '(' Expression² <mf-comparison> Expression² ')'
&#32;                 | '(' Expression² <mf-lt> Expression² <mf-lt> Expression² ')'
&#32;                 | '(' Expression² <mf-gt> Expression² <mf-gt> Expression² ')'
</pre></x>


1: This `InterpolatedIdentifier` may not be the identifier `"and"`.

2: These `Expression`s may not contain binary operator expressions with the
operators `=`, `>`, `>=`, `<`, or `<=`, except within parentheses (including
function calls and map literals) and square brackets.

The `<mf-comparison>`, `<mf-lt>`, and `<mf-gt>` productions are defined in
[Media Queries Level 4][].

[Media Queries Level 4]: https://drafts.csswg.org/mediaqueries-4/#mq-syntax

> Note that Sass currently doesn't support parsing full media conditions
> according to the level 4 specification, since no browsers support it yet. See
> [sass/sass#2538][] for details.

[sass/sass#2538]: https://github.com/sass/sass/issues/2538

### CSS

Plain CSS media queries are parsed using the following syntax:

<!-- markdown-link-check-disable -->
<x><pre>
**CssMediaQueryList** ::= CssMediaQuery (',' CssMediaQuery)*
**CssMediaQuery**     ::= CssMediaType
&#32;                    | (CssMediaType 'and')? CssMediaFeature ('and' CssMediaFeature)*
**CssMediaType**      ::= <ident-token> <ident-token>¹?
**CssMediaFeature**   ::= '(' <declaration-value> ')'
</pre></x>
<!-- markdown-link-check-enable -->

1: This `Identifier` may not be the identifier `"and"`.

The `<ident-token>` production matches the [railroad diagram][ident-token]
listed in CSS Syntax Level 3. The `<declaration-value>` production uses
[the definition][declaration-value] from CSS Syntax Level 3,
[consuming tokens][] only as needed until the production terminates.

[ident-token]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
[declaration-value]: https://drafts.csswg.org/css-syntax-3/#typedef-declaration-value
[consuming tokens]: https://drafts.csswg.org/css-syntax-3/#consume-a-token

> This is the existing syntax Sass uses to reparse plain CSS media queries.
> Since they're already parsed using `<declaration-value>`, no change in
> behavior is necessary to support range-form queries.
