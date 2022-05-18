# `@media`

Although the `@media` rule is a plain CSS rule, Sass has special support for
parsing it and handling at runtime, in order to bring nested `@media` queries
to the top-level for browsers that don't support nesting natively.

## Table of Contents

* [Syntax](#syntax)
  * [Sass](#sass)
  * [CSS](#css)

## Syntax

Sass parses media queries twice. The first time is part of parsing the Sass
stylesheet, at which point the queries may contain SassScript expressions and
interpolation. The second parses the result of evaluating the SassScript as
plain CSS.

### Sass

Media queries are parsed from Sass source using the following syntax:

<x><pre>
**MediaQueryList**       ::= MediaQuery (',' MediaQuery)*
**MediaQuery**           ::= MediaType ('and' MediaFeature)*
&#32;                      | MediaFeatureInParens ('and' MediaFeature)*
**MediaType**            ::= [InterpolatedIdentifier][] [InterpolatedIdentifier][]¹?
**MediaFeature**         ::= Interpolation | MediaFeatureInParens
**MediaFeatureInParens** ::= '(' Expression² ')'
&#32;                      | '(' Expression² ':' Expression ')'
&#32;                      | '(' Expression² [\<mf-comparison>][] Expression² ')'
&#32;                      | '(' Expression² [\<mf-lt>][] Expression² [\<mf-lt>][] Expression² ')'
&#32;                      | '(' Expression² [\<mf-gt>][] Expression² [\<mf-gt>][] Expression² ')'
</pre></x>

[InterpolatedIdentifier]: ../syntax.md#interpolatedidentifier
[\<mf-comparison>]: https://drafts.csswg.org/mediaqueries-4/#typedef-mf-comparison
[\<mf-lt>]: https://drafts.csswg.org/mediaqueries-4/#typedef-mf-lt
[\<mf-gt>]: https://drafts.csswg.org/mediaqueries-4/#typedef-mf-gt

1: This `InterpolatedIdentifier` may not be the identifier `"and"`.

2: These `Expression`s may not contain binary operator expressions with the
operators `=`, `>`, `>=`, `<`, or `<=`, except within parentheses (including
function calls and map literals) and square brackets.

> Note that Sass currently doesn't support parsing full media conditions
> according to the level 4 specification, since no browsers support it yet. See
> [sass/sass#2538][] for details.

[sass/sass#2538]: https://github.com/sass/sass/issues/2538

### CSS

Plain CSS media queries are parsed using the following syntax:

<x><pre>
**CssMediaQueryList** ::= CssMediaQuery (',' CssMediaQuery)*
**CssMediaQuery**     ::= CssMediaType
&#32;                    | (CssMediaType 'and')? CssMediaFeature ('and' CssMediaFeature)*
**CssMediaType**      ::= [\<ident-token>][] [\<ident-token>][]¹?
**CssMediaFeature**   ::= '(' [\<declaration-value>][] ')'
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
[\<declaration-value>]: https://drafts.csswg.org/css-syntax-3/#typedef-declaration-value

1: This `<ident-token>` may not be the identifier `"and"`.
