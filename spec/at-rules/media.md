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

Media queries are parsed from Sass source using the following syntax. All
identifiers are matched case-insensitively:

<x><pre>
**MediaQueryList** ::= MediaQuery (',' MediaQuery)*
**MediaQuery**     ::= MediaNot
&#32;                | MediaInParens (MediaAnd* | MediaOr*)
&#32;                | MediaType ('and' MediaNot | MediaAnd*)
**MediaType**      ::= [InterpolatedIdentifier] [InterpolatedIdentifier]¹?
**MediaNot**²      ::= 'not' MediaOrInterp
**MediaAnd**²      ::= 'and' MediaOrInterp
**MediaOr**²       ::= 'or' MediaOrInterp
**MediaOrInterp**  ::= Interpolation | MediaInParens
**MediaInParens**  ::= '(' Expression³ ')'
&#32;                | '(' Expression³ [\<mf-comparison>] Expression³ ')'
&#32;                | '(' Expression³ [\<mf-lt>] Expression³ [\<mf-lt>] Expression³ ')'
&#32;                | '(' Expression³ [\<mf-gt>] Expression³ [\<mf-gt>] Expression³ ')'
&#32;                | '(' MediaNot ')'
&#32;                | '(' MediaInParens (MediaAnd\*| MediaOr\*) ')'
</pre></x>

[InterpolatedIdentifier]: ../syntax.md#interpolatedidentifier
[\<mf-comparison>]: https://drafts.csswg.org/mediaqueries-4/#typedef-mf-comparison
[\<mf-lt>]: https://drafts.csswg.org/mediaqueries-4/#typedef-mf-lt
[\<mf-gt>]: https://drafts.csswg.org/mediaqueries-4/#typedef-mf-gt

1. This `InterpolatedIdentifier` may not be the identifier `"and"`.

2. No whitespace is allowed between the identifier and the `MediaOrInterp` in
   these productions.

3. These `Expression`s may not:

   * Contain binary operator expressions with the operators `=`, `>`, `>=`, `<`,
     or `<=`, except within parentheses (including function calls and map
     literals) and square brackets.

   * Begin with the case-insensitive identifier `"not"`.

   * Begin with the character `"("`.

### CSS

Plain CSS media queries are parsed using the following syntax. All identifiers
are matched case-insensitively:

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
