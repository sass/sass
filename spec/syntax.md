# Syntax

## Table of Contents

* [Definitions](#definitions)
  * [Source File](#source-file)
* [Grammar](#grammar)
  * [`InterpolatedIdentifier`](#interpolatedidentifier)
  * [`Name`](#name)
  * [`MinMaxExpression`](#minmaxexpression)
* [Procedures](#procedures)
  * [Parsing Text as CSS](#parsing-text-as-css)
  * [Consuming an Identifier](#consuming-an-identifier)
  * [Consuming an Interpolated Identifier](#consuming-an-interpolated-identifier)
  * [Consuming a Name](#consuming-a-name)
  * [Consuming an Escaped Code Point](#consuming-an-escaped-code-point)
  * [Consuming `min()` or `max()`](#consuming-min-or-max)

## Definitions

### Source File

A *source file* is a Sass abstract syntax tree along with an absolute URL, known
as that file's *canonical URL*. A given canonical URL cannot be associated with
more than one source file.

## Grammar

### `InterpolatedIdentifier`

<x><pre>
**InterpolatedIdentifier** ::= ([\<ident-token>][] | '-'? Interpolation) ([Name][] | Interpolation)*
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
[Name]: #name

No whitespace is allowed between components of an `InterpolatedIdentifier`.

### `Name`

<x><pre>
**Name** ::= ([name code point][] | [escape][])+
</pre></x>

[name-start code point]: https://drafts.csswg.org/css-syntax-3/#name-start-code-point
[escape]: https://drafts.csswg.org/css-syntax-3/#escape-diagram

### `MinMaxExpression`

<x><pre>
**MinMaxExpression** ::= CssMinMax | FunctionExpression
**CssMinMax**        ::= ('min(' | 'max(')¹ CalcValue (',' CalcValue)* ')'
**CalcValue**        ::= CalcValue (('+' | '-' | '*' | '/') CalcValue)+
&#32;                  | '(' CalcValue ')'
&#32;                  | ('calc(' | 'env(' | 'var(')¹ InterpolatedDeclarationValue ')'
&#32;                  | CssMinMax
&#32;                  | Interpolation
&#32;                  | Number
</pre></x>

1: The strings `min(`, `max(`, `calc(`, `env(`, and `var(` are matched
   case-insensitively.

## Procedures

### Parsing Text as CSS

This algorithm takes a string, `text`, and returns a Sass abstract syntax tree.

> This algorithm is designed with two goals in mind:
>
> 1. CSS imported from Sass should be as compatible with standard CSS as
>    possible. In some cases we err even more towards CSS compatibility than
>    SCSS does, because the CSS being imported is likely not written by someone
>    who knows to avoid things that Sass interprets specially (such as certain
>    `@import` URLs).
>
> 2. We should provide clear and eager feedback to users who accidentally try to
>    use Sass features in CSS imports. We don't allow these features, and we
>    want users to know that through error messages rather than digging through
>    generated CSS only to find that Sass features were passed through
>    unmodified. This is a particular concern because LibSass has historically
>    allowed the use of Sass features in CSS imports.

The algorithm for parsing text as CSS works like parsing text as SCSS, with some
modifications. The following productions should produce errors:

* Any at-rules that are defined in Sass and not in plain CSS. At the time of
  writing, this means:

  * `@at-root`
  * `@content`
  * `@debug`
  * `@each`
  * `@error`
  * `@extend`
  * `@for`
  * `@function`
  * `@if`
  * `@include`
  * `@mixin`
  * `@return`
  * `@warn`
  * `@while`

* An `@import` that contains interpolation in the `url()`, the media query, or
  the supports query.

* An `@import` that appears within a style rule or at-rule.

* An `@import` with more than one argument.

* A declaration followed by an open curly brace (that is, a nested declaration).

* A style rule appearing within another style rule.

* The parent selector `&`, either in a selector or a declaration value.

* Placeholder selectors.

* All built-in functions, *excluding* the following:

  * `rgb()`
  * `rgba()`
  * `hsl()`
  * `hsla()`
  * `grayscale()`
  * `invert()`
  * `alpha()`
  * `opacity()`

  > Note that user-defined functions are *not* forbidden, whether they're
  > defined using `@function` or through a host language API.

* Any function called with keyword arguments or variable-length arguments.

* Interpolation anywhere its contents would be evaluated. At the time of
  writing, this means:

  * At-rule values (including `@media` queries)
  * Declaration names
  * Declaration values
  * Style rule selectors

* All SassScript operations *except for*:

  * `/`
  * `not`
  * `or`
  * `and`

  > Note that although unary `-` is forbidden, the `-` that appears at the
  > beginning of a number literal is part of that literal and thus allowed.

* Parentheses in declaration values that aren't part of a CSS production.

* Map literals.

* The empty list literal `(,)`.

* Uses or declarations of Sass variables.

* `//`-style ("silent") comments.

In addition, some productions should be parsed differently than they would be in
SCSS:

* All functions that don't produce errors should be parsed as plain CSS
  functions, regardless of whether a Sass function with that name is defined.

* All `@import`s that don't produce errors should be parsed as static CSS
  imports.

* The tokens `not`, `or`, `and`, and `null` should be parsed as unquoted
  strings.

  > The `/` operation should be parsed as normal. Because variables,
  > parentheses, functions that return numbers, and all other arithmetic
  > expressions are disallowed, it will always compile to slash-separated values
  > rather than performing division.

### Consuming an Identifier

This algorithm consumes input from a stream of [code points][] and returns a
string.

[code points]: https://infra.spec.whatwg.org/#code-point

This production has the same grammar as [`<ident-token>`][].

[`<ident-token>`]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram

* Let `string` be an empty string.

* If the stream starts with `--`, consume it and append it to `string`.

* Otherwise:

  * If the stream starts with `-`, consume it and append it to `string`.

  * If the stream starts with `\`, [consume an escaped code point][] with the
    `start` flag set and append it to `string`.

    [consume an escaped code point]: #consuming-an-escaped-code-point

  * Otherwise, if the stream starts with a [name-start code point][], consume it
    and append it to `string`.

    [name-start code point]: https://drafts.csswg.org/css-syntax-3/#name-start-code-point

  * Otherwise, throw an error.

* [Consume a name](#consuming-a-name) and append it to `string`.

* Return `string`.

### Consuming an Interpolated Identifier

This algorithm consumes input from a stream of [code points][] and returns a
sequence of strings and/or expressions. It follows the grammar for an
[`InterpolatedIdentifier`][].

[code points]: https://infra.spec.whatwg.org/#code-point
[`InterpolatedIdentifier`]: #interpolatedidentifier

* Let `components` be an empty list of strings and/or expressions.

* If the input starts with `-#{`, consume a single code point and add `"-"` to
  `components`.

* If the input starts with `#{`, consume an interpolation and add
  its expression to `components`.

* Otherwise, [consume an identifier](#consuming-an-identifier) and add its string
  to `components`.

* While the input starts with `#{`, a [name code point][], or `\`:

  [name code point]: https://drafts.csswg.org/css-syntax-3/#name-code-point

  * If the input starts with `#{`, consume an interpolation and add
    its expression to `components`.

  * Otherwise, [consume a name](#consuming-a-name) and add its string to
    `components`.

* Return `components`.

### Consuming a Name

This algorithm consumes input from a stream of [code points][] and returns a
string. It follows the grammar for a [`Name`][].

The grammar for this production is:

<x><pre>
**Name** ::= ([name code point][] | [escape][])+
</pre></x>

* Let `string` be an empty string.

* While the input starts with a [name code point][] or `\`:

  * If the input starts with a [name code point][], consume it and append it to
    `string`.

  * Otherwise, [consume an escaped code point][] and append it to `string`.

* Return `string`.

### Consuming an Escaped Code Point

This algorithm consumes input from a stream of [code points][]. It takes an
optional boolean flag, `start`, which indicates whether it's at the beginning of
an identifier and defaults to false. It returns a string.

This production has the same grammar as [`escape`][escape] in CSS Syntax Level 3.

* If the stream doesn't [start with a valid escape][], throw an error.

  [start with a valid escape]: https://drafts.csswg.org/css-syntax-3/#starts-with-a-valid-escape

* Let `codepoint` be the result of [consuming an escaped code point][].

  [consuming an escaped code point]: https://drafts.csswg.org/css-syntax-3/#consume-escaped-code-point

* Let `character` be the string containing only `codepoint`.

* If `codepoint` is a [name-start code point][], return `character`.

* Otherwise, if `codepoint` is a [name code point][] and the `start` flag is
  not set, return `character`.

* Otherwise, if `codepoint` is a [non-printable code point][], U+0009 CHARACTER
  TABULATION, U+000A LINE FEED, U+000D CARRIAGE RETURN, or U+000C FORM FEED;
  *or* if `codepoint` is a [digit][] and the `start` flag is set:

  [non-printable code point]: https://drafts.csswg.org/css-syntax-3/#non-printable-code-point
  [digit]: https://drafts.csswg.org/css-syntax-3/#digit

  * Let `code` be the lowercase hexadecimal representation of `codepoint`,
    with no leading `0`s.

  * Return `"\"` + `code` + `" "`.

  > Tab characters are parsed as explicit escapes in order to support a browser
  > hack that targets IE 10 and earlier, wherein ending a declaration value with
  > `\9` would cause IE to interpret it as valid but other browsers to ignore
  > it.

* Otherwise, return `"\"` + `character`.

### Consuming `min()` or `max()`

This algorithm consumes input from a stream of [code points][] and returns a
SassScript expression.

* Let `expression` be the result of consuming a [`MinMaxExpression`][]. If the
  expression is ambiguous between `CssMinMax` and `FunctionExpression`,
  `CssMinMax` should take precedence.

  [`MinMaxExpression`]: #minmaxexpression

* If `expression` is a `FunctionExpression`, return it as-is.

* Otherwise, if `expression` is a `CssMinMax`, return an unquoted interpolated
  string expression that would be identical to the source text according to CSS
  semantics for all possible interpolated strings.
