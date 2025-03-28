# Syntax

## Table of Contents

* [Definitions](#definitions)
  * [Source File](#source-file)
  * [Vendor Prefix](#vendor-prefix)
* [Syntax](#syntax-1)
  * [`ArgumentList`](#argumentlist)
  * [`InterpolatedIdentifier`](#interpolatedidentifier)
  * [`InterpolatedUrl`](#interpolatedurl)
  * [`Name`](#name)
  * [`ParameterList`](#parameterlist)
  * [`ProductExpression`](#productexpression)
  * [`PseudoSelector`](#pseudoselector)
  * [`SingleExpression`](#singleexpression)
  * [`SpecialFunctionExpression`](#specialfunctionexpression)
* [Procedures](#procedures)
  * [Parsing Text](#parsing-text)
  * [Parsing Text as CSS](#parsing-text-as-css)
  * [Consuming an Identifier](#consuming-an-identifier)
  * [Consuming an Interpolated Identifier](#consuming-an-interpolated-identifier)
  * [Consuming a Name](#consuming-a-name)
  * [Consuming an Escaped Code Point](#consuming-an-escaped-code-point)
  * [Consuming a special function](#consuming-a-special-function)
* [Semantics](#semantics)
  * [`Percent`](#percent)

## Definitions

### Source File

A *source file* is a Sass abstract syntax tree along with an absolute URL, known
as that file's *canonical URL*; and an [importer]. A given canonical URL cannot
be associated with more than one source file.

[importer]: modules.md#importer

### Vendor Prefix

Some identifiers have a *vendor prefix*, which is an initial substring beginning
with U+002D HYPHEN-MINUS code point followed by one or more non-U+002D code
points followed by another U+002D. An identifier only has a vendor prefix if the
final U+002D is followed by additional text. This additional text is referred to
as the *unprefixed identifier*.

## Syntax

Unless otherwise specified, if a grammar production's value is parsed as a
single other named Sass production, then the AST does not contain a
representation of the intermediate production.

> For example, suppose we have the productions:
>
> <x><pre>
> **Foo** ::= Bar '*'?
> **Bar** ::= \<ident-token>
> </pre></x>
>
> Parsing `"ident*"` creates an AST that contains a `Foo` that contains a `Bar`.
> But parsing `"ident"` alone creates an AST that only contains a `Bar`—the
> `Foo` wrapper is removed. (The `Bar` wrapper is *not* removed because
> `<ident-token>` is a CSS production, not a Sass production.)

### `ArgumentList`

<x><pre>
**ArgumentList**  ::= '(' Expression (',' Expression)\*
&#32;                 (',' NamedArgument)\* (',' RestArgument){0,2} ','? ')'
&#32;               | '(' NamedArgument (',' NamedArgument)\*
&#32;                 (',' RestArgument){0,2} ','? ')'
&#32;               | '(' RestArgument (',' RestArgument)? ','? ')'
&#32;               | '(' ')'
**NamedArgument** ::= [PlainVariable] ':' Expression
**RestArgument**  ::= Expression '...'
</pre></x>

### `InterpolatedIdentifier`

<x><pre>
**InterpolatedIdentifier** ::= ([\<ident-token>] | '-'? Interpolation) ([Name] | Interpolation)*
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
[Name]: #name

No whitespace is allowed between components of an `InterpolatedIdentifier`.

### `InterpolatedUrl`

<x><pre>
**InterpolatedUrl**         ::= 'url(' (QuotedString | InterpolatedUnquotedUrlContents) ')'
**InterpolatedUnquotedUrlContents** ::= ([unescaped url contents] | [escape] | Interpolation)*
</pre></x>

[unescaped url contents]: https://www.w3.org/TR/css-syntax-3/#url-token-diagram

No whitespace is allowed between components of an `InterpolatedUnquotedUrlContents`.

### `Name`

<x><pre>
**Name** ::= ([identifier code point] | [escape])+
</pre></x>

[identifier code point]: https://drafts.csswg.org/css-syntax-3/#identifier-code-point
[escape]: https://drafts.csswg.org/css-syntax-3/#escape-diagram

### `ParameterList`

<x><pre>
**ParameterList** ::= '(' Parameter (',' Parameter)\* ','? ')'
&#32;               | '(' Parameter (',' Parameter)\* (',' RestParameter)? ','? ')'
&#32;               | '(' RestParameter ','? ')'
&#32;               | '(' ')'
**Parameter**     ::= [PlainVariable] (':' Expression)?
**RestParameter** ::= [PlainVariable] '...'
</pre></x>

[PlainVariable]: variables.md#syntax

### `ProductExpression`

<x><pre>
**ProductExpression** ::= (ProductExpression ('*' | '%'))? SingleExpression
</pre></x>

### `PseudoSelector`

<x><pre>
**PseudoSelector**          ::= NormalPseudoSelector
&#32;                         | SelectorPseudo
&#32;                         | NthSelectorPseudo
**NormalPseudoSelector**    ::= ':' ':'? VendorPrefix? [\<ident-token>]
&#32;                           ('(' [\<declaration-value>] ')')?
**SelectorPseudo**          ::= SelectorPseudoName '(' Selector ')'
**NthSelectorPseudo**       ::= NthSelectorPseudoName '(' [\<an+b>] 'of'¹ Selector ')'
**SelectorPseudoPrefix**    ::= ':' SelectorPseudoClassName | '::slotted'
**SelectorPseudoClassName** ::= 'not' | 'is' | 'matches' | 'where' | 'any'
&#32;                         | 'current' | 'has' | 'host' | 'host-context'
**NthSelectorPseudoName**   ::= ':' ('nth-child' | 'nth-last-child')
</pre></x>

[\<declaration-value>]: https://www.w3.org/TR/css-syntax-3/#typedef-declaration-value
[\<an+b>]: https://www.w3.org/TR/css-syntax-3/#the-anb-type

1: The string `of` is matched case-insensitively. In addition, it must be parsed
   as an identifier.

   > In other words, it must have whitespace separating it from other
   > identifiers, so `:nth-child(2nof a)` and `:nth-child(2n ofa)` are both
   > invalid. However, `:nth-child(2of.foo)` is valid.

If a `PseudoSelector` begins with`SelectorPseudoName` or `NthSelectorPseudoName`
followed by a parenthesis, it must be parsed as a `SelectorPseudo` or an
`NthSelectorPseudo` respectively, not as a `NormalPseudoSelector`.

No whitespace is allowed anywhere in a `PseudoSelector` except within
parentheses.

### `SingleExpression`

<x><pre>
**SingleExpression** ::= '(' [ContainedListExpression] ')'
&#32;                  | Important
&#32;                  | Boolean
&#32;                  | [BracketedListExpression]
&#32;                  | ColorLiteral
&#32;                  | FunctionExpression
&#32;                  | IDName¹
&#32;                  | Null
&#32;                  | Number
&#32;                  | ParentExpression
&#32;                  | String²
&#32;                  | UnaryExpression
&#32;                  | UnicodeRange
&#32;                  | [Variable]
&#32;                  | Percent³
**Percent**          ::= '%'
</pre></x>

[BracketedListExpression]: types/list.md#syntax
[ContainedListExpression]: types/list.md#syntax
[Variable]: variables.md#syntax

1: If this is ambiguous with `ColorLiteral`, it should be parsed as
   `ColorLiteral` preferentially.

2: If this is ambiguous with any other production, parse the other production
   preferentially.

3: If this is ambiguous with part of `ProductExpression`, parse
   `ProductExpression` preferentially. If this is followed by a [`Whitespace`]
   that contains a [`LineBreak`], do not parse that `Whitespace` as part of an
   [`IndentSame`] or [`IndentMore`] production.

   > This effectively means that the unquoted string `%` is allowed everywhere
   > *except* in a middle element of a space-separated list, since that would be
   > ambiguous with a modulo operation. The whitespace clause ensures that a `%`
   > at the end of a line in the indented syntax always looks at the next token,
   > for backwards-compatibility with parsing it as an operator and so that
   > whether the statement ends on that line or not doesn't depend on the first
   > token of the next line.

[`Whitespace`]: statement.md#whitespace
[`LineBreak`]: statement.md#whitespace
[`IndentSame`]: statement.md#indentation
[`IndentMore`]: statement.md#indentation

### `SpecialFunctionExpression`

> These functions are "special" in the sense that their arguments don't use the
> normal CSS expression-level syntax, and so have to be parsed more broadly than
> a normal SassScript expression.

<x><pre>
**SpecialFunctionExpression** ::= SpecialFunctionName InterpolatedDeclarationValue ')'
**SpecialFunctionName**¹      ::= VendorPrefix? ('element(' | 'expression(')
&#32;                           | VendorPrefix 'calc('
&#32;                           | 'type('
**VendorPrefix**¹             ::= '-' ([identifier-start code point] | [digit]) '-'
</pre></x>

> No browser has yet supported `type()` with a vendor prefix, nor are they
> likely to do so in the future given that vendor prefixes are largely unpopular
> now.

[digit]: https://drafts.csswg.org/css-syntax-3/#digit

1: Both `SpecialFunctionName` and `VendorPrefix` are matched case-insensitively,
   and neither may contain whitespace.

## Procedures

### Parsing Text

This algorithm takes a string `text` and a syntax `syntax` ("indented", "css",
or "scss"), and returns a Sass abstract syntax tree.

* If `syntax` is "indented", return the result of parsing `text` as the indented
  syntax.

* If `syntax` is "css", return the result of [parsing `text` as
  CSS](#parsing-text-as-css).

* If `syntax` is "scss", return the result of parsing `text` as SCSS.

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
  * `@forward`
  * `@function`
  * `@if`
  * `@include`
  * `@mixin`
  * `@return`
  * `@use`
  * `@warn`
  * `@while`

* An `@import` that contains interpolation in the `url()` or any of its
  `ImportModifier`s.

* An `@import` that appears within a style rule or at-rule.

* An `@import` with more than one argument.

* A declaration followed by an open curly brace (that is, a nested declaration).

* The parent selector `&` in a declaration value.

* A style rule whose selector contains a trailing combinator.

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

  * `not`
  * `or`
  * `and`

  > Note that although unary `-` is forbidden, the `-` that appears at the
  > beginning of a number literal is part of that literal and thus allowed.

* Parentheses in declaration values that aren't part of a CSS production.

* Map literals.

* The empty list literal `(,)`.

* Uses or declarations of Sass variables.

* `//`-style ("silent") comments outside of an expression context.

In addition, some productions should be parsed differently than they would be in
SCSS:

* All functions that don't produce errors should be parsed as plain CSS
  functions, regardless of whether a Sass function with that name is defined.

* All `@import`s that don't produce errors should be parsed as static CSS
  imports.

* The tokens `not`, `or`, `and`, and `null` should be parsed as unquoted
  strings.

* In an expression context, `//` is not parsed as a silent comment. Instead, two
  adjacent `/`s in a [`SlashListExpression`] may have no whitespace between
  them, so `//` is parsed as two slash separators in a slash-separated list.

  [`SlashListExpression`]: types/list.md#slashlistexpression

* A `ParentSelector` may appear anywhere in a `CompoundSelector`, rather than
  just as the first `SimpleSelector`.

* A `ParentSelector` may not have a `suffix`.

### Consuming an Identifier

This algorithm consumes input from a stream of [code points] and returns a
string.

[code points]: https://infra.spec.whatwg.org/#code-point

This production has the same grammar as [`<ident-token>`].

[`<ident-token>`]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram

* Let `string` be an empty string.

* If the stream starts with `--`, consume it and append it to `string`.

* Otherwise:

  * If the stream starts with `-`, consume it and append it to `string`.

  * If the stream starts with `\`, [consume an escaped code point] with the
    `start` flag set and append it to `string`.

  * Otherwise, if the stream starts with an [identifier-start code point],
    consume it and append it to `string`.

  * Otherwise, throw an error.

  [consume an escaped code point]: #consuming-an-escaped-code-point
  [identifier-start code point]: https://drafts.csswg.org/css-syntax-3/#identifier-start-code-point

* [Consume a name](#consuming-a-name) and append it to `string`.

* Return `string`.

### Consuming an Interpolated Identifier

This algorithm consumes input from a stream of [code points] and returns a
sequence of strings and/or expressions. It follows the grammar for an
[`InterpolatedIdentifier`].

[`InterpolatedIdentifier`]: #interpolatedidentifier

* Let `components` be an empty list of strings and/or expressions.

* If the input starts with `-#{`, consume a single code point and add `"-"` to
  `components`.

* If the input starts with `#{`, consume an interpolation and add
  its expression to `components`.

* Otherwise, [consume an identifier](#consuming-an-identifier) and add its string
  to `components`.

* While the input starts with `#{`, a [identifier code point], or `\`:

  * If the input starts with `#{`, consume an interpolation and add
    its expression to `components`.

  * Otherwise, [consume a name](#consuming-a-name) and add its string to
    `components`.

* Return `components`.

### Consuming a Name

This algorithm consumes input from a stream of [code points] and returns a
string. The grammar for this production is:

<x><pre>
**Name** ::= ([identifier code point] | [escape])+
</pre></x>

* Let `string` be an empty string.

* While the input starts with a [identifier code point] or `\`:

  * If the input starts with a [identifier code point], consume it and append
    it to `string`.

  * Otherwise, [consume an escaped code point] and append it to `string`.

* Return `string`.

### Consuming an Escaped Code Point

This algorithm consumes input from a stream of [code points]. It takes an
optional boolean flag, `start`, which indicates whether it's at the beginning of
an identifier and defaults to false. It returns a string.

This production has the same grammar as [`escape`][escape] in CSS Syntax Level 3.

* If the stream doesn't [start with a valid escape], throw an error.

  [start with a valid escape]: https://drafts.csswg.org/css-syntax-3/#starts-with-a-valid-escape

* Let `codepoint` be the result of [consuming an escaped code point].

  [consuming an escaped code point]: https://drafts.csswg.org/css-syntax-3/#consume-escaped-code-point

* Let `character` be the string containing only `codepoint`.

* If `codepoint` is a [identifier-start code point], return `character`.

* Otherwise, if `codepoint` is an [identifier code point] and the `start` flag
  is not set, return `character`.

* Otherwise, if `codepoint` is a [non-printable code point], U+0009 CHARACTER
  TABULATION, U+000A LINE FEED, U+000D CARRIAGE RETURN, or U+000C FORM FEED;
  *or* if `codepoint` is a [digit] and the `start` flag is set:

  [non-printable code point]: https://drafts.csswg.org/css-syntax-3/#non-printable-code-point

  * Let `code` be the lowercase hexadecimal representation of `codepoint`,
    with no leading `0`s.

  * Return `"\"` + `code` + `" "`.

  > Tab characters are parsed as explicit escapes in order to support a browser
  > hack that targets IE 10 and earlier, wherein ending a declaration value with
  > `\9` would cause IE to interpret it as valid but other browsers to ignore
  > it.

* Otherwise, return `"\"` + `character`.

### Consuming a special function

This algorithm consumes input from a stream of [code points] and returns a
SassScript expression.

* Let `expression` be the result of consuming a [`SpecialFunctionExpression`].

  [`SpecialFunctionExpression`]: #specialfunctionexpression

* Return an unquoted interpolated string expression that would be identical to
  the source text according to CSS semantics for all possible interpolated
  strings.
  
## Semantics

### `Percent`

To evaluate a `Percent`, return an unquoted string with the value `%`.
