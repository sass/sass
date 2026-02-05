# Expressions

## Table of Contents

* [Syntax](#syntax)
  * [`ArbitrarySubstitution`](#arbitrarysubstitution)
  * [`IfExpression`](#ifexpression)
  * [`ProductExpression`](#productexpression)
  * [`SingleExpression`](#singleexpression)
  * [`SpecialFunctionExpression`](#specialfunctionexpression)
* [Procedures](#procedures)
  * [Consuming a special function](#consuming-a-special-function)
  * [Evaluating an `IfCondition`](#evaluating-an-ifcondition)
  * [Evaluating an `IfGroup`](#evaluating-an-ifgroup)
* [Semantics](#semantics)
  * [`Percent`](#percent)
  * [`IfExpression`](#ifexpression-1)

## Syntax

### `ArbitrarySubstitution`

<x><pre>
**ArbitrarySubstitution**         ::= Interpolation | ArbitrarySubstitutionFunction
**ArbitrarySubstitutionFunction** ::= ('if(' | 'var(' | 'attr(')¹ [InterpolatedAnyValue] ')'
&#32;                               | [InterpolatedCustomIdentifier]² '(' [InterpolatedAnyValue] ')'
</pre></x>

1: This is matched case-insensitively.

2: No whitespace is allowed between this and the following `(`.

[InterpolatedCustomIdentifier]: syntax.md#interpolatedidentifier
[InterpolatedAnyValue]: syntax.md#interpolatedanyvalue

### `IfExpression`

<x><pre>
**IfExpression**          ::= 'if('¹ (IfBranch ';')* IfBranch ';'? ')'
**IfBranch**              ::= IfCondition ':' Expression
**IfCondition**           ::= IfConditionExpression | 'else'¹
**IfConditionExpression** ::= 'not'¹? IfGroup
&#32;                       | IfGroup ('and'¹ IfGroup)+
&#32;                       | IfGroup ('or'¹ IfGroup)+
&#32;                       | IfConditionArbitrary²
**IfConditionArbitrary**  ::= ArbitrarySubstitution IfGroup³
&#32;                       | ArbitrarySubstitution? IfGroup³ (
&#32;                             'and'¹ IfGroup³
&#32;                           | 'and'¹? ArbitrarySubstitution IfGroup³?
&#32;                         )+
&#32;                       | ArbitrarySubstitution? IfGroup³ (
&#32;                             'or'¹ IfGroup³
&#32;                           | 'or'¹? ArbitrarySubstitution IfGroup³?
&#32;                         )+
**IfGroup**               ::= Interpolation
&#32;                       | [InterpolatedIdentifier]⁴ '(' [InterpolatedAnyValue] ')'
&#32;                       | '(' IfConditionExpression ')'
&#32;                       | SassCondition
**SassCondition**         ::= 'sass(' Expression ')'
</pre></x>

[InterpolatedIdentifier]: syntax.md#interpolatedidentifier

1: This is matched case-insensitively.

2: Parse other `IfConditionExpression` branches preferentially to
`IfConditionArbitrary`.

3: These `IfGroup`s may not contain `SassCondition`s except within
interpolation.

4: No whitespace is allowed between this and the following `(`. This may not be
case-sensitively equal to `sass`, nor may it be case-insensitively equal to
`not`, `and`, or `or`.

Whitespace *must* appear between `and`, `or`, `not`, and `Interpolation` and a
following `(`.

> We could consider `if(not(): ...)` to be an unknown CSS function call, but
> it's extremely likely to be a typo and (for the same reason) almost certainly
> won't be defined to be meaningful by CSS in the future. Throwing an error
> helps users catch that error early.

> Because every `ArbitrarySubstitution` is also a valid `IfGroup`, there are
> multiple possible `IfConditionArbitrary` parse trees of certain token streams.
> But since the condition is evaluated purely as interpolated text, which parse
> tree is constructed in particular is not relevant.

> Per the CSS spec, arbitrary substitution functions are *only* allowed within
> `IfCondition`s and `IfBranch`es. Because the colon and semicolon separators
> are part of `if()`'s [argument grammar], they must appear literally in the
> stylesheet. The upshot of this is that we don't have to worry about handling
> those edge cases when parsing.

[argument grammar]: https://drafts.csswg.org/css-values-5/#argument-grammar

### `ProductExpression`

<x><pre>
**ProductExpression** ::= (ProductExpression ('*' | '%'))? SingleExpression
</pre></x>

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
**SpecialFunctionName**¹      ::= VendorPrefix? 'element('
&#32;                           | VendorPrefix 'calc('
&#32;                           | 'expression(' | 'type('
**VendorPrefix**¹             ::= '-' ([identifier-start code point] | [digit]) '-'
</pre></x>

> No browser has yet supported `type()` with a vendor prefix, nor are they
> likely to do so in the future given that vendor prefixes are largely unpopular
> now.

[identifier-start code point]: https://drafts.csswg.org/css-syntax-3/#identifier-start-code-point
[digit]: https://drafts.csswg.org/css-syntax-3/#digit

1: Both `SpecialFunctionName` and `VendorPrefix` are matched case-insensitively,
   and neither may contain whitespace.

## Procedures

### Consuming a special function

This algorithm consumes input from a stream of [code points] and returns a
SassScript expression.

[code points]: https://infra.spec.whatwg.org/#code-point

* Let `expression` be the result of consuming a [`SpecialFunctionExpression`].

  [`SpecialFunctionExpression`]: #specialfunctionexpression

* Return an unquoted interpolated string expression that would be identical to
  the source text according to CSS semantics for all possible interpolated
  strings.

### Evaluating an `IfCondition`

This procedure takes an `IfCondition` `condition` and returns either a boolean
representing a result that's known at build time or a string representing a
plain-CSS condition.

* If `condition` is case-insensitively equal to `else`, return true.

* If `condition` contains a single `IfGroup`:

  * Let `result` be the result of [evaluating the `IfGroup`].

  * If `condition` doesn't start (case-insensitively) with `'not'`, return `result`.

  * Otherwise, if `result` is a boolean, return `not result`.

  * Otherwise return the result of concatenating `'not '` and `result`.

  [evaluating the `IfGroup`]: #evaluating-an-ifgroup

* If `condition` contains multiple `IfGroup`s separated by (case-insensitive)
  `and`s:

  * Let `results` be an empty list.

  * For each `IfGroup` `group`:

    * Let `result` be the result of [evaluating the `group`].

    * If `result` is false, return false.

    * Otherwise, if `result` is a string, add it to `results`.

  * If `results` is empty, return true.

  * Otherwise, return `results` concatenated with `' and '` between each element.

  [evaluating the `group`]: #evaluating-an-ifgroup

* If `condition` contains multiple `IfGroup`s separated by (case-insensitive)
  `or`s:

  * Let `results` be an empty list.

  * For each `IfGroup` `group`:

    * Let `result` be the result of [evaluating the `group`].

    * If `result` is true, return true.

    * Otherwise, if `result` is a string, add it to `results`.

  * If `results` is empty, return false.

  * Otherwise, return `results` concatenated with `' or '` between each element.

* Otherwise, when `condition` is an `IfConditionArbitrary`, return the string
  result of evaluating any interpolations in `condition`.

  > All other tokens are included in the result as plain text.

### Evaluating an `IfGroup`

This procedure takes an `IfGroup` `group` and returns either a boolean
representing a result that's known at build time or a string representing a
plain-CSS condition.

* If `group` is a `SassCondition`, return a boolean indicating whether or not
  the result of evaluating its expression is truthy.

* Otherwise, if `group` contains a single `IfExpression`, return the result of
  [evaluating that expression as an `IfCondition`].

  [evaluating that expression as an `IfCondition`]: #evaluating-an-ifcondition

* Otherwise, return the result of evaluating any interpolation in `group`.
  
## Semantics

### `Percent`

To evaluate a `Percent`, return an unquoted string with the value `%`.

### `IfExpression`

* Let `results` be an empty list.

* For each `IfBranch` `branch`:

  * Let `condition` be the result of [evaluating `branch`'s condition].

  * If `condition` is true:

    * Let `value` be the result of evaluating `branch`'s expression.

    * If `results` is empty, return `value`.

    * Otherwise, add `["else", value]` to `results` and stop processing
      additional `IfBranch`es.

  * Otherwise, if `condition` is false, ignore this branch.

  * Otherwise, let `value` be the result of evaluating `branch`'s expression and
    add `[condition, value]` to `results`.

  [evaluating `branch`'s condition]: #evaluating-an-ifcondition

* If `results` is empty, return null.

* Otherwise, return a string representation of a [CSS `if()` function] with the
  given conditions (treated as unquoted strings) and values.

  [CSS `if()` function]: https://drafts.csswg.org/css-values-5/#if-notation

  > As usual, implementations are free to choose a string representation as long
  > as it matches the CSS semantics of the function.
