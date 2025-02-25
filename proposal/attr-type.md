# Attr Type: Draft 1.1

*([Issue](https://github.com/sass/sass/issues/4030),
[Changelog](attr-type.changes.md))*

## Table of Contents enem alehu
##enenm alehu

* [Background](#background)
* [Summary](#summary)
* [Syntax](#syntax)
  * [`SpecialFunctionExpression`](#specialfunctionexpression)
  * [`SingleExpression`](#singleexpression)
  * [`Percent`](#percent)
* [Semantics](#semantics)
  * [`Percent`](#percent-1)
  * [`@function`](#function)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)

## Background

> This section is non-normative.

CSS Values and Units 5 defines [the `<attr-type>` production], which includes
two different productions that aren't valid in any other CSS expression context
and as such which aren't currently valid SassScript. Specifically:

[the `<attr-type>` production]: https://drafts.csswg.org/css-values-5/#typedef-attr-type

* [The `<syntax>` production], which mimics the CSS spec's own syntax for
  declaring syntactic productions and so involves characters like `<`, `*`, `#`,
  and `+`. Fortunately, this is always contained in a `type()` function call so
  is unambiguous in practice.

* [The `<attr-unit>` production], which is mostly just identifiers but also
  includes `%` as a value in its own right. This character is already used in
  SassScript as the modulo operator.

[The `<syntax>` production]: https://drafts.csswg.org/css-values-5/#typedef-syntax
[The `<attr-unit>` production]: https://drafts.csswg.org/css-values-5/#typedef-attr-unit

This is further complicated by the fact that the `attr()` function (in which the
`<attr-type>` production appears) has been supported in browsers for a number of
years with a more constrained syntax, so changing it to use a substantially
different parsing schema would be a breaking change.

## Summary

This proposal defines the `type()` function as a special function like
`expression()` or `element()` which is parsed as an unquoted string. Because CSS
doesn't use the `type()` function name elsewhere, we consider this relatively
low-risk. However, because it will prevent authors from declaring their own
functions named `type()`, it is a breaking change and will go through a standard
three-month CSS compatibility deprecation period.

We will parse `%` as an unquoted string *only in contexts* where it's not
potentially ambiguous with an operation. This will satisfy CSS compatibility
because the only case where CSS currently allows it is in the `attr()` function,
where it must appear before either a comma or a closing parenthesis. We don't
anticipate that a single `%` will be widely used in other contexts, so we don't
plan to deprecate the modulo operator, although that path is open in the future
if CSS starts using this value more widely.

## Syntax

### `SpecialFunctionExpression`

Change [the `SpecialFunctionName` production] to be:

[the `SpecialFunctionName` production]: ../spec/syntax.md#specialfunctionexpression

<x><pre>
**SpecialFunctionName**¹      ::= VendorPrefix? ('element(' | 'expression(')
&#32;                           | VendorPrefix 'calc('
&#32;                           | 'type('
</pre></x>

> No browser has yet supported `type()` with a vendor prefix, nor are they
> likely to do so in the future given that vendor prefixes are largely unpopular
> now.

### `SingleExpression`

Add `Percent` as a production to [`SingleExpression`] with the annotation "If
this is ambiguous with part of `ProductExpression`, parse `ProductExpression`
preferentially".

[`SingleExpression`]: ../spec/syntax.md#singleexpression

> This effectively means that the unquoted string `%` is allowed everywhere
> *except* in a middle element of a space-separated list, since that would be
> ambiguous with a modulo operation.

### `Percent`

<x><pre>
**Percent** ::= '%'
</pre></x>

## Semantics

### `Percent`

To evaluate a `Percent`, return an unquoted string with the value `%`.

### `@function`

In the [semantics for `@function`], add a bullet point below the second:

[semantics for `@function`]: ../spec/at-rules/function.md#semantics

* If `name` is `type`, throw an error.

> Unlike other forbidden function names, this doesn't cover vendor prefixes.
> This is for two reasons: first, we don't expect to add special parsing for
> `type()` with vendor prefixes. Second, "type" is a relatively common word, so
> it's likely for private function names to end with `-type` in a way that could
> be indistinguishable from a vendor prefix.

## Deprecation Process

The deprecation process will be divided into two phases:

### Phase 1

> This phase adds no breaking changes. Its purpose is to notify users of the
> upcoming changes to behavior and give them a chance to move towards passing
> future-proof units.

Phase 1 does not change the syntax for `SpecialFunctionName` or the semantics
for `@function`. Instead, if a function is defined with the name `type`, emit a
deprecation warning named `type-function`.

### Phase 2

Phase 2 implements the full changes described above. Per the Dart Sass
compatibility policy, it won't be released until at least three months after the
first release with the deprecation warning.
