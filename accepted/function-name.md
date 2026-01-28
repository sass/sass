# Function Name: Draft 2.0

*([Issue](https://github.com/sass/sass/issues/4048), [Changelog](function-name.changes.md))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Syntax](#syntax)
  * [`SpecialFunctionExpression`](#specialfunctionexpression)
* [Semantics](#semantics)
  * [`@function`](#function)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)

## Background

> This section is non-normative.

Historically, Sass [has forbidden] the use of certain reserved words and
functions that have special parsing as plain CSS from being used as the names of
user-defined functions. This ensures that users don't accidentally write a
function that can't actually be called, because special syntactic handling takes
over. However, there were a number of oddities and inconsistencies in this
prohibition:

[has forbidden]: ../spec/at-rules/function.md#semantics

* These function names were specified as exact matches to lowercase names,
  rather than being case-sensitive, despite the fact that CSS function names are
  case-insensitive (albeit idiomatically written in lower case).

* All such forbidden function names would match vendor-prefixed equivalents as
  well, despite the fact that some (`and`, `or`, and `not`) are forbidden
  because they conflict with Sass identifiers, not CSS identifiers, and thus
  could never possibly have meaningful vendor prefixes.

* The `url()` function has been widely-supported for decades and was never
  guarded by a vendor prefix. The `expression()` function is not standard and
  will never be supported by new browsers, and when it was supported it was
  never guarded by a vendor prefix. Requiring vendor prefix matches for these
  function names is unnecessary.

* As of the introduction of [first-class calculations], all calculation
  functions are now explicitly defined so that an in-scope user-defined function
  will take precedence over the original CSS function name without needing to
  reparse the entire function call. Despite this, the function name `calc()` is
  still forbidden.

[first-class calculations]: ../accepted/first-class-calc.md

## Summary

This proposal addresses all three of the issues above:

* The function name `calc` is no longer forbidden, nor are any case variants.
  The vendor-prefixed variant is still forbidden.

* Vendor-prefixed equivalents of the function names `and`, `or`, `not`,
  `expression`, and `url` are no longer forbidden.

* The CSS function names `element`, `expression`, and `url` are now matched
  case-insensitively in both function definitions and calls. For example,
  `@function URL()` is now an error where it wasn't before.

## Syntax

### `SpecialFunctionExpression`

Replace [the definition of `SpecialFunctionName`] with the following:

[the definition of `SpecialFunctionName`]: ../spec/expressions.md#specialfunctionexpression

<x><pre>
**SpecialFunctionName**¹      ::= VendorPrefix? 'element('
&#32;                           | VendorPrefix 'calc('
&#32;                           | 'progid:' \[a-z.]* '('
&#32;                           | 'expression(' | 'type('
</pre></x>

1: Both `SpecialFunctionName` and `VendorPrefix` are matched case-insensitively,
   and neither may contain whitespace.

## Semantics

### `@function`

Remove the second bullet point and replace the third bullet point of [the
semantics for `@function`] with:

[the semantics for `@function`]: ../spec/at-rules/function.md#semantics

* If `name` is `and`, `or`, or `not`, throw an error.

* If `name` is case-insensitively equal to `element`, `expression`, `type`, or
  `url`, throw an error.

* If `name` has a [vendor prefix] and the unprefixed identifier is
  case-insensitively equal to `element` or `calc`, throw an error.

[vendor prefix]: ../spec/syntax.md#vendor-prefix

## Deprecation Process

The deprecation process will be divided into two phases:

### Phase 1

> This phase adds no breaking changes. Its purpose is to notify users of the
> upcoming changes to behavior and give them a chance to move towards
> future-proof function names.

Phase 1 does not throw an error for user-defined functions with names that match
case-insensitively *but not* case-sensitively (other than `type`, which already
threw an error). Instead, it produces a deprecation warning named
`function-name`.

In phase 1, calls to vendor-prefixed `expression()`, `url()`, and `progid:...()`
functions continue to be parsed as

<x><pre>
**SpecialFunctionName**¹      ::= VendorPrefix? 'element('
&#32;                           | VendorPrefix 'calc('
&#32;                           | VendorPrefix? 'progid:' \[a-z.]* '('
&#32;                           | VendorPrefix? 'expression('
&#32;                           | 'type('
**InterpolatedUrl**           ::= VendorPrefix? 'url(' (QuotedString | InterpolatedUnquotedUrlContents) ')'
</pre></x>

If a vendor-prefixed `expression()` or `url()` isn't also valid when parsed as a
[`FunctionCall`], or if that `FunctionCall`'s `ArgumentList` would produce an
error when [parsed as CSS] for anything other than interpolation, produce a
`function-name` deprecation warning.

[`FunctionCall`]: ../spec/functions.md#syntax
[parsed as CSS]: ../spec/syntax.md#parsing-text-as-css

> Forbidding SassScript in `expression()` and `url()` calls ensures that they'll
> work the same both before and after the breaking change. Otherwise,
> `expression(1 + 1)` would be syntactically valid in both cases, but would
> produce `expression(1 + 1)` before the change and `expression(2)` afterwards.

When parsing a vendor-prefixed call to `progid:...()`, produce a `function-name`
deprecation warning.

> This will be invalid syntax entirely in Phase 2.

### Phase 2

Phase 2 implements the full changes described above. Per the Dart Sass
compatibility policy, it won't be released until at least three months after the
first release with the deprecation warning.
