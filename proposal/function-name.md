# Function Name: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4048))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
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

* The function name `calc` is no longer forbidden, nor are any variants.

* Vendor-prefixed equivalents of the function names `and`, `or`, `not`,
  `expression`, and `url` are no longer forbidden.

* The CSS function names `element`, `expression`, and `url` are now matched
  case-insensitively. For example, `@function URL()` is now an error where it
  wasn't before.

## Semantics

### `@function`

Remove the second bullet point and replace the fourth bullet point of [the
semantics for `@function`] with:

[the semantics for `@function`]: ../spec/at-rules/function.md#semantics

* If `name` is `and`, `or`, or `not`, throw an error.

* If `name` is case-insensitively equal to `element`, `expression`, `type`, or
  `url`, throw an error.

* If `name` has a [vendor prefix] and the unprefixed identifier is
  case-insensitively equal to `element`, throw an error.

[vendor prefix]: ../spec/syntax.md#vendor-prefix

## Deprecation Process

The deprecation process will be divided into two phases:

### Phase 1

> This phase adds no breaking changes. Its purpose is to notify users of the
> upcoming changes to behavior and give them a chance to move towards passing
> future-proof units.

Phase 1 does not throw an error for function names that match case-insensitively
*but not* case-sensitively. Instead, it produces a deprecation warning named
`function-case`. All other changes are implemented as specified.

### Phase 2

Phase 2 implements the full changes described above. Per the Dart Sass
compatibility policy, it won't be released until at least three months after the
first release with the deprecation warning.
