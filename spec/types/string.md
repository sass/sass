# Strings

## Table of Contents

* [Definitions](#definitions)
  * [Logical String](#logical-string)
  * [Anti-Identifier](#anti-identifier)
* [Types](#types)
* [Procedures](#procedures)
  * [Getting the Semantic Value of a String](#getting-the-semantic-value-of-a-string)
  * [Converting a String to an Identifier](#converting-a-string-to-an-identifier)

## Definitions

### Logical String

A *logical string* is a finite sequence of Unicode code points. This terminology
is used specifically when it would otherwise be ambiguous with a [SassScript
string]. It's referred to as just a *string* in cases where there's no
ambiguity.

[SassScript string]: #types

### Anti-Identifier

An *anti-identifier* is any Sass string that has no [semantic value].

[logical string]: #logical-string
[semantic value]: #getting-the-semantic-value-of-a-string

> The only quoted string which is an anti-identifier is the empty string. All
> unquoted strings whose textual values aren't valid CSS identifiers are
> anti-identifiers, such as `meta.unquote("$")` or `meta.unquote("a b")`.

## Types

A *SassScript string* (referred to as just a *string* when doing so is
unambiguous with the more general concept) contains a [logical string] (known as
its *contents* or its *textual value* when necessary to disambiguate with its
[semantic value]). A string may or may not be *quoted*.

## Procedures

### Getting the Semantic Value of a String

This procedure takes a SassScript string `string` and returns a [logical
string].

* If `string` is quoted, return its textual value.

* If `string`'s textual value is an [`<ident-token>`], return that token's
  value.

* Otherwise, throw an error.

[`<ident-token>`]: https://www.w3.org/TR/css-syntax-3/#ident-token-diagram

### Converting a String to an Identifier

This procedure takes a [logical string] `value` and returns an unquoted
SassScript string whose contents is an [`<ident-token>`] `ident` such that:

* `ident`'s value is equal to `value`.

* `ident` only contains escape codes for code points whose literal
  representation would cause it not to be a valid `<ident-token>`, code points
  in [private use areas], or U+FFFD REPLACEMENT CHARACTER.

* All code points in private use areas and U+FFFD REPLACEMENT CHARACTER are
  represented as escape codes.

* Escape codes use the lowercase hexadecimal representation with no leading `0`s
  for code points in private use areas, ASCII control characters and digits,
  U+007F DELETE, and U+FFFD REPLACEMENT CHARACTER; and for no other code points.

* All hexadecimal escape codes are followed by U+0020 SPACE.
  
[private use areas]: https://en.wikipedia.org/wiki/Private_Use_Areas

If there is no such identifier, throw an error.

> This is intended to uniquely determine a single identifier representation that
> can represent any given logical string except those which cannot be
> represented as identifiers (the empty string or strings containing U+0000
> NULL).
