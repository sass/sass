# Escapes in Identifiers: Draft 3

*([Issue](https://github.com/sass/sass/issues/1542), [Changelog](identifier-escapes.changes.md))*

This proposal adjusts how escaped code points are handled by Sass outside of
string contexts. It's intended to bring Sass's semantics more in line with how
CSS handles escapes.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Compatibility](#compatibility)
* [Syntax](#syntax)
  * [Consuming an Identifier](#consuming-an-identifier)
  * [Consuming an Interpolated Identifier](#consuming-an-interpolated-identifier)
  * [Consuming a Name](#consuming-a-name)
  * [Consuming an Escaped Code Point](#consuming-an-escaped-code-point)

## Background

> This section is non-normative.

At time of writing, while Sass recognizes escaped code points in identifiers and
other names, it doesn't resolve them into the code points they represent. This
means that, for example, Sass considers the selector `.\!foo` and the selector
`.\21 foo` to be distinct. This is contrary to the [CSS Syntax Level 3][], which
[says that][css: consume name] the value of an escaped code point should be
included in the name rather than the syntax of the escape.

[CSS Syntax Level 3]: https://drafts.csswg.org/css-syntax-3/
[css: consume name]: https://drafts.csswg.org/css-syntax-3/#consume-name

However, the current behavior works well for unquoted strings in SassScript.
These strings need to distinguish between escaped code points and the literal
characters they represent, because unquoted strings can represent more than just
identifiers. For example, the SassScript expression `unquote("@x")` should be
rendered to CSS as `@x`, whereas the expression `\@x` should be rendered as
`\@x` (or `\40 x`). Any proposal for parsing escapes properly should preserve
this distinction.

## Summary

> This section is non-normative.

As identifiers are parsed, escapes will be normalized into a canonical form.
This preserves the benefits of the existing behavior, where `\@x` and
`unquote("@x")` are different SassScript expressions, while ensuring that
`.\!foo` and `.\21 foo` are considered the same selector.

The canonical form of a code point is:

* The literal code point if it's a valid identifier character; or

* a backslash followed by the code point's lowercase hex code followed by a
  space if it's not printable or a newline; or

* a backslash followed by the code point's lowercase hex code followed by a
  space if it's a digit at the beginning of an identifier; or

* a backslash followed by the literal code point.

For example, in SassScript:

* `ax`, `\61x`, and `\61 x` all parse to the unquoted string `ax`;
* `\7f x`, `\7fx`, and `\7Fx` all parse to the unquoted string `\7f x`; and
* `\31 x` and `\31x` parse to the unquoted string `\31 x`; and
* `\@x`, `\40x`, and `\0040x` all parse to the unquoted string `\@x`.

### Compatibility

The proposed change affects existing observable behavior. It's theoretically
possible that an existing user is, for example, using `\@x` and `\40 x` as
distinct map keys; or that they're relying on `length(\40 x)` returning `5`
rather than `3`. However, the chances of this seem extremely low, and it would
be very difficult to produce actionable deprecation warnings without
compromising efficiency.

Given that, and given that this is arguably a bug fix (in that we're moving
towards interpreting plain CSS text following the CSS spec, which we hadn't been
before), I propose that we don't consider this a breaking change and release it
with only a minor version bump.

## Syntax

This proposal defines a new algorithm for
[consuming an identifier](#consuming-an-identifier) and
[an interpolated identifier](#consuming-an-interpolated-identifier). These are
intended to replace the existing algorithms.

> Other than modifying the way escaped code points are handled, these algorithm
> are designed to accurately capture the current behavior of all Sass
> implementations.

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

  * Otherwise, if the stream starts with a [name-start code point][], consume it
    and append it to `string`.

  * Otherwise, throw an error.

  [consume an escaped code point]: #consuming-an-escaped-code-point
  [name-start code point]: https://drafts.csswg.org/css-syntax-3/#name-start-code-point

* [Consume a name](#consuming-a-name) and append it to `string`.

* Return `string`.

### Consuming an Interpolated Identifier

This algorithm consumes input from a stream of [code points][] and returns a
sequence of strings and/or expressions.

[code points]: https://infra.spec.whatwg.org/#code-point

The grammar for this production is:

<x><pre>
**InterpolatedIdentifier** ::= ([\<ident-token>][`<ident-token>`] | '-'? Interpolation) ([Name](#consuming-a-name) | Interpolation)*
</pre></x>

[name-start code point]: https://drafts.csswg.org/css-syntax-3/#name-start-code-point
[escape]: https://drafts.csswg.org/css-syntax-3/#escape-diagram

No whitespace is allowed between components of an `InterpolatedIdentifier`.

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
string.

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

* Otherwise, if `codepoint` is a [non-printable code point][], U+000A LINE FEED,
  U+000D CARRIAGE RETURN, or U+000C FORM FEED; *or* if `codepoint` is a
  [digit][] and the `start` flag is set:

  [non-printable code point]: https://drafts.csswg.org/css-syntax-3/#non-printable-code-point
  [digit]: https://drafts.csswg.org/css-syntax-3/#digit

    * Let `code` be the lowercase hexadecimal representation of `codepoint`,
      with no leading `0`s.

    * Return `"\"` + `code` + `" "`.

* Otherwise, return `"\"` + `character`.

