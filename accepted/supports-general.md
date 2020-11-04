# `@supports` `<general-enclosed>`: Draft 2.1

*([Issue](https://github.com/sass/sass/issues/2780), [Changelog](supports-general.changes.md))*

This proposal defines how Sass parses supports queries that use the
[`<general-enclosed>`][] production.

[`<general-enclosed>`]: https://drafts.csswg.org/mediaqueries-4/#typedef-general-enclosed

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [How Much To Support](#how-much-to-support)
    * [SassScript Injection](#sassscript-injection)
    * [Backwards Compatibility](#backwards-compatibility)
* [Syntax](#syntax)
  * [`SupportsCondition`](#supportscondition)
  * [`InterpolatedAnyValue`](#interpolatedanyvalue)

## Background

> This section is non-normative.

Historically, Sass has supported the `@supports` condition syntax as defined in
[the April 2013 CSS Conditional Rules Level 3 Candidate Recommendation][], with
the addition of supporting raw SassScript expressions in the declaration syntax.
The [Editor's Draft][] (as published October 2019) expands this syntax with a
`<general-enclosed>` production to ensure parser forwards-compatibility with
future CSS, which Sass does not yet support. The [Level 4 Editor's Draft][] (as
of March 2019) further adds a `selector()` function syntax (which is covered by
`<general-enclosed>`), which is [supported by Firefox][] as of version 69.

[the April 2013 CSS Conditional Rules Level 3 Candidate Recommendation]: https://www.w3.org/TR/2013/CR-css3-conditional-20130404/#at-supports
[Editor's Draft]: https://drafts.csswg.org/css-conditional-3/#at-supports
[Level 4 Editor's Draft]: https://drafts.csswg.org/css-conditional-4/#at-supports-ext
[supported by Firefox]: https://caniuse.com/#search=supports%20selector

Sass's current `@supports` rule syntax allows SassScript expressions in place of
either the declaration name or value in `<supports-decl>`. This syntax doesn't
need to be wrapped in interpolation, which means that many expressions that
would be parsed in CSS as `<general-enclosed>` are currently parsed by Sass as
`<supports-decl>`, and even more expressions need arbitrary look-ahead to
determine whether they have a `:` that would distinguish them between
`<supports-decl>` and `<general-enclosed>`.

## Summary

> This section is non-normative.

Sass will parse the `<general-enclosed>` production essentially as defined by
CSS, with the following exceptions:

* SassScript can be injected in the productions using interpolation.

* The `(<ident> <any-value>)` syntax for `<general-enclosed>` may not contain
  top-level `":"` tokens. This preserves backwards-compatibility with existing
  `@supports` rules that use non-interpolated SassScript expressions in
  `<supports-decl>` and may help catch accidental syntax errors.

### Design Decisions

#### How Much To Support

Per Sass's CSS compatibility policy, it must add support for at least the
`selector()` function, since it's been shipped in a browser. Everything else
under `<general-enclosed>` is optional, though, since the spec says "Authors
must not use `<general-enclosed>` in their stylesheets." We could choose to
avoid implementing it entirely and just special-case `selector()`, or we could
choose to implement only the function syntax and ignore the `(<ident>
<any-value>)` syntax.

This proposal covers the full `<general-enclosed>` syntax for the same reason
CSS does: forwards compatibility. Sass has a general policy of loose coupling
with CSS, so that Sass needs as few updates as possible as CSS continues to
evolve. Supporting the full range of potential syntax for `@supports` conditions
now means that we'll need fewer proposals like this in the future as new
conditions are added in practice.

#### SassScript Injection

There are two possible ways to allow SassScript to be injected into the
`<general-enclosed>` production. One is to support it only via interpolation,
allowing the production to otherwise exactly match the CSS syntax; the other is
to allow raw Sass-script values to be used within the parentheses.

The latter option is appealing at first glance. It requires fewer characters,
and matches the parsing of `<supports-decl>`, which allows non-interpolated
expressions on either side of the `":"`. However, it's only CSS-compatible as
long as all the syntax CSS uses in those positions is also a valid SassScript
expression. If not, Sass must continually update its syntax to support new CSS
constructs.

We've been burned by this in the past. Both this proposal and the [media ranges
proposal][] would have been unnecessary (or at least much simpler) if the rules
in questions had only allowed SassScript via interpolation in the first place.
And it's likely that this syntax *will* evolve in ways that aren't
SassScript-compatible, as with the `selector()` function [mentioned above][].

[media ranges proposal]: ../accepted/media-ranges.md
[mentioned above]: #background

Given this, it's much safer to only allow SassScript within interpolation.

#### Backwards Compatibility

The `<general-enclosed>` syntax is very broad, so care is needed to ensure that
adding support for it doesn't break existing Sass stylesheets—particularly those
that are relying on SassScript in `<supports-decl>`. For example, strictly
speaking `(foo + bar: baz)` is valid CSS that parses as `<general-enclosed>`,
but Sass needs to continue to parse it as a declaration containing a SassScript
expression on the left-hand side.

The question then is how to balance backwards-compatibility with CSS
compatibility. In most cases, CSS takes clear precedence, and we could manage
that here by declaring that `<supports-decl>`s that begin with an
`InterpolatedIdentifier` must immediately follow it with a `":"` and allowing
all other syntax to be parsed as `<general-enclosed>`. However, this approach is
not without its downsides. It would require a deprecation period, and it would
parse `<general-enclosed>` in a number of cases (such as `(foo: )` or `(fo o:
bar)`) that are much more likely to be typos.

Fortunately, `<general-enclosed>` is explicitly specified for
forwards-compatibility rather than having CSS semantics in and of itself, so we
have more flexibility to limit compatibility with it in favor of Sass backwards
compatibility. This allows us to limit the syntax we parse as
`<general-enclosed>` to forbid a top-level `":"`, which ensures it's never
ambiguous with a declaration and thus with raw SassScript.

## Syntax

### `SupportsCondition`

<x><pre>
**SupportsCondition**   ::= 'not' SupportsInParens
&#32;                     | SupportsInParens ('and' SupportsInParens)*
&#32;                     | SupportsInParens ('or' SupportsInParens)*
**SupportsInParens**    ::= '(' (SupportsCondition | SupportsDeclaration | SupportsAnything) ')'
&#32;                     | SupportsFunction | Interpolation
**SupportsDeclaration** ::= Expression¹ ':' Expression
**SupportsAnything**    ::= [InterpolatedIdentifier][]² [InterpolatedAnyValue][]³?
**SupportsFunction**    ::= [InterpolatedIdentifier][]⁴ '(' [InterpolatedAnyValue][]? ')'
</pre></x>

[InterpolatedIdentifier]: ../spec/syntax.md#interpolatedidentifier
[InterpolatedAnyValue]: #interpolatedanyvalue

1: This `Expression` may not begin with the identifier `"not"` or the token
`"("`.

2: This `InterpolatedIdentifier` may not be the identifier `"not"`.

3: This `InterpolatedAnyValue` may not contain a top-level `":"`, and it may not
begin with the identifier tokens `"and"` or `"or"`.

4: This `InterpolatedIdentifier` may not be the identifier `"not"`. No
whitespace is allowed between it and the following `"("`.

The identifiers `"not"`, `"and"`, and `"or"` are matched case-insensitively for
the purposes of this production.

> Implementations must perform some amount of lookahead to disambiguate between
> `SupportsDeclaration`, `SupportsAnything`, and `SupportsFunction`. When doing
> so, it may be helpful to note that the contents of a `SupportsInParens` must
> be parsed as a `SupportsDeclaration` if and only if it contains a top-level
> `":"`.

### `InterpolatedAnyValue`

The `InterpolatedAnyValue` production is identical to CSS's [`<any-value>`][]
except that after it parses `"#{"`, it parses an `Expression` which must be
followed by `"}"`.

[`<any-value>`]: https://drafts.csswg.org/css-syntax-3/#typedef-any-value
