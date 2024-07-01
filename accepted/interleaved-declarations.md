# Interleaved Declarations: Draft 1

*([Issue](https://github.com/sass/sass/issues/3846),
[Changelog](interleaved-declarations.changes.md))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Definitions](#definitions)
  * [Current Style Rule](#current-style-rule)
  * [Current Keyframe Block](#current-keyframe-block)
* [Declarations](#declarations)
  * [Semantics](#semantics)
* [`@nest` Rule](#nest-rule)
  * [Syntax](#syntax)
  * [Semantics](#semantics-1)
* [Deprecation Process](#deprecation-process)
  * [`@nest` Rule](#nest-rule-1)

## Background

> This section is non-normative.

Historically, Sass has hoisted declarations that appear after nested rules into
the original style rule. Although this doesn't match the logical ordering of the
source document, it *does* avoid duplicating the style rule (and any other
nested rules) for each set of interleaved properties.

Many years after this decision was made, the [CSS Nesting Module] was published
by the W3C, adding native support for nesting rules within style rules using
essentially the same syntax as Sass. Although initially its semantics for
interleaved declarations were the same as Sass's, [a later update] (four days
ago at time of writing) changed it to preserve the logical ordering of
interleaved declarations.

[CSS Nesting Module]: https://drafts.csswg.org/css-nesting/
[a later update]: https://github.com/w3c/csswg-drafts/commit/e5547b96f5de6fb5a68d050f42d562c448b99d0b

## Summary

> This section is non-normative.

This proposal changes the behavior of declarations that appear after nested
rules. Instead of being hoisted to the beginning of the parent rule, they will
now be placed in their original logical order, duplicating the parent selector.

That is, the following Sass:

```scss
.a {
  color: red;
  @media screen {color: blue}
  color: green;
}
```

will now compile to this CSS:

```scss
.a {
  color: red;
}

@media screen {
  .a {
    color: blue;
  }
}

.a {
  color: green;
}
```

## Definitions

### Current Style Rule

Change the definition of the [current style rule][old] to:

[old]: ../spec/style-rules.md#current-style-rule

> Differences are highlighted in bold.

The *current style rule* is the CSS style rule that was created by the innermost
[execution of a style rule], `@media` rule, `@supports` rule, unknown at-rule.
**This may be overridden by the [execution of a declaration].**

[execution of a style rule]: ../spec/style-rules.md#semantics
[execution of a declaration]: #semantics

### Current Keyframe Block

Change the definition of the [current keyframe block][old keyframe] to:

[old keyframe]: ../spec/style-rules.md#current-keyframe-block

> Differences are highlighted in bold.

The *current keyframe block* is the CSS keyframe block that was created by the
innermost [execution of a style rule]. **This may be overridden by the
[execution of a declaration].**

## Declarations

### Semantics

Add the following to the semantics for [executing a declaration] `declaration`
before "Append `css` to `parent`":

[executing a declaration]: ../spec/declarations.md#semantics

* If `parent` isn't the last statement in its parent:

  * Let `copy` by a copy of `parent` without any children.

  * Append `copy` to `parent`'s parent.

  * Set the [current style rule], [keyframe block], or at-rule (according to
    `copy`'s type) to `copy`, for the remaining duration of its previous value.

  * Set `parent` to `copy`.

[current style rule]: #current-style-rule
[keyframe block]: #current-keyframe-block

## `@nest` Rule

Add special semantics for the `@nest` rule. Although this rule is primarily
intended to give the CSSOM a consistent representation for interleaved
declarations and is never required to be written explicitly, it *is* valid CSS
and Sass must ensure that its use preserves the existing CSS semantics.

> This is also provides an explicit way for Sass authors to write styles that
> are consistent with CSS semantics during the deprecation period for
> interleaved declarations.

### Syntax

<x><pre>
**NestRule** ::= '@nest'¹ '{' Statements '}'
</pre></x>

1: This is case-insensitive.

### Semantics

To execute a `@nest` rule `rule`:

* If there's a [current keyframe block], throw an error.

  [current keyframe block]: #current-keyframe-block

* If there's a [current style rule], evaluate each child in `rule`'s
  `Statement`s.

* Otherwise, [evaluate `rule` as an unknown at-rule] with
  `InterpolatedIdentifier` "nest", no `InterpolatedValue`, and the same
  `Statements`.

  [evaluate `rule` as an unknown at-rule]: ../spec/at-rules/unknown.md

## Deprecation Process

This proposal's change to the semantics of interleaved declarations is
backwards-incompatible. Before changing to the new semantics, an implementation
should have a period of deprecation in which it emits a deprecation warning for
a declaration whose `parent` is not the last statement in its parent without
changing the existing behavior.

> Authors can move interleaved declarations before any nested rules to preserve
> existing behavior, or nest them in `& { ... }` style rules to anticipate the
> new behavior.

### `@nest` Rule

During the deprecation period, use the `@nest` syntax specified above but the
following semantics:

* If there's a [current keyframe block], throw an error.

* If there's a [current style rule] `style-rule`:

  * If `style-rule`'s stylesheet was [parsed as CSS], evaluate each child in
    `rule`'s `Statement`s.

  * Otherwise, [evaluate `rule` as a style rule with `SelectorList` "&" and the
    same `Statements`.

* Otherwise, [evaluate `rule` as an unknown at-rule] with
  `InterpolatedIdentifier` "nest", no `InterpolatedValue`, and the same
  `Statements`.

[parsed as CSS]: ../spec/syntax.md#parsing-text-as-css
