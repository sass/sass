# Interleaved Declarations: Draft 2.0

*([Issue](https://github.com/sass/sass/issues/3846),
[Changelog](interleaved-declarations.changes.md))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Definitions](#definitions)
  * [Current Style Rule](#current-style-rule)
  * [Current Keyframe Block](#current-keyframe-block)
* [Procedures](#procedures)
  * [Splitting the Current Parent If Necessary](#splitting-the-current-parent-if-necessary)
* [Declarations](#declarations)
  * [Semantics](#semantics)
* [Deprecation Process](#deprecation-process)

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

## Procedures

### Splitting the Current Parent If Necessary

This procedure accepts no arguments and returns a style rule, keyframe block,
at-rule, or null.

* Let `parent` be the [current style rule], [keyframe block], or at-rule if one
  exists; or the innermost if multiple exist.

  [current style rule]: ../spec/style-rules.md#current-style-rule
  [keyframe block]: ../spec/style-rules.md#current-style-rule

* If `parent` is null, return null.

* Otherwise, if `parent` is the last statement in its parent, return `parent`.

* Otherwise:

  * Let `copy` by a copy of `parent` without any children.

  * Append `copy` to `parent`'s parent.

  * Set the [current style rule], [keyframe block], or at-rule (according to
    `copy`'s type) to `copy`, for the remaining duration of its previous value.

  * Return `copy`.

## Declarations

### Semantics

In the semantics for [executing a declaration], [executing an unknown at-rule],
and executing a comment, before "Append `css` to `parent`", assign `parent` to
the result of [splitting the current parent if necessary] rather than whatever
it was assigned to before.

[executing a declaration]: ../spec/declarations.md#semantics
[executing an unknown at-rule]: ../spec/at-rules/unknown.md#semantics
[splitting the current parent if necessary]: #splitting-the-current-parent-if-necessary

* Split `parent` if necessary.

## Deprecation Process

This proposal's change to the semantics of interleaved declarations is
backwards-incompatible. Before changing to the new semantics, an implementation
should have a period of deprecation in which it emits a deprecation warning for
a declaration whose `parent` is not the last statement in its parent without
changing the existing behavior.

> Authors can move interleaved declarations before any nested rules to preserve
> existing behavior, or nest them in `& { ... }` style rules to anticipate the
> new behavior.
