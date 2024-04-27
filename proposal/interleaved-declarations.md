# Interleaved Declarations: Draft 1

*([Issue](https://github.com/sass/sass/issues/3846))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Definitions](#definitions)
  * [Current Style Rule](#current-style-rule)
  * [Current Style Rule](#current-style-rule-1)
  * [Current Keyframe Block](#current-keyframe-block)
* [Declarations](#declarations)
  * [Semantics](#semantics)
* [Unknown At-Rules](#unknown-at-rules)
  * [Semantics](#semantics-1)
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

### Current Style Rule

Change the definition of the [current style rule][old style] to:

[old style]: ../spec/style-rules.md#current-style-rule

> Differences are highlighted in bold.

The *current style rule* is the CSS style rule that was created by the innermost
[execution of a style rule], `@media` rule, `@supports` rule, unknown at-rule.
**This may be overridden by the [execution of a declaration].**

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

## Unknown At-Rules

### Semantics

Add the following to the semantics for executing an unknown at-rule `rule` after
"If `rule`'s name is 'font-face', or if its unprefixed name is 'keyframes',
append `css` to the current module's CSS":

* If `rule`'s name is case-insensitively equal to "nest", append `css` to
  `parent`.

## Deprecation Process

This proposal's change to the semantics of interleaved declarations is
backwards-incompatible. Before changing to the new semantics, an implementation
should have a period of deprecation in which it emits a deprecation warning for
a declaration whose `parent` is not the last statement in [the current module]'s
CSS without changing the existing behavior.

> Authors can move interleaved declarations before any nested rules to preserve
> existing behavior, or nest them in `& { ... }` style rules to anticipate the
> new behavior.
