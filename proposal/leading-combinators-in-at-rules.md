# Leading Combinators in At-Rules: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4279))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Semantics](#semantics)
  * [Executing a Style Rule](#executing-a-style-rule)

## Background

> This section is non-normative.

As part of the [bogus combinators proposal], Sass no longer allows leading
visible combinators (e.g. `> .foo`) to be emitted, as these are generally
invalid and are ignored by browsers.

However, there is actually a case where selector a leading combinator can be
valid: when [nested within a `@scope` rule]. In this case, they behave as if
[prepended with `:where(:scope)`].

Because of this, code like

```scss
@scope (.foo) {
  > .bar {
    color: blue;
  }
}
```

is valid CSS that is properly handled by browsers, but Sass currently errors
due to the leading combinator it incorrectly views as invalid.

[bogus combinators proposal]: ../accepted/bogus-combinators.md
[nested within a `@scope` rule]: https://drafts.csswg.org/css-cascade-6/#scoped-rules
[prepended with `:where(:scope)`]: https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/At-rules/@scope#specificity_in_scope

## Summary

> This section is non-normative.

This proposal relaxes the prohibition on leading combinators to allow them
when nested under any unknown at-rule. This ensures that selectors like `> .foo`
are allowed when nested under `@scope`, as well as any similar at-rules CSS
adds in the future. These selectors would continue to produce an error when
directly nested under `@media` or other at-rules that Sass already has special
handling for where we know them to be invalid CSS.

This notably does *not* change the definition of "bogus selector". `> .foo`
on its own is still considered bogus, and we instead change the procedure for
evaluating a style rule within an unknown at-rule to prepend the implicit
`:where(:scope)` parent selector when checking whether a selector is bogus.
This is prepended only for validation, with only the original selector emitted
as CSS.

### Design Decisions

Currently, `@scope` is the only CSS at-rule where selectors with leading
combinators are valid, so one alternative would be to special-case `@scope` as
the only at-rule where we the prohibition is relaxed. However, this would make
the Sass spec brittle to new CSS at-rules with similar behavior.

Instead of prepending the implicit `:where(:scope)` when validating a selector,
we could instead redefine "bogus selector" itself to exclude those with leading
combinators nested within an unknown at-rule, but this would make that
definition dependent on where a selector is within a stylesheet, rather than
a simple property of the selector itself.

## Semantics

### Executing a Style Rule

This proposal replaces the second-to-last bullet in the procedure for
[executing a style rule] checking if `selector` is bogus with:

* If `parent` is an unknown at-rule, let `validating-selector` be the result of
  prepending `:where(:scope)` to `selector`.

* Otherwise, let `validating-selector` be `selector`.

* If `css` contains any children and `validating-selector` is bogus, throw an
  error.

[executing a style rule]: ../spec/style-rules.md#semantics
