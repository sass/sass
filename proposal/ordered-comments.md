# Ordered Comments: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/3504))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Traversal Order](#traversal-order)
* [Procedures](#procedures)
  * [Resolving a Module's Extensions](#resolving-a-modules-extensions)
* [Semantics](#semantics)
  * [`@import`](#import)

## Background

> This section is non-normative

When Sass introduced its new module system, the model of how stylesheets were
compiled changed as well. Instead of every source file adding CSS to one global
output stylesheet, each module produced its own CSS which was then stitched
together using more complex logic. This allowed us to support important features
like only emitting each module's CSS once, `@extend` scoping, and
`meta.load-css()`, but it did have one undesirable consequence: the order of
comments changed.

Because each module had its own separate CSS output, any loud comments that
appeared before or between that module's `@use` or `@forward` rules would end up
all together at the beginning of that module. Because that module's CSS was
always emitted in turn after its dependencies', these comments would appear to
be pushed later in the output than authors expected.

Although comment ordering doesn't affect the semantics of compiled CSS, it can
be confusing and undesirable for users. This is especially true when using
meaningful comments like license headers or postprocessor directives.

## Summary

> This section is non-normative

This proposal adjusts the order that comments are emitted relative to CSS so
that the resulting stylesheet is in essentially the same order it would have
been when using `@import`, without repeating stylesheets that are loaded
multiple times.

### Design Decisions

#### Traversal Order

There are two potential orders that could be chosen for comments. Either would
be better than the current ordering, and either can be better than the other
depending on the user's needs.

The order we propose here is "traversal order", in which comments are emitted in
the same order that they're evaluated. The other plausible ordering is "linked
order", in which comments that appear directly above a `@use` or `@forward` rule
are always emitted before the module loaded by that rule.

These two orderings result in the same output when each module is loaded once,
but once a given module is loaded multiple times with comments around it they
become different. For example:

```scss
// _upstream.scss
a {b: c}
```

```scss
// styles.scss
/* before @use */
@use 'upstream';

/* before @forward */
@forward 'upstream';
```

produces the following outputs:

```scss
// In traversal order
/* before @use */
a {
  b: c;
}

/* before @forward */
```

```scss
// In linked order
/* before @use */
/* before @forward
a {
  b: c;
}
```

Linked order makes sense when using comments to annotate information about
dependencies, but it's counterproductive when a user wants to annotate the _end_
of a module, since that comment would be considered linked to the next module
load. Traversal order handles that case better _and_ matches the old `@import`
behavior, so we chose to use it instead.

## Procedures

### Resolving a Module's Extensions

Adjust the definition of [Resolving a Module's Extensions] by replacing the
definition of the traversing procedure with the following:

[Resolving a Module's Extensions]: ../spec/at-rules/extend.md#resolving-a-modules-extensions

* Define a mutating recursive procedure, *traversing*, which takes a module
  `domestic`:

  * If `domestic` has already been traversed, do nothing.

  * For each module `upstream` in `domestic`'s dependencies:

    * For each unmarked comment in `domestic`'s CSS, if that comment originally
      appeared before the `@use` or `@forward` rule that loaded `upstream`, add
      a copy of that comment to `css` and then mark it.

    * Traverse `upstream`.

    > Because this traverses modules depth-first, it emits CSS in reverse
    > topological order.

  * Let `initial-imports` be the longest initial subsequence of top-level
    statements in `domestic`'s CSS tree that contains only comments and
    `@import` rules *and* that ends with an `@import` rule.

  * Insert a copy of `initial-imports` in `css` after the longest initial
    subsequence of comments and `@import` rules in `css`.

    > If there are no comments or `@import` rules in `css`, this initial
    > subsequence is empty and `initial-imports` is inserted at the beginning of
    > `css`.

  * For each top-level statement `statement` in `domestic`'s CSS tree after
    `initial-imports`:

    * If `statement` is a marked comment, ignore it.

    * Otherwise, add a copy of `statement` to the end of `css`, with any style
      rules' selectors replaced with the corresponding selectors in
      `new-selectors`.

## Semantics

### `@import`

Replace the line of [the `@import` semantics] that reads:

[the `@import` semantics]: ../spec/at-rules/import.md#semantics

* Add an `@import` with the evaluated modifiers to [the current module]'s
  CSS AST.

  [the current module]: ../spec/spec.md#current-module

with one that reads:

* Add an `@import` with the evaluated modifiers to [the current module]'s CSS
  AST after the longest initial subsequence of comments and `@import` rules.

> This ensures that each module's CSS individually raises plain CSS `@import`s
> to the top, and allows the procedure for combining CSS to be a bit simpler by
> only having to handle leading `@import`s.
