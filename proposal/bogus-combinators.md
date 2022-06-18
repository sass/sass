# Bogus Combinators: Draft 1

*([Issue](https://github.com/sass/sass/issues/3340))*

This proposal increases the strictness with which Sass parses and resolves
non-standard ("bogus") uses of selector combinators. In particular, it forbids
the use of multiple combinators in a row (such as `div + ~ a`) and limits the
use of leading combinators (such as `> a`) and trailing combinators (such as `a
>`) to selector nesting.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Phase 1: Deprecation](#phase-1-deprecation)
  * [Phase 2: Removal](#phase-2-removal)
* [Definitions](#definitions)
  * [Visible Combinator](#visible-combinator)
  * [Complex Selector](#complex-selector)
  * [Complex Selector Component](#complex-selector-component)
  * [Trailing Combinator](#trailing-combinator)
  * [Bogus Selector](#bogus-selector)
* [Syntax](#syntax)
* [Semantics](#semantics)
  * [Evaluating a Style Rule](#evaluating-a-style-rule)
* [Functions](#functions)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)

## Background

> This section is non-normative.

Currently, Sass is very liberal when it comes to where explicit selector
combinators can be written—much more so than CSS itself. It allows:

* Multiple combinators between compound selectors (`a > + b`).
* Combinators at the beginning of selectors (`> a`).
* Combinators at the end of selectors (`a >`).

The latter two are useful when nesting style rules, but the former has no known
use at all. Historically, we've said that we support these because they enable
some browser hacks, but this seems to be purely hypothetical: [browserhacks.com]
is the most comprehensive source of hacks I've found, and it doesn't list any
hacks that are enabled by this support, even among its "legacy hacks".

[browserhacks.com]: http://browserhacks.com

In addition, supporting these selectors imposes substantial complexity on Sass
implementations. They [cause bugs] and make the data model more complex than it
needs to be. This in turn makes bugs like [sass/sass#1807] that don't involve
non-standard combinators more difficult to resolve.

[cause bugs]: https://github.com/sass/dart-sass/issues/1053
[sass/sass#1807]: https://github.com/sass/sass/issues/1807

## Summary

> This section is non-normative.

We'll move towards forbidding these combinators in two phases.

### Phase 1: Deprecation

In the first phase, we'll issue deprecation messages for all forbidden cases,
but only change behavior in cases where Sass was already producing invalid CSS
anyway. In particular:

* Once nesting and extensions have been resolved, if any of a style rule's
  selectors contains a leading, trailing, or multiple combinator after nesting
  and extensions are resolved, omit that style rule from the generated CSS and
  emit a deprecation warning.

* If a selector with a leading or trailing combinator is used with any
  extend-related infrastructure, emit a deprecation warning but *don't* change
  the behavior unless the resolved selector still has a bogus combinator, as
  above.

* If a selector with a doubled combinator is used with any extend-related
  infrastructure, emit a deprecation warning and treat that selector as though
  it matches no elements.

This will be sufficient to substantially simplify the implementation without
affecting the in-browser behavior of any stylesheets.

### Phase 2: Removal

In the second phase, which for existing implementations will accompany a major
version release, we will emit errors everywhere Phase 1 produced deprecation
warnings. In particular:

* If a style rule's selectors contain leading, trailing, or multiple combinators
  after nesting is resolved, emit an error.

* If a selector with a leading, trailing, or multiple combinator is used with
  `@extend` (which, given the previous restriction, will only be possible using
  extension functions from `sass:selector`), emit an error.

## Definitions

> Most existing definitions being modified here haven't been defined explicitly
> before this document. The old definitions are listed in strikethrough mode to
> clarify the change.

### Visible Combinator

A *visible combinator* is any selector [combinator] other than the [descendant
combinator].

[selector combinator]: https://drafts.csswg.org/selectors-4/#combinators
[descendant combinator]: https://drafts.csswg.org/selectors-4/#descendant-combinators

### Complex Selector

~~A *complex selector* is a sequence of [visible combinators] (its *leading
combinators*) as well as a sequence of one or more [complex selector
components].~~

[visible combinators]: #visible-combinator 
[complex selector components]: #complex-selector-components

A *complex selector* is an optional [visible combinator] (its *leading
combinator*) as well as a sequence of one or more [complex selector components].

[visible combinator]: #visible-combinator 

### Complex Selector Component

~~A *complex selector component* is a compound selector as well as a sequence of
zero or more [visible combinators].~~

A *complex selector component* is a compound selector as well as a single
[combinator].

### Trailing Combinator

A [complex selector]'s *trailing combinator* is its final [complex selector
component]'s combinator if it's not a [descendant combinator]. If it *is* a
descendant combinator, the complex selector doesn't have a trailing combinator.

[complex selector]: #complex-selector
[complex selector component]: #complex-selector-component

### Bogus Selector

A [complex selector] is *bogus* if it has a leading or [trailing combinator], or
if any of the simple selectors it transitively contains is a selector pseudo
with a bogus selector.

A selector list is *bogus* if any of its complex selectors are bogus.

[trailing combinator]: #trailing-combinator

## Syntax

> Note that the existing productions being modified have not been defined
> explicitly before this document. The old productions are listed in
> strikethrough mode to clarify the change.

This proposal modifies the existing `ComplexSelector` and
`ComplexSelectorComponent` productions to drop support for multiple combinators:

<x><pre>
~~**ComplexSelector**          ::= Combinator* ComplexSelectorComponent+~~
~~**ComplexSelectorComponent** ::= CompoundSelector Combinator*~~
**ComplexSelector**          ::= Combinator? ComplexSelectorComponent+
**ComplexSelectorComponent** ::= CompoundSelector Combinator?
**Combinator**               ::= '+' | '>' | '~'
</pre></x>

## Semantics

### Evaluating a Style Rule

This proposal adds the following to [Evaluating a Style Rule], before
creating a CSS style rule:

[Evaluating a Style Rule]: ../spec/style-rules.md#semantics

* If `selector` is [bogus], throw an error.

[bogus]: #bogus-selector

## Functions

For the `selector.append()`, `selector.extend()`, `selector.is-superselector()`,
`selector.replace()`, and `selector.unify()` functions, after parsing their
selector arguments, throw an error if any of the parsed selectors are [bogus].

> `selector.nest()` is still allowed to take bogus selectors, because they're
> explicitly useful for nesting.

## Deprecation Process

The deprecation will be divided into two phases:

### Phase 1

This phase will only change behavior that doesn't affect in-browser rendering.
In particular:

* The parsing of `ComplexSelector` and `ComplexSelectorComponent` is unchanged.

* A complex selector is instead considered [bogus] if it contains any leading
  combinators, if its final component contains any combinators, or if any of its
  components contains multiple combinators, or if any of the simple selectors it
  transitively contains is a selector pseudo with a bogus selector.

* The newly-added errors produce deprecation warnings instead.

* In [Evalutating a Style Rule], only append `css` to the current module's CSS
  if its selector is not [bogus].

* In [Extending a Selector], if a complex selector has multiple combinators, or
  if any of its components has multiple combinators, treat it as a selector that
  can match no elements.

  [Extending a Selector]: ../spec/at-rules/extend.md#extending-a-selector

### Phase 2

This phase will emit errors as described in the body of the proposal.
