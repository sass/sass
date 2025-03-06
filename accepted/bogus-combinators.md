# Bogus Combinators: Draft 4

*([Issue](https://github.com/sass/sass/issues/3340), [Changelog](bogus-combinators.changes.md))*

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
  * [`ComplexSelector`](#complexselector)
  * [`PseudoSelector`](#pseudoselector)
  * [`ExtendRule`](#extendrule)
* [Semantics](#semantics)
  * [Evaluating a Style Rule](#evaluating-a-style-rule)
  * [Executing an Extend Rule](#executing-an-extend-rule)
* [Functions](#functions)
  * [`selector.is-superselector()`](#selectoris-superselector)
  * [`selector.extend()`, `selector.replace()`, and `selector.unify()`](#selectorextend-selectorreplace-and-selectorunify)
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

[combinator]: https://drafts.csswg.org/selectors-4/#combinators
[descendant combinator]: https://drafts.csswg.org/selectors-4/#descendant-combinators

### Complex Selector

~~A *complex selector* is a sequence of [visible combinators] (its *leading
combinators*) as well as a sequence of [complex selector components]. Either,
but not both, of these sequences may be empty~~

[visible combinators]: #visible-combinator
[complex selector components]: #complex-selector-component

A *complex selector* is an optional [visible combinator] (its *leading
combinator*) as well as a sequence of [complex selector components]. The
component sequence may be empty only for complex selectors with leading
combinators.

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

A [complex selector] is *bogus* if it has a leading or [trailing combinator].

A selector list is *bogus* if any of its complex selectors are bogus.

[trailing combinator]: #trailing-combinator

## Syntax

### `ComplexSelector`

> Note that the existing productions being modified have not been defined
> explicitly before this document. The old productions are listed in
> strikethrough mode to clarify the change.

This proposal modifies the existing `ComplexSelector` and
`ComplexSelectorComponent` productions to drop support for multiple combinators:

<x><pre>
~~**ComplexSelector**          ::= [\<combinator>]\* ComplexSelectorComponent+~~
~~&#32;                          | [\<combinator>]+~~
~~**ComplexSelectorComponent** ::= CompoundSelector [\<combinator>]\*~~
**ComplexSelector**          ::= [\<combinator>]? ComplexSelectorComponent+
&#32;                          | [\<combinator>]
**ComplexSelectorComponent** ::= CompoundSelector [\<combinator>]?
</pre></x>

[\<combinator>]: https://drafts.csswg.org/selectors-4/#typedef-combinator

### `PseudoSelector`

This proposal adds the following annotation to [the `SelectorPseudo` and
`NthSelectorPseudo` productions]:

[the `SelectorPseudo` and `NthSelectorPseudo` productions]: ../spec/syntax.md#pseudoselector

None of the `ComplexSelector`s in the `Selector` production may end with a
[`<combinator>`]. None of them may begin with a [`<combinator>`] either, except
for a `SelectorPseudo` whose `SelectorPseudoName` is case-insensitively equal to
":has".

[`<combinator>`]: https://drafts.csswg.org/selectors-4/#typedef-combinator

### `ExtendRule`

This proposal adds the following annotation to the `ExtendRule` production:

None of the `ComplexSelector`s in the `Selector` production may be [bogus].

## Semantics

### Evaluating a Style Rule

This proposal adds the following to [Evaluating a Style Rule], after executing
each child of `rule`:

[Evaluating a Style Rule]: ../spec/style-rules.md#semantics

* If `css` contains any children and `selector` is [bogus], throw an error.

[bogus]: #bogus-selector

### Executing an Extend Rule

This proposal adds the following to [Executing an Extend Rule], after checking
for a current style rule:

[Executing an Extend Rule]: ../spec/at-rules/extend.md#executing-an-extend-rule

* If the current style rule is [bogus], throw an error.

## Functions

> `selector.append()`, `selector.nest()`, and `selector.parse()` are still
> allowed to take bogus selectors because these functions are syntactic rather
> than semantic. This means on one hand that there aren't ambiguities about how
> to handle bogus selector inputs, and on the other that it may be useful to
> emit bogus selectors for later use in nesting contexts.
>
> Note that `selector.append()` already forbids selectors with leading or
> trailing combinators from being passed in between selectors.

### `selector.is-superselector()`

After parsing the selector arguments, throw an error if any of the parsed
selectors are [bogus].

### `selector.extend()`, `selector.replace()`, and `selector.unify()`

After parsing the selector arguments, throw an argument if the `$selector`
argument has a [trailing combinator], or if any other parsed selector is
[bogus].

> We allow selectors with leading combinators to be extended because they can
> appear in a nested context in plain CSS.

## Deprecation Process

The deprecation will be divided into two phases:

### Phase 1

This phase will only change behavior that doesn't affect in-browser rendering.
In particular:

* The parsing of `ComplexSelector` and `ComplexSelectorComponent` is unchanged.

* A complex selector is instead considered [bogus] if it would be bogus in Phase
  2 *or* if it can be parsed in Phase 1 but not in Phase 2.

* The newly-added errors and forbidden syntax produces deprecation warnings
  instead.

* In [Evaluating a Style Rule], remove any complex selectors from `css`'s
  selectors that are [bogus], except those that have a single leading combinator
  but are otherwise not bogus.

  > Leading combinators are allowed in Phase 1 (but still emit deprecation
  > warnings) because they may be used for nesting along with `meta.load-css()`.

* Define a "useless" selector as:

  * A complex selector that has multiple combinators.

  * A bogus pseudo selector.

  * Any selector that contains a useless selector.

  In [Extending a Selector], treat useless selectors as selectors that can match
  no elements.

  [Extending a Selector]: ../spec/at-rules/extend.md#extending-a-selector

### Phase 2

This phase will emit errors as described in the body of the proposal.
