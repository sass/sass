# Selectors

## Table of Contents

* [Definitions](#definitions)
  * [Visible Combinator](#visible-combinator)
  * [Complex Selector](#complex-selector)
  * [Complex Selector Component](#complex-selector-component)
  * [Trailing Combinator](#trailing-combinator)
  * [Bogus Selector](#bogus-selector)
* [Syntax](#syntax)
  * [`ComplexSelector`](#complexselector)

## Definitions

### Visible Combinator

A *visible combinator* is any selector [combinator] other than the [descendant
combinator].

[combinator]: https://drafts.csswg.org/selectors-4/#combinators
[descendant combinator]: https://drafts.csswg.org/selectors-4/#descendant-combinators

### Complex Selector

A *complex selector* is an optional [visible combinator] (its *leading
combinator*) as well as a sequence of [complex selector components]. The
component sequence may be empty only for complex selectors with leading
combinators.

[visible combinator]: #visible-combinator 
[complex selector components]: #complex-selector-component

### Complex Selector Component

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
with a bogus selector, except that `:has()` may contain complex selectors with
leading combinators.

A selector list is *bogus* if any of its complex selectors are bogus.

[trailing combinator]: #trailing-combinator

## Syntax

### `ComplexSelector`

<x><pre>
**ComplexSelector**          ::= [\<combinator>]? ComplexSelectorComponent+
&#32;                          | [\<combinator>]
**ComplexSelectorComponent** ::= CompoundSelector [\<combinator>]?
</pre></x>

[\<combinator>]: https://drafts.csswg.org/selectors-4/#typedef-combinator
