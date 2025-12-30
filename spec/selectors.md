# Selectors

## Table of Contents

* [Definitions](#definitions)
  * [Visible Combinator](#visible-combinator)
  * [Complex Selector](#complex-selector)
  * [`PseudoSelector`](#pseudoselector)
  * [Complex Selector Component](#complex-selector-component)
  * [Trailing Combinator](#trailing-combinator)
  * [Bogus Selector](#bogus-selector)
* [Syntax](#syntax)
  * [`ComplexSelector`](#complexselector)
* [Serialization](#serialization)
  * [Parent Selector](#parent-selector)

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

### `PseudoSelector`

<x><pre>
**PseudoSelector**          ::= NormalPseudoSelector
&#32;                         | SelectorPseudo
&#32;                         | NthSelectorPseudo
**NormalPseudoSelector**    ::= ':' ':'? VendorPrefix? [\<ident-token>]
&#32;                           ('(' [\<declaration-value>] ')')?
**SelectorPseudo**          ::= SelectorPseudoName '(' Selector ')'
**NthSelectorPseudo**       ::= NthSelectorPseudoName '(' [\<an+b>] 'of'¹ Selector ')'
**SelectorPseudoPrefix**    ::= ':' SelectorPseudoClassName | '::slotted'
**SelectorPseudoClassName** ::= 'not' | 'is' | 'matches' | 'where' | 'any'
&#32;                         | 'current' | 'has' | 'host' | 'host-context'
**NthSelectorPseudoName**   ::= ':' ('nth-child' | 'nth-last-child')
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
[\<declaration-value>]: https://www.w3.org/TR/css-syntax-3/#typedef-declaration-value
[\<an+b>]: https://www.w3.org/TR/css-syntax-3/#the-anb-type

1: The string `of` is matched case-insensitively. In addition, it must be parsed
   as an identifier.

   > In other words, it must have whitespace separating it from other
   > identifiers, so `:nth-child(2nof a)` and `:nth-child(2n ofa)` are both
   > invalid. However, `:nth-child(2of.foo)` is valid.

If a `PseudoSelector` begins with`SelectorPseudoName` or `NthSelectorPseudoName`
followed by a parenthesis, it must be parsed as a `SelectorPseudo` or an
`NthSelectorPseudo` respectively, not as a `NormalPseudoSelector`.

No whitespace is allowed anywhere in a `PseudoSelector` except within
parentheses.

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

## Serialization

### Parent Selector

To serialize a parent selector, emit the character `&`.

> A parent selector can only appear in a serialized selector if it was parsed
> from plain CSS, which doesn't allow it to have a suffix.
