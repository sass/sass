# Adjacent Compound Selectors: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4207))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Syntax](#syntax)
  * [`Options.sourceMapIncludeSources`](#optionssourcemapincludesources)
  * [`Importer`](#importer)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
    * [`Options.sourceMapIncludeSources`](#optionssourcemapincludesources-1)
  * [Phase 2](#phase-2)

## Background

> This section is non-normative.

Sass has historically allowed two [compound selectors] to be written adjacent to
one another, without whitespace or any combinator, in situations where the next
simple selector is not valid as part of the compound (such as `[id]a`). This is
parsed as though they were separated with whitespace, similarly to how
space-separated lists are parsed. However, this is contrary to [the CSS spec],
which states that "A **selector** is a chain of one or more [sequences of simple
selectors] > separated by [combinators]." In addition, the [descendant
combinator] is defined as "[whitespace] that separates two sequences of simple
selectors." This rules out the absence of any characters being parsed as a
descendant combinator.

[compound selectors]: https://developer.mozilla.org/en-US/docs/Web/CSS/Guides/Selectors/Selector_structure#compound_selector
[the CSS spec]: https://www.w3.org/TR/selectors-3/#selector-syntax
[sequences of simple selectors]: https://www.w3.org/TR/selectors-3/#sequence
[combinators]: https://www.w3.org/TR/selectors-3/#combinators
[descendant combinator]: https://www.w3.org/TR/selectors-3/#descendant-combinators
[whitespace]: https://www.w3.org/TR/selectors-3/#whitespace

(Note that the CSS spec has changed its terminology over the years, so what used
to be a "complex selector" is now referred to as a "selector" and what used to
be a "compound selector" is now referred to as a "sequence of simple selectors".
The Sass spec continues to use the older terms to avoid the ambiguity of the
unmarked term "selector" and to match MDN.)

In practice, Gecko and Blink (and likely other engines) reject adjacent compound
selectors such as `[id]a` as invalid, so Sass's behavior here is clearly
incorrect.

## Summary

> This section is non-normative

This proposal makes compound selectors that are not separated by a combinator
(including whitespace) into a syntax error.

## Syntax

### `ComplexSelector`

Replace the definition of `ComplexSelector` with:

<x><pre>
**ComplexSelector** ::= [\<combinator>]¹? (CompoundSelector [\<combinator>])*
&#32;                   CompoundSelector [\<combinator>]¹?
&#32;                 | [\<combinator>]¹
</pre></x>

[\<combinator>]: https://drafts.csswg.org/selectors-4/#typedef-combinator

1: These combinators must be [visible].

[visible]: ../spec/selectors.md#visible-combinator

## Deprecation Process

### Phase 1

In the first phase of deprecation, `ComplexSelector` will be parsed using the
old syntax. However, two adjacent `ComplexSelectorComponent`s not separated by a
[`<combinator>`] will produce a deprecation warning named `adjacent-compounds`.

[`<combinator>`]: https://drafts.csswg.org/selectors-4/#typedef-combinator

### Phase 2

In the next major version after the deprecation goes live, the full changes
above will be implemented.

