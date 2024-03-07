# Plain CSS Nesting: Draft 1

*([Issue](https://github.com/sass/sass/issues/3524),
[Changelog](plain-css-nesting.changes.md))*

## Table of Contents

# New Cammer.

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [The `&foo` Syntax](#the-foo-syntax)
* [Procedures](#procedures)
  * [Parsing Text as CSS](#parsing-text-as-css)
* [Semantics](#semantics)
  * [Style Rules](#style-rules)
* [Serialization](#serialization)
  * [Parent Selector](#parent-selector)



## Background

> This section is non-normative.

Browsers have recently begun implementing the [CSS Nesting] module, which adds
native support for Sass-like nesting. While we can't yet support this directly
in Sass without causing a colossal set of breaking changes (see [the blog] for
details), we can support it in plain CSS files (distinguished with the `.css`
extension).

[CSS Nesting]: https://www.w3.org/TR/css-nesting-1/
[the blog]: https://sass-lang.com/blog/sass-and-native-nesting

## Summary

> This section is non-normative.

This proposal adds support for parsing nested rules and the parent selector `&`
in plain CSS contexts. This nesting is not resolved in any way; it's passed
through to the output as-is.

### Design Decisions

#### The `&foo` Syntax

This spec does not include support for the `&foo` syntax in plain CSS nesting.
The future of this syntax is [open for debate] and it can be adequately
represented as `foo&`, so Sass won't support it for now.

[open for debate]: https://github.com/w3c/csswg-drafts/issues/8662

## Procedures

### Parsing Text as CSS

This modifies [the existing procedure] for parsing text as CSS.

[the existing procedure]: ../spec/syntax.md#parsing-text-as-css

Adjust the list of productions that should produce errors as follows:

* Remove "A style rule appearing within another style rule".

* Replace "The parent selector `&`, either in a selector or a declaration value"
  with "The parent selector `&` in a declaration value".

* Add "A style rule whose selector contains a trailing combinator."

  > While the [bogus combinators] deprecation is in place, style rules with
  > trailing combinators that *don't* have nested rules will produce warnings.
  > Those with nested rules will produce errors since Sass never parsed them
  > successfully in the first place.

  [bogus combinators]: ../accepted/bogus-combinators.md

Add the following to the list of parsing differences:

* A `ParentSelector` may appear anywhere in a `CompoundSelector`, rather than
  just as the first `SimpleSelector`.

* A `ParentSelector` may not have a `suffix`.

## Semantics

### Style Rules

Replace [the existing semantics for style rules] with:

[the existing semantics for style rules]: ../spec/style-rules.md#semantics

> Differences are highlighted in bold.

To execute a style rule `rule`:

* Let `selector` be the result of evaluating all interpolation in `rule`'s
  selector and parsing the result as a selector list.

* **If `rule`'s stylesheet wasn't [parsed as CSS]**:

  [parsed as CSS]: ../spec/syntax.md#parsing-text-as-css

  > Checking whether `rule`'s stylesheet is CSS ensures that the plain CSS
  > behavior occurs even when plain CSS is evaluated in a Sass context, such as
  > through a nested `@import` or a `meta.load-css()` call.

  * If there is a [current style rule]:

    * If `selector` contains one or more parent selectors, replace them with the
      current style rule's selector and set `selector` to the result.

    * Otherwise, nest `selector` within the current style rule's selector using
      the [descendant combinator] and set `selector` to the result.

  * Otherwise, if `selector` contains one or more parent selectors, throw an
    error.

  [current style rule]: ../spec/style-rules.md#current-style-rule
  [descendant combinator]: https://www.w3.org/TR/selectors-3/#descendant-combinators

* Let `css` be a CSS style rule with selector `selector`.

* Execute each child `child` of `rule`.

* If `css` contains any children and `selector` is [bogus], throw an error.

  [bogus]: ../spec/selectors.md#bogus-selector

* Remove any [complex selectors][] containing a placeholder selector that
  begins with `-` or `_` from `css`'s selector.
  
  [complex selectors]: https://drafts.csswg.org/selectors-4/#complex

* Unless `css`'s selector is now empty:

  * **If `rule`'s stylesheet was [parsed as CSS] and there is a [current style
    rule] or a current at-rule, append `css` to whichever of the two exists, or
    the innermost if both exist.**

  * **If there is a current at-rule, append `css` to its children.**
  
    > This was intended to be in the current spec, but was overlooked.

  * Otherwise, append `css` to [the current module]'s CSS.

  [the current module]: ../spec/spec.md#current-module

## Serialization

### Parent Selector

To serialize a parent selector, emit the character `&`.

> A parent selector can only appear in a serialized selector if it was parsed
> from plain CSS, which doesn't allow it to have a suffix.
