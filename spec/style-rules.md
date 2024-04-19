# Style Rules

## Table of Contents

* [Definitions](#definitions)
  * [Current Style Rule](#current-style-rule)
* [Semantics](#semantics)

## Definitions

### Current Style Rule

The *current style rule* is the CSS style rule that was created by the innermost
[execution of a style rule](#semantics).

## Semantics

To execute a style rule `rule`:

* Let `selector` be the result of evaluating all interpolation in `rule`'s
  selector and parsing the result as a selector list.

* Let `parent` by the [current style rule] or current at-rule if one exists, or
  the innermost if both exist.

  [current style rule]: #current-style-rule

* If `parent` is a style rule whose stylesheet wasn't [parsed as CSS]:

  [parsed as CSS]: #parsing-text-as-css

  > Checking whether `rule`'s stylesheet is CSS ensures that the plain CSS
  > behavior occurs even when plain CSS is evaluated in a Sass context, such as
  > through a nested `@import` or a `meta.load-css()` call.

  * If `selector` contains one or more parent selectors and `rule`'s stylesheet
    wasn't [parsed as CSS], replace those parent selectors with the current
    style rule's selector and set `selector` to the result.

  * Otherwise, nest `selector` within the current style rule's selector using
    the [descendant combinator] and set `selector` to the result.

  [descendant combinator]: https://www.w3.org/TR/selectors-3/#descendant-combinators

* Otherwise, if `selector` contains one or more parent selectors and `rule`'s
  stylesheet wasn't [parsed as CSS], throw an error.

* Let `css` be a CSS style rule with selector `selector`.

* Execute each child `child` of `rule`.

* If `css` contains any children and `selector` is [bogus], throw an error.

  [bogus]: selectors.md#bogus-selector

* Remove any [complex selectors][] containing a placeholder selector that
  begins with `-` or `_` from `css`'s selector.
  
  [complex selectors]: https://drafts.csswg.org/selectors-4/#complex

* Unless `css`'s selector is now empty:
  
  * If `parent` is set and its stylesheet was [parsed as CSS], append `css` to
    `parent`

  * Otherwise, if there is a current at-rule, append `css` to its children.
  
    > This was intended to be in the current spec, but was overlooked.

  * Otherwise, append `css` to [the current module]'s CSS.

  [the current module]: spec.md#current-module
