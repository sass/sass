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

* If there is a [current style rule](#current-style-rule):

  * If `selector` contains one or more parent selectors, replace them with the
    current style rule's selector and set `selector` to the result.

  * Otherwise, nest `selector` within the current style rule's selector using
    the [descendant combinator][] and set `selector` to the result.

  [descendant combinator]: https://www.w3.org/TR/selectors-3/#descendant-combinators

* Otherwise, if `selector` contains one or more parent selectors, throw an
  error.

* Let `css` be a CSS style rule with selector `selector`.

* Execute each child `child` of `rule`.

* Remove any [complex selectors][] containing a placeholder selector that
  begins with `-` or `_` from `css`'s selector.
  
  [complex selectors]: https://drafts.csswg.org/selectors-4/#complex

* Unless `css`'s selector is now empty, append `css` to [the current module][]'s
  CSS.

  [the current module]: spec.md#current-module
