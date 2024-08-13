# Style Rules

## Table of Contents

* [Definitions](#definitions)
  * [Current Style Rule](#current-style-rule)
  * [Current Keyframe Block](#current-keyframe-block)
* [Semantics](#semantics)

## Definitions

### Current Style Rule

The *current style rule* is the CSS style rule that was created by the innermost
[execution of a style rule](#semantics), `@media` rule, `@supports` rule, or
unknown at-rule. This may be overridden by the [execution of a declaration].

[execution of a declaration]: declarations.md#semantics

### Current Keyframe Block

The *current keyframe block* is the CSS keyframe block that was created by the
innermost [execution of a style rule](#semantics). This may be overridden by the
[execution of a declaration].

## Semantics

To execute a style rule `rule`:

* Let `selector-text` be the result of evaluating all interpolation in `rule`'s
  selector.

* Let `parent` be the [current style rule], at-rule, or keyframe block if one
  exists, or the innermost if multiple exist.

  [current style rule]: #current-style-rule

* If `parent` is a keyframe block, throw an error.

* Otherwise, if `parent` is an unknown at-rule whose name without vendor
  prefixes is "keyframes":

  * Let `selector` be the result of parsing `selector-text` as a keyframe
    selector.

  * Append a keyframe block with selector `selector` to `parent`.

  * Evaluate each child of `rule`.

  * Cease evaluating `rule`.

* Otherwise, if `parent` is a style rule whose stylesheet wasn't [parsed as
  CSS]:

  [parsed as CSS]: syntax.md#parsing-text-as-css

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

* If `parent` is set and its stylesheet was [parsed as CSS], append `css` to
  `parent`.

* Otherwise, if there is a current at-rule, append `css` to its children.

* Otherwise, append `css` to [the current module]'s CSS.

  [the current module]: spec.md#current-module

* Execute each child of `rule`.

* If `css` contains any children and `selector` is [bogus], throw an error.

  [bogus]: selectors.md#bogus-selector

* Otherwise, if `css` contains no children, remove it from the current module's
  CSS.
