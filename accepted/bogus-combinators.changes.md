## Draft 3

* In Phase 1:

  * Clarify the definition of bogus selectors.

  * Only omit style rules if *all* of their complex selectors are bogus.

  * Expand the set of selectors that are treated by the extend algorithm as
    matching nothing to include bogus pseudo selectors, since these can never be
    transformed into anything useful.

* In Phase 2:

  * Don't throw an error for style rules that contain no children, since this
    would break nesting.

  * Throw an error for an `@extend` rule with a bogus extender.

  * Support single leading combinators.

  * Support complex selectors composed of only a single combinator.

  * Allow bogus selectors in `selector.append()`.

## Draft 2

* Allow leading combinators in `:has()`.

* Replace our custom `Combinator` production with the CSS spec's `<combinator>`.

## Draft 1

* Initial draft.
