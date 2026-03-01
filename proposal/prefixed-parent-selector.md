# Prefixed Parent Selector: Draft 3
[Issue](https://github.com/sass/sass/issues/1425)

## Background

The parent selector(&) provides flexibility to create rules according to the
parent classes and simplify wordy expressions to a single line.

It currently supports the variations below:
```
& selector { }
selector & { }
&selector { }
```
However, one interesting way to use the `&` selector is not supported in sass:
```
selector& {}
```

A specific use case for this selector is the following:

```
.custom-effect {
  /* common styles */
}
a.custom-effect {
  /* styles for <a> element */
}
button.custom-effect {
  /* styles for <button> element */
}
```

Using the the prefixed parent selector
```
.custom-effect {

  // common styles

  a& {
    // styles for <a> element
  }

  button& {
    // styles for <button> element
  }
}
```

Similar functionality is currently supported, but only by using the following verbose syntax:
```
.custom-effect {

  // common styles

  @at-root #{selector-unify(&, "a")} {
    // styles for <a> element
  }

  @at-root #{selector-unify(&, "button")} {
    // styles for <button> element
  }
}
```

Another [example](https://github.com/sass/sass/issues/1425#issuecomment-212077907) is included as a comment in the issue about this feature.

This allows for grouping the parent styling based on the different base selectors, while keeping the styling nicely nested within the rest of the ruleset. It is an elegant solution which is not possible without using a combination of built-in sass functions.
([Current workaround](https://github.com/sass/sass/issues/1425#issuecomment-404462836))

Note that this syntax is supported by Less and Stylus, and people have run into the lack of support for this syntax when trying to switch over to Sass:

* [Comment #1](https://github.com/sass/sass/issues/1425#issuecomment-55462526)
* [Comment #2](https://github.com/sass/sass/issues/1425#issuecomment-123913176)
* [Comment #3](https://github.com/sass/sass/issues/1425#issuecomment-123916395)

There has been an attempt to create a proposal for it, but it fell short in addressing review comments and was closed soon after:
[Previous Proposal](https://github.com/sass/sass/pull/2723)

## Summary

The goal of this proposal is to add support for `foo&` with certain exceptions that are defined in the ‘syntax’ section. Allowing this placement of the parent selector will provide the ability to write the following syntax:
```
.foo {
  div& {
    ...
  }
  p& {
    ...
  }
}

// Instead of writing

div.foo {
 ...
}

p.foo {
 ...
}
```

## Syntax

Each compound selector may only contain one parent selector `&`.  The parent selector
may appear by itself, after a `<type-selector>`, or on either side of any `<wq-name>`,
`<subclass-selector>`, `<pseudo-element-selector>` or `<pseudo-class-selector>`.

(In terms of [Selectors Level 4 Grammar](https://www.w3.org/TR/selectors-4/#grammar))

```
CompoundSelector            ::= [ ParentSelector
                                | <type-selector>? <subclass-selector>* PseudoSelectors*
                                | TypeSelectorWithParent <subclass-selector>* PseudoSelectors*
                                | <type-selector>? SubclassSelectorsWithParent PseudoSelectors*
                                | <type-selector>? <subclass-selector>*
                                  PseudoSelectorsWithParent ]!
TypeSelectorWithParent      ::= ParentSelector <wq-name> | <type-selector> ParentSelector
SubclassSelectorsWithParent ::= ParentSelector <subclass-selector>+
                              | <subclass-selector>+ ParentSelector <subclass-selector>*
PseudoSelectorsWithParent   ::= ParentSelector PseudoSelectors+
                              | PseudoSelectors+ ParentSelector PseudoSelectors*
PseudoSelectors             ::= <pseudo-element-selector> <pseudo-class-selector>*
ParentSelector              ::= '&'
```

## Semantics

The following modifies the [Style Rules Semantics](https://github.com/sass/sass/blob/master/spec/style-rules.md#semantics):

To execute a style rule `rule`:

* Let `selector` be the result of evaluating all interpolation in `rule`'s
  selector and parsing the result as a selector list.

* If there is a current style rule:

  * If `selector` one or more parent selectors, replace each parent selector with
    the current style rule's selector:

    * If the result of the replacement(s) is not a syntactically valid
      [complex selector][], throw an error.

      [complex selector]: https://www.w3.org/TR/selectors-4/#typedef-complex-selector

    * Otherwise set `selector` to the result of the replacement.

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

  [the current module]: ../spec/spec.md#current-module
