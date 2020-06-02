# Prefixed Parent Selector: Draft 2
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

## Syntax
(In terms of [Selectors Level 4 Grammar](https://www.w3.org/TR/selectors-4/#grammar))

```
SelectorList         ::= ComplexSelectorList#
ComplexSelectorList  ::= ComplexSelector#
CompoundSelectorList ::= CompoundSelector#
SimpleSelectorList   ::= SimpleSelector#
RelativeSelectorList ::= RelativeSelector#
ComplexSelector      ::= CompoundSelector [ <combinator>? CompoundSelector ]*
RelativeSelector     ::= <combinator>? ComplexSelector
CompoundSelector     ::= ParentSelector? <compound-selector>
                         | <compound-selector> ParentSelector
                         | ParentSelector
SimpleSelector       ::= ParentSelector? <simple-selector>
                         | <simple-selector> ParentSelector
                         | ParentSelector
ParentSelector       ::= '&'
```

Note that it is not necessary to support the following syntax:
```
.custom-effect {
  p&:focus {
    // styles for <p> when focused
  }
}
```

Because the same effect can be achieved with one extra level of nesting:
```
.custom-effect {
  p& {
    &:focus {
      // styles for <p> when focused
    }
  }
}
```

## Semantics

The following modifies the [Style Rules Semantics](https://github.com/sass/sass/blob/master/spec/style-rules.md#semantics):

To execute a style rule `rule`:

* Let `selector` be the result of evaluating all interpolation in `rule`'s
  selector and parsing the result as a selector list.

* If there is a current style rule:

  * If `selector` contains one or more `ParentSelector`:

      * If multiple `ParentSelector` appear in the a single `SimpleSelector` or `CompoundSelector`,
        throw an error.

      * Otherwise, replace each `ParentSelector` with the current style rule's
        selector and set `selector` to the result.

  * Otherwise, nest `selector` within the current style rule's selector using
	the [descendant combinator][] and set `selector` to the result.

  [descendant combinator]: https://www.w3.org/TR/selectors-3/#descendant-combinators

* Otherwise, if `selector` contains one or more `ParentSelector`, throw an
  error.

* Let `css` be a CSS style rule with selector `selector`.

* Execute each child `child` of `rule`.

* Remove any [complex selectors][] containing a placeholder selector that
  begins with `-` or `_` from `css`'s selector.

  [complex selectors]: https://drafts.csswg.org/selectors-4/#complex

* Unless `css`'s selector is now empty, append `css` to [the current module][]'s
  CSS.
