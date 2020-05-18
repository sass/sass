# Prefixed Parent Selector: Draft 1
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

Note that this syntax is supported by Less and Stylus, and people have run into the lack of support for this syntax when trying to switch over to Sass.
[Comment #1](https://github.com/sass/sass/issues/1425#issuecomment-55462526)
[Comment #2](https://github.com/sass/sass/issues/1425#issuecomment-123913176)
[Comment #3](https://github.com/sass/sass/issues/1425#issuecomment-123916395)

There has been an attempt to create a proposal for it, but it fell short in addressing review comments and was closed soon after.
[Previous Proposal](https://github.com/sass/sass/pull/2723)

## Semantics

The [Style Rules Semantics](https://github.com/sass/sass/blob/master/spec/style-rules.md#semantics) describes the parent selector behavior as follows:

> If `selector` contains one or more parent selectors, replace them with the current style rule's selector and set `selector` to the result.

###Proposal #1:

The current [Style Rules Semantics](https://github.com/sass/sass/blob/master/spec/style-rules.md#semantics) allows parent selectors anywhere in a `selector` and does not explain why it currently errors for prefixed parent selectors. If the prefixed parent selector is supported, the semantics section for Style Rules would not need to be changed.

###Proposal #2:

The [Style Rules Semantics](https://github.com/sass/sass/blob/master/spec/style-rules.md#semantics) can explicitly allow the syntax in its description of the parent selector behavior:

> If the selector contains one or more parent selectors, replace them with the current style's rule selector and set selector to the result. The parent selector is allowed to be prefixed or suffixed onto an existing identifier, or appear by itself.

(Please provide feedback about which proposal is more suitable for this change.)
