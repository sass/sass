# Prefixed Parent Selector: Draft 1

[Issue](https://github.com/sass/sass/issues/1425)

## Background

When transitioning from Less or Stylus, or just learning Sass, nesting and using the parent selector `&` is common.

```
.selector {
    & .nested {} // .selector .nested - as expected
    .nested & {} // .nested .selector - as expected
    &.nested {}  // .selector.nested - as expected
    
    .nested& {} // "&" may only used at the beginning of a compound selector. - not as expected. 
                // expected: .nested.selector
}
```

Everything works just as expected until the `&` is in the beginning of a nested selector.

For example, when creating a component that could either be `<a>` or `<button>`, 
you'd like to add some styling for each of the variants.

The imperative way would be: 
```
.component {
  /* common styles */
}

a.component {
  /* styles for <a> element */
}

button.component {
  /* styles for <button> element */
}
```

The declarative and intuitive way to write this in Sass (without looking at the documentation) would be: 
```
.component {
  
  // common styles
  
  a& {
    // styles for <a> element
  }
  
  button& {
    // styles for <button> element
  }
}
```

But that results in `"&" may only used at the beginning of a compound selector.`.


There's a work-around with some limitations (from a [comment in the issue](https://github.com/sass/sass/issues/1425#issuecomment-404462836)):

> Please keep in mind that `@at-root` doesn't always produce the desirable result.
> 
> Consider this:
> 
> ```css-scss
> .wrapper {
>     .field {
>         input.& { ... }
>         select.& { ... }
>     }
> }
> ```
> 
> The suggested workaround becomes:
> 
> ```css-scss
> .wrapper {
>     .field {
>         @at-root input#{&} { ... }
>         @at-root select#{&} { ... }
>     }
> }
> ```
> 
> Which produces:
> 
> ```css
> input.wrapper .field { ... }
> select.wrapper .field { ... }
> ```
> 
> While I would want it to be:
> 
> ```css
> .wrapper input.field { ... }
> .wrapper select.field { ... }
> ```

The `@at-root input#{&}` is so much more complicated than just `input.&`. It might scare of beginners and/or cause unnecessary headache.


## Summary

Sass with prefixed parent selectors should work intuitively.
Both people new and from other pre-processors expect this to work, and have to resort to searching
and reading the docs for such a simple use case.

Turning this:
```
.selector {
    .nested& {}
}
```
into `.nested.selector`, instead of `"&" may only used at the beginning of a compound selector.`



## Syntax

I have no idea what I'm doing. Help needed.
PrefixedParentSelectorExpression ::= *'$'
