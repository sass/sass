# Prefixed Parent Selector: Draft 2

[Issue](https://github.com/sass/sass/issues/1425)

## Background

When transitioning from Less or Stylus, or just learning Sass, nesting is common.

Everything works just as expected until a `&` is in the beginning of a nested selector.

`"&" may only used at the beginning of a compound selector.` 

Considering that there's been quite a few issues regarding this, and that they keep comming up, 
and that people expect this to work, I think this would be a valuable addition to Sass.

There's a work-around ([with some limitations](https://github.com/sass/sass/issues/1425#issuecomment-404462836)),
but it's subpar. It might scare of beginners and/or cause unnecessary headache.


## Summary

Prefixed parent selectors will work as expected, solving the frustration of current, and new, users of Sass.

@arkonan's [example](https://github.com/sass/sass/issues/1425#issuecomment-405921429) neatly summarises this: 
```
& selector { }  // works
selector & { }  // works
&selector { }   // works
selector& { }   // doesn't work
```



## Syntax


expect
```
.title {
  h1& {
    font-weight: 600;
  }
}
```

to become

```
h1.title {
  font-weight: 600;
}
```

instead of this error

```
`"&" may only used at the beginning of a compound selector.` 
```
