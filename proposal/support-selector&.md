# Support `selector&`: Draft 1

[Issue](https://github.com/sass/sass/issues/1425)

## Background

During my transitioning from Stylus, everything went fine until I placed a `&` in the beginning of a nested selector.

`"&" may only used at the beginning of a compound selector.` 

Oh no. 


While it might not be the best of practices, there are cases where it makes sense and/or just helps the transition to sass.
A quick search lead me to many different issues, before finding the one linked above. 


There's a work-around ([with some limitations](https://github.com/sass/sass/issues/1425#issuecomment-404462836)), 
but it would be nice to prevent other developers from experiencing the same struggle.


## Summary

Sass will support `selector&` expressions, making the last example here (copied from a [comment in the issue](https://github.com/sass/sass/issues/1425#issuecomment-405921429)) work:

```
& selector { }  // works
selector & { }  // works
&selector { }   // works
selector& { }   // doesn't work
```
