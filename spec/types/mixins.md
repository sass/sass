# Mixins

## Table of Contents

* [Types](#types)
  * [Operations](#operations)
    * [Equality](#equality)
  * [Serialization](#serialization)

## Types

The value type known as a "mixin" is a procedure that takes an [`ArgumentList`]
`args` and returns nothing. Each mixin has a string name and a boolean that
indicates whether or not it accepts a content block.

[`ArgumentList`]: ../syntax.md#argumentlist

> The specific details of executing this procedure differ depending on where and
> how the mixin is defined. A mixin will typically add nodes to the CSS
> stylesheet.

### Operations

A mixin follows the default behavior of all SassScript operations, except that
equality is defined as below.

#### Equality

Mixins use reference equality: two mixin values are equal only if they refer to
the exact same instance of the same procedure.

> If the same file were to be imported multiple times, Sass would create a new
> mixin value for each `@mixin` rule each time the file is imported. Because a
> new mixin value has been created, although the name, body, and source span of
> a given mixin from the file would be the same between imports, the values
> would not be equal because they refer to different instances. Mixins
> pre-defined by the Sass language are instatiated at most once during the
> entire evaluation of a program.
>
> As an example, if we declare two mixins:
>
> ```scss
> @mixin foo {
>   color: red;
> }
>
> $a: meta.get-mixin(foo);
>
> @mixin foo {
>   color: red;
> }
>
> $b: meta.get-mixin(foo);
> ```
>
> Although every aspect of the two mixins is the same, `$a != $b`, because they
> refer to separate mixin values.

### Serialization

To serialize a mixin value:

* If the value is not being inspected, throw an error.

* Otherwise, emit `'get-mixin("'`, then the mixin's name, then `'")'`.
