# Functions

## Table of Contents

* [Types](#types)
  * [Operations](#operations)
    * [Equality](#equality)
  * [Serialization](#serialization)

## Types

The value type known as a "function" is a procedure that takes an
`ArgumentInvocation` `args` and returns a SassScript value. Each function has a
string name.

> The specific details of executing this procedure differ depending on where and
> how the function is defined.

### Operations

A function follows the default behavior of all SassScript operations, except
that equality is defined as below.

#### Equality

Functions use reference equality: two function values are equal only if they
refer to the exact same instance of the same procedure.

> If the same file were to be imported multiple times, Sass would create a new
> function value for each `@function` rule each time the file is imported.
> Because a new function value has been created, although the name, body, and
> source span of a given function from the file would be the same between
> imports, the values would not be equal because they refer to different
> instances. Functions pre-defined by the Sass language are instatiated at most
> once during the entire evaluation of a program.
>
> As an example, if we declare two functions:
>
> ```scss
> @function foo() {
>   @return red;
> }
>
> $a: meta.get-function(foo);
>
> @mixin foo {
>   @return red;
> }
>
> $b: meta.get-mixin(foo);
> ```
>
> Although every aspect of the two functions is the same, `$a != $b`, because
> they refer to separate function values.

### Serialization

To serialize a function value:

* If the value is not being inspected, throw an error.

* Otherwise, emit `'get-function("'`, then the function's name, then `'")'`.
