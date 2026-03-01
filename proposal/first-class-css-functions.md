# First-Class CSS Functions

*([Issue](https://github.com/sass/sass/issues/3176))*

## Table of Contents

- [First-Class CSS Functions](#first-class-css-functions)
  - [Table of Contents](#table-of-contents)
  - [Background](#background)
  - [Summary](#summary)
  - [Syntax](#syntax)
  - [Types](#types)
    - [Serialization](#serialization)
      - [CssFunction](#cssfunction)
  - [Semantics](#semantics)
  - [Functions](#functions)
    - [`meta.type-of()`](#metatype-of)
    - [`meta.fn-name()`](#metafn-name)
    - [`meta.fn-args()`](#metafn-args)

## Background

> This section is non-normative.

Sass currently treats non-[calculation] [CSS Functions] as fully opaque,
allowing almost any sequence of tokens within the parentheses and evaluating it
to an unquoted string. Once a function is used it can't be inspected or
manipulated in any way.

Notice that interpolation in CSS Functions is already supported and therefore
not included in this proposal:

```scss
$foo: 1px;
width: var(--foo, $foo); // Output: width: var(--foo, 1px);
```

[calculation]: https://sass-lang.com/documentation/values/calculations
[CSS Functions]: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Functions

## Summary

> This section is non-normative.

This proposal changes all CSS Functions from being parsed as unquoted strings to
being parsed in-depth and producing a new data type known as a "css-function".
This data type holds the name and arguments of the function.

For example, given this block of code:

```scss
$namespace: 'my-pkg';

$foo: var(--foo);
$bar: var(--bar, 8px);
$baz: var(--#{$namespace}-baz, 8px);
$rotation: rotate(0.5turn);
```

- `meta.type-of($foo)` will return `css-function`
- `meta.fn-name($foo)` will return `"var"`
- `meta.fn-args($foo)` will return `unquote("--foo")`
- `meta.fn-args($bar)` will return `unquote("--bar"), 8px`
- `meta.fn-args($baz)` will return `unquote("--my-pkg-baz"), 8px`
- `meta.fn-name($rotation)` will return `"rotate"`
- `meta.fn-args($rotation)` will return `0.5turn`

Note that if an argument is a number or a nested calculation, it’s returned as
that type. Otherwise, it’s returned as an unquoted string.

## Syntax

## Types

This proposal introduces a new value type known as a "css-function", with the
following structure:

```ts
interface CssFunction {
  name: string;
  arguments: CssFunctionArgument[];
}

type CssFunctionArgument =
  | Number
  | UnquotedString
  | Calculation;
```

### Serialization

#### CssFunction

To serialize a CSS Function, emit its name followed by "(", then each of its
arguments separated by ",", then ")".

## Semantics

## Functions

### `meta.type-of()`

Add the following clause to the [`meta.type-of()`] function and the top-level
`type-of()` function:

[`meta.type-of()`]: ../spec/built-in-modules/meta.md#type-of

* If `$value` is a CSS Function, return an unquoted string with value
  `"css-function"`.

### `meta.fn-name()`

This is a new function in the `sass:meta` module.

```
meta.fn-name($fn)
```

* If `$fn` is not a CSS Function, throw an error.

* Return `$fn`'s name as a quoted string.

### `meta.fn-args()`

This is a new function in the `sass:meta` module.

```
meta.fn-args($fn)
```

* If `$fn` is not a CSS Function, throw an error.

* Let `args` be an empty list.

* For each argument `arg` in `$fn`'s arguments:

  * If `arg` is a number or a calculation, add it to `args`.

  * Otherwise, [serialize](#serialization) `arg` and add the result to `args` as
    an unquoted string.

* Return `args` as an unbracketed comma-separated list.
