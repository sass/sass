# Plain CSS `clamp()`: Draft 1

*([Issue](https://github.com/sass/sass/issues/2860))*

This proposal defines how Sass handles CSS's `clamp()` [math function][], by
updating the definition of a [special number string][special] in Sass.

[math function]: https://drafts.csswg.org/css-values/#comp-func

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Definitions](#definitions)
  * [Special Number String](#special-number-string)

## Background

> This section is non-normative.

[CSS Values and Units Module Level 4][values] defines a CSS `clamp()` function
which accepts three calculations: a minimum value, a central value, and a
maximum value. All three values are treated like independant `calc()` functions
before applying the min and max "clamps" to the result:

```css
html {
  font-size: clamp(1em, 1em + (2vw + 2vh) / 2, 2em + 1vmin);
}
```

This plain CSS `clamp()` function has shipped in Chrome, Edge, and Firefox.
However, the example above currently causes an error in Sass, because the units
are incompatable. Sass attempts to ressolve any math expressions (including
those inside the `clamp()` function) as part of compilation. That makes it
impossible to use the CSS `clamp()` function as intended.

In order to properly support the CSS `clamp()` function, Sass must treat
`clamp()` as a [special number string][special], similar to `calc()`.

[special]: ../spec/functions.md#special-number-string

Sass already provides a complimentary `clamp()` function in the `sass:math`
module, which is designed specifically for values that can be resolved at
compile time. Since the existing function is scoped to a Sass module, it does
not cause any conflicts with the CSS function.

## Summary

> This section is non-normative.

This proposal makes it possible to use the CSS `clamp()` function as defined in
CSS, without any interference or processing as SassScript. Sass will not attempt
to resolve math inside the CSS `clamp()` function, and will not error when
internal math involves incompatible units. CSS `clamp()` syntax will also be
allowed in SassScript functions that shadow CSS functions, such as `min()`,
`max()`, `rgb()`, and other color functions.

The existing `clamp()` function inside the `sass:math` module will not be
affected by this change.

## Definitions

> This proposal updates the definition of a "special number string" to include
> `clamp(` as a prefix. The new defenition reads as follows:

### Special Number String

A *special number string* is an unquoted string that CSS will recognize as a
function that may return a number. For the purposes of Sass, this is any
unquoted string that begins with `calc(`, `var(`, `env(`, `min(`, `max(`, or
`clamp(`. This matching is case-insensitive.

> Sass functions that shadow CSS functions must work with any invocation that
> CSS allows, which includes allowing special number strings anywhere a number
> would be allowed.
