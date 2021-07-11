# Floating Point Numbers: Draft 1.0

*[(Issue)](https://github.com/sass/sass/issues/2892)*

This proposal formally defines all numbers in Sass to be double-precision or 64-bit floating point numbers.

## Table of Contents

- [Floating Point Numbers: Draft 1.0](#floating-point-numbers-draft-10)
  - [Table of Contents](#table-of-contents)
  - [Background](#background)
  - [Summary]()
  - [Semantics](#semantics)
    - [Division by Zero](#division-by-zero)
    - [Equality](#equality)
    - [Ordering](#ordering)
    - [Hashing](#hashing)
    - [Formatting](#formatting)

## Background

Numbers in Sass today are ill-defined. Between implementations there
is very little agreement around edge cases, and there are few spec tests
exercising these cases.

The behavior of `dart-sass` and `libsass`, the two major implementations
at the time of writing, differ greatly with regard to numeric equality, rounding, and overflows.

Additionally, this lack of definition has caused both [regressions](https://github.com/sass/dart-sass/issues/807)
and [crashes](https://github.com/sass/dart-sass/issues/1059) for `dart-sass`.

This document serves to unify these edge cases and resolve existing issues within implementations.

## Semantics

This proposal formally defines all numbers in Sass to be double-precision or 64-bit floating point numbers.

More specifically, numbers are represented as [IEEE 754-2008 "binary64" floating point numbers](https://web.archive.org/web/20190820164556/http://www.dsc.ufcg.edu.br/~cnum/modulos/Modulo2/IEEE754_2008.pdf).

The semantics enumerated here serve to more succintly describe their behavior and
to highlight Sass-specific behavior.

### Division by Zero

Any positive number divided by zero will return `Infinity`

Zero divided by zero will return `NaN`

Any negative number divided by zero will return `-Infinity`

### Equality

Numbers in Sass are symmetrically and transitively equal. That is,
if `a == b`, then `b == a` and if `a == b` and `b == c`, then `a == c`.

Numbers in Sass are *not* necessarily reflexively equal. `NaN == NaN`,
`Infinity == Infinity`, and `-Infinity == -Infinity` will always be false.

In the case that two numbers differ only in their 11th or later digits after
the decimal point, both numbers are rounded to 10 decimal places and compared.

> For example,
> 
> These two numbers differ *before* the 10th decimal place, so they should be considered
> unequal
> 1.0000000010 == 1.0000000020
>   ---------^      ---------^
> 
> These two numbers differ *at* the 10th decimal place, so they should be considered
> unequal
> 1.0000000001 == 1.0000000002
>   ---------^      ---------^
> 
> These two numbers differ only *beyond* the 10th decimal place, so they should be considered
> equal
> 1.00000000001 == 1.00000000002
>   ---------^       ---------^

### Ordering

Numbers in Sass do not have total ordering. Any comparison to `NaN` will return false.
Otherwise, ordering follows what would be expected when comparing two numbers.

Numbers with more than 10 decimal places are rounded to 10 places before comparison.

### Hashing

The non-reflexive nature of number equality in Sass makes it difficult to
use floating point numbers as keys inside maps. This property means
that `hash(a)` doesn't necessarily equal `hash(b)` even if `a == b`.

Despite this, it is desirable for users to be able to use numbers as map keys,
and dropping support for numbers as map keys would break backwards compatibility.

In order to support numbers as map keys, a Sass implementation must raise an error when
trying to construct a map with, insert into (`map-insert`), retrieve (`map-get`) or remove (`map-remove`)
from a map any of `NaN`, `Infinity`, or `-Infinity`.

### Formatting

In order to emit a number:
 - Round the number to 10 decimal places
 - Remove trailing zeros
 - If the number is negative and non-zero, emit a negative sign, `-`
 - Emit the whole number
 - If the decimal portion is non-zero, emit the decimal portion

`NaN`, `Infinity`, and `-Infinity` are emitted as written.

| As written in Sass | As emitted in expanded mode |
| ------------------ | --------------------------- |
| 0                  | 0                           |
| 0.0                | 0                           |
| -0                 | 0                           |
| -0.0               | 0                           |
| 1                  | 1                           |
| 1.0                | 1                           |
| -1                 | -1                          |
| -1.0               | -1                          |
| 0.1                | 0.1                         |
| -0.1               | -0.1                        |
| .1                 | 0.1                         |
| -.1                | -0.1                        |
| 1.1                | 1.1                         |
| -1.1               | -1.1                        |
| NaN                | NaN                         |
| Infinity           | Infinity                    |
| -Infinity          | -Infinity                   |
