# Floating Point Numbers: Draft 1.0

*[(Issue)](https://github.com/sass/sass/issues/2892)*

This proposal formally defines all numbers in Sass to be double-precision or 64-bit floating point numbers.

## Table of Contents

- [Floating Point Numbers: Draft 1.0](#floating-point-numbers-draft-10)
  - [Table of Contents](#table-of-contents)
  - [Background](#background)
  - [Division by Zero](#division-by-zero)
  - [Equality](#equality)
  - [Ordering](#ordering)
  - [Hashing](#hashing)
  - [Formatting](#formatting)

## Background

Numbers in Sass today are ill-defined. Between implementations there
is very little agreement around edge cases, and there are few spec tests
exercising these cases.

The behavior of `dart-sass` and `libsass`, the two officially "blessed" implementations
at the time of writing, differ greatly with regard to numeric equality, rounding, and overflows.

Additionally, this lack of definition has caused both [regressions](https://github.com/sass/dart-sass/issues/807)
and [crashes](https://github.com/sass/dart-sass/issues/1059) for `dart-sass`.

This document serves to unify these edge cases and resolve existing issues within implementations.

## Division by Zero

Any positive number divided by zero will return `Infinity`

Zero divided by zero will return `NaN`

Any negative number divided by zero will return `-Infinity`

## Equality

Numbers in Sass are symmetrically and transitively equal. That is,
if `a == b`, then `b == a` and if `a == b` and `b == c`, then `a == c`.

Numbers in Sass are *not* necessarily reflexively equal. `NaN == NaN`,
`Infinity == Infinity`, and `-Infinity == -Infinity` will always be false.

In the case that two real numbers use more than 10 decimal places of precision,
both numbers are truncated to 10 decimal places and compared.

## Ordering

Numbers in Sass do not have total ordering. Any comparison to `NaN` will return false.
Otherwise, ordering follows what would be expected when comparing two real numbers.

Real numbers with more than 10 decimal places are truncated to 10 places before comparison.

## Hashing

The non-reflexive nature of number equality in Sass makes it difficult to
use floating point numbers as keys inside maps. This property means
that `hash(a)` does not necessarily equal `hash(a)`.

Despite this, it is desirable for users to be able to use numbers as map keys,
and dropping support for numbers as map keys would break backwards compatibility.

In order to support numbers as map keys, a Sass implementation must
 - deny `NaN`, `Infinity`, and `-Infinity` as keys
 - truncate all numbers to 10 decimal places before insertion

## Formatting

In order to emit a real number:
 - Round the number to 10 decimal places
 - Remove trailing zeros
 - If the number is negative and non-zero, emit a negative sign, `-`
 - If in compressed mode:
   - If the whole number is zero, do nothing
   - Otherwise, emit the whole number
 - Otherwise, if in expanded mode:
   - Emit the whole number
 - If the decimal portion is non-zero, emit the decimal portion

`NaN`, `Infinity`, and `-Infinity` are emitted as written.

| As written in Sass | As emitted in expanded mode | As emitted in compressed mode |
| ------------------ | --------------------------- | ----------------------------- |
| 0                  | 0                           | 0                             |
| 0.0                | 0                           | 0                             |
| -0                 | 0                           | 0                             |
| -0.0               | 0                           | 0                             |
| 1                  | 1                           | 1                             |
| 1.0                | 1                           | 1                             |
| -1                 | -1                          | -1                            |
| -1.0               | -1                          | -1                            |
| 0.1                | 0.1                         | .1                            |
| -0.1               | -0.1                        | -.1                           |
| .1                 | 0.1                         | .1                            |
| -.1                | -0.1                        | -.1                           |
| 1.1                | 1.1                         | 1.1                           |
| -1.1               | -1.1                        | -1.1                          |
| NaN                | NaN                         | NaN                           |
| Infinity           | Infinity                    | Infinity                      |
| -Infinity          | -Infinity                   | -Infinity                     |
