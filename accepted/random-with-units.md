# Random With Units: Draft 1

*([Issue](https://github.com/sass/sass/issues/1890))*

This proposal modifies the behavior of the built-in [`math.random()`][random]
function to return a number with matching units to the numeric argument it
received.

[random]: ../spec/built-in-modules/math.md#random

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [New Behavior vs New Syntax](#new-behavior-vs-new-syntax)
    * [No Stripping Units Fallback](#no-stripping-units-fallback)
* [Semantics](#semantics)
* [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

Sass provides a built-in [`math.random()` function][random] which takes an
optional numeric parameter `$limit` (defaults to `null`).

When `null` is passed it returns a decimal in the range `[0, 1)`. When an
integer greater than zero is passed it returns a number in the range
`[1, $limit)`. Otherwise it throws an error.

However, a numeric integer can include units (e.g. `5px` or `8em`) and the
current behavior [drops the units][issue], which is unexpected for most users.
For example: `math.random(42px) => 28` (there is no `px`).

[random]: https://sass-lang.com/documentation/modules/math#random
[issue]: https://github.com/sass/sass/issues/1890

## Summary

> This section is non-normative.

The built-in `math.random($limit: null)` function will keep the same behavior
for numbers without units, but when given an integer with units it will return a
random integer with matching units.

### Design Decisions

#### New Behavior vs New Syntax

This proposal keeps the existing syntax but changes the semantics, therefore it
is a breaking change.

A backwards compatible alternative was a second optional parameter for units,
e.g. `math.random(42, 'px')`, but it didn't solve the problem when the first
parameter has units, e.g. `math.random(42em, 'px')`.

We decided to update the behavior and follow the [deprecation process].

[deprecation process]: #deprecation-process

#### No Stripping Units Fallback

Sass considers [stripping units an anti-pattern], so we won't provide a fallback
option for the previous unit-stripping behavior. Users are expected to rely on
unit-based arithmetic.

[stripping units an anti-pattern]: https://github.com/sass/sass/issues/533#issuecomment-52531596

## Semantics

The `math.random()` function can take an optional parameter `$limit` which
defaults to `null`.

* If `$limit` is `null` then return a pseudo-random floating-point number in the
  range `[0, 1)`.

  > Example: `math.random() => 0.1337001337`

* If `$limit` is an **integer** [number] greater than zero:

  * Return a pseudo-random integer in the range `[1, $limit)` with the same
    [units] as `$limit`.

    > Examples:
    > - `math.random(123) => 87`
    > - `math.random(123px) => 43px`
    > - `math.random(500%) => 238%`

* Otherwise throw an error.

[number]: https://sass-lang.com/documentation/values/numbers
[units]: https://sass-lang.com/documentation/values/numbers#units

## Deprecation Process

Given some users may be relying on the existing Dart Sass implementation which
strips off the units, this will be a breaking change for Dart Sass v1.

We will emit deprecation warnings for any use of `math.random($limit)` where the
`$limit` argument evaluates to a number with units.
