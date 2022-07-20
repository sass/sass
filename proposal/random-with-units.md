# Random: Draft 1

*([Issue](https://github.com/sass/sass/issues/1890))*

This proposal defines the behavior of the built-in [`random()`][random]
function.

[random]: ../spec/built-in-modules/math.md#random

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Semantics](#semantics)
* [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

Sass contains a built-in `random()` function which is [documented on the public
site][random public], but the behavior itself is **not** [specified][].

Currently the function parameter `$limit` (defaults to `null`) changes the
behavior from returning a decimal in range `[0, 1]` to returning an integer in
range `[1, $limit]`. An error is thrown when passing a non-numeric value.

It is undefined what happens when the `$limit` parameter contains a unit (e.g.
`5px` or `8em`), and the current behavior is unexpected as `random()` will
[drop the units and percent symbol][issue].
For example: `random(42px) => 28` (there is no `px`).

[random public]: https://sass-lang.com/documentation/modules/math#random
[specified]: ../spec/built-in-modules/math.md#random
[issue]: https://github.com/sass/sass/issues/1890

## Summary

> This section is non-normative.

The built-in `random($limit: null)` function will keep its current behavior but
it will now return units whenever a number with units or percentage is given as
an argument.

### Design Decisions

This proposal keeps the existing syntax but changes the current semantics, and
we therefore consider it a breaking change.

An alternative that has also been suggested adding an optional second parameter
to specify units, for example `random(42, 'px')` even though this can also have
unexpected behavior such as dealing with `random(42em, 'px')` which under the
current behavior it would strip the number from the units and replace it with
different ones.

We decided the alternative would be more confusing and instead will pursue with
allowing numbers with units to be passed through and implement the expected
behavior on the next major version. For now we'll include a deprecation warning
whenever a number with units or percentages is passed as an argument.

One potential source of friction is that Sass doesn't include a built-in utility
to strip away units so users relying on the current behavior for [stripping
units] would need to implement it themselves.

[stripping units]: https://stackoverflow.com/a/12335841

## Semantics

The `random()` function can take an optional parameter `$limit` which defaults
to `null`.

* If `$limit` is `null` then return a floating-point number in range `[0, 1]`.

  > Example: `random() => 0.1337001337`

* If `$limit` is an **integer** [number][] greater than zero:

  * Generate an integer number in range `[1, $limit]`

  * If `$limit` [is unitless][], return the generated number without units.

    > Example: `random(123) => 87`

  * If `$limit` has [units][], return the generated number with the same units.

    > Examples: `random(123px) => 97px` and `random(500%) => 238%`

* Otherwise throw an error.

[number]: https://sass-lang.com/documentation/values/numbers
[is unitless]: ../spec/built-in-modules/math.md#is-unitless
[units]: https://sass-lang.com/documentation/values/numbers#units

## Deprecation Process

Given some users may be relying on the existing Dart Sass implementation which
strips off the units, this will be a breaking change for Dart Sass v1.

We will emit deprecation warnings for any use of `random($limit)` where the
`$limit` argument evaluates to a number with units.
