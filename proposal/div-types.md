# Div Types: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4287))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Alternatives Considered](#alternatives-considered)
  * [Existing Deprecation](#existing-deprecation)
* [Math Functions](#math-functions)
  * [`div()`](#div)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)

## Background

> This section is non-normative.

When we rolled out the first phase of the [slash separator proposal] in 2019, we
made `math.div()` emit a warning when the user passed in non-number values. We
didn't make it emit an error because we wanted it to be as smooth as possible to
migrate all uses of `/`-as-division to `math.div()`, even those that didn't
actually use numbers.

[slash separator proposal]: ../accepted/slash-separator.md

However, we never made this warning an actual named deprecation, nor did we
include the desired behavior in the specification. This proposal addresses those
shortcomings.

## Summary

> This section is non-normative.

This makes passing non-number values to `math.div()` an error, and defines a
named deprecation (`div-types`) for the transition period.

## Alternatives Considered

> This section is non-normative.

### Existing Deprecation

We considered including this behavior in the existing `slash-div` deprecation,
which will become a full error in Dart Sass 2.0.0. However, this has two
downsides:

1. We said in the [road to Dart Sass 2] blog post that any new deprecations
   added after publishing that blog post wouldn't become errors in Dart Sass 2.
   Adding new deprecated behavior to an existing deprecation arguably violates
   that promise. Similarly, it would mean that a user who has enabled
   `--fatal-deprecations slash-div` would suddenly have a new breaking change.

2. The point of allowing non-numbers in `math.div()` at all is to make it easier
   for users to fix the `slash-div` deprecation. If we don't support that
   behavior in Dart Sass 2, then it's not really helping users migrate at all.

[road to Dart Sass 2]: https://sass-lang.com/blog/the-road-to-dart-sass-2/

## Math Functions

### `div()`

Replace the existing "Otherwise, if either of `$number1` or `$number2` are not
numbers" step with

* Otherwise, if either of `$number1` or `$number2` are not numbers, throw an
  error.

## Deprecation Process

### Phase 1

In the first phase, to be released immediately, passing non-number arguments to
`math.div()` will have the same behavior except that it will emit a deprecation
warning named `div-types`.

### Phase 2

In the second phase, we'll obsolete the `div-types` breaking change and
implement the changes described above.
