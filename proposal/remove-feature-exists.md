# `feature-exists` removal: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/3702))*

This proposal removes `feature-exists` in the built-in `sass:meta` module.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Deprecation Process](#deprecation-process)
  * [`feature-exists`](#feature-exists)

## Background

> This section is non-normative.

`feature-exists` is ill-defined as nothing specifies what each feature actually
requires to be considered as supported. The shared `sass-spec` testsuite does
not cover this either. Thus, no new feature identifier has been added in it
since years.
New Sass features essentially fall into one of three categories:

1. New built-in functions or mixins, which are easy to detect using other
   `sass:meta` functions.
2. Language-level features which are relatively easy to detect on their own
   (for example, first-class calc can be detected with calc(1 + 1) == 2).
3. New syntax which can't even be parsed in implementations that don't support
   it, for which feature detection isn't particularly useful anyway.

## Summary

> This section is non-normative.

The `feature-exists` function of `sass:meta` and its global alias will be
removed without any direct replacement.

## Deprecation Process

The `feature-exists` function will be supported until the next major version
with a deprecation warning.

### `feature-exists`

During the deprecation period, when invoking the `feature-exists` function,
emit a deprecation warning named `feature-exists`.
