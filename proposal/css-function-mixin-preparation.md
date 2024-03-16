# Preparing for Plain CSS Functions and Mixins: Draft 1

*([Issue](https://github.com/sass/sass/issues/3787))*

This proposal adds errors for existing Sass syntax which is likely to conflict
with upcoming plain CSS support for functions and mixins.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Semantics](#semantics)
  * [`@mixin`](#mixin)
  * [`@function`](#function)
* [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

In [this informal proposal] and [this issue], the CSS working group have begun
seriously discussing the possibility of introducing functions and mixins to CSS
itself. Although many specific details are still in flux, all syntaxes that have
been floated have two things in common:

[this informal proposal]: https://css.oddbird.net/sasslike/mixins-functions/
[this issue]: https://github.com/w3c/csswg-drafts/issues/9350

* They use at-rules named `@mixin` and `@function`, just as Sass does today.
* They require custom identifiers beginning with `--` after these at-rules.

Sass currently allows functions and mixins whose names begin with `--`, but this
is a relatively painless thing to deprecate and would allow us full CSS
compatibility with whatever version of the proposal ends up getting implemented.

## Summary

> This section is non-normative.

This proposal makes any mixin or function whose name begins with `--` an error.
This will be preceded by a deprecation period in which those mixins and
functions will just produce warnings.

## Semantics

### `@mixin`

Update [the `@mixin` semantics] to add after the first line:

[the `@mixin` semantics]: ../spec/at-rules/mixin.md

* If `name` begins with `--`, throw an error.

### `@function`

Update [the `@function` semantics] to add after the first line:

[the `@function` semantics]: ../spec/at-rules/function.md

* If `name` begins with `--`, throw an error.

## Deprecation Process

During the deprecation period, instead of throwing errors as described in [the
semantics section] above, emit a deprecation warning named `css-function-mixin`.

[the semantics section]: #semantics
