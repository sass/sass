# Disallow Private Variables Identifiers In `with` Clauses: Draft 2.1

*([Issue](https://github.com/sass/sass/issues/4034)),
[Changelog](private-configuration.changes.md)*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Semantics](#semantics)
  * [`@use`](#use)
  * [`@forward`](#forward)
* [Functions](#functions)
  * [`meta.load-css()`](#metaload-css)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)

## Background

> This section is non-normative.

By using [`!default`], module authors can define a module variable with a
default value. This variable can be overridden by any file that loads the module
by setting a new value in the [`with`] clause. Additionally, `!default` allows
conditionally assigning a value to a variable only when it has a `null` value,
similar to the null-coalescing operator in other programming languages.

[`!default`]: ../accepted/module-system.md#configuring-libraries
[`with`]: ../accepted/module-system.md#configuring-libraries

However, the current behavior unintentionally allows the `with` clause to modify
the value of a private variable. This is inconsistent with the expected privacy
of Sass modules.

## Summary

> This section is non-normative.

This proposal seeks to rectify the unintended behavior of the `!default` flag
affecting private variables in Sass modules. By disallowing private variables
from being overridden in a `with` clause, we reinforce module encapsulation and
prevent unexpected overrides of internal module states.

## Semantics

### `@use`

Add the following step before evaluating each [`KeywordArgument`]'s expression
in a `WithClause`:

* If the `argument`'s identifier name begins with `-`, throw an error.

  > Sass treats `_` and `-` as equivalent in identifiers.

[`KeywordArgument`]: ../spec/at-rules/use.md#semantics

### `@forward`

Add the following step before evaluating each [`ForwardWithArgument`] in a
`WithClause`:

* If the `argument`'s identifier name begins with `-`, throw an error.

  > Sass treats `_` and `-` as equivalent in identifiers.

[`ForwardWithArgument`]: ../spec/at-rules/forward.md#semantics

## Functions

### `meta.load-css()`

Change the definition of [`meta.load-css()`] by replacing the step that assigns
`config` with the following:

* If `$with` is not null:

  * If `$with` is not a map, throw an error.

  * If any keys in `$with` are not strings, throw an error.

  * If any keys in `$with` begin with `-` or `_`, throw an error.

  * Let `config` be a configuration whose variable names and values are given by
    `$with`'s keys and values.

* Otherwise, let `config` be the empty configuration.

[`meta.load-css()`]: ../spec/built-in-modules/meta.md#load-css

## Deprecation Process

The deprecation process will be divided into two phases:

### Phase 1

> This phase adds no breaking changes. Its purpose is to notify users of the
> upcoming changes to behavior and give them a chance to move towards passing
> future-proof units.

Phase 1 does not change the semantics of `with` clauses. However, we will emit a
deprecation warning named `with-private` when a `with` clause in `@use`,
`@forward`, or `meta.load-css()` configures a private variable.

### Phase 2

Phase 2 implements the full changes described above. Per the Dart Sass
compatibility policy, it won't be released until at least three months after the
first release with the deprecation warning.
