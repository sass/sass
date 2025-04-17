# Disallow `!default` on Private Variables: Draft 2.0

*([Issue](https://github.com/sass/sass/issues/4034)),
[Changelog](default-on-private-variables.changes.md)*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Semantics](#semantics)
  * [`@use`](#use)
  * [`@forward`](#forward)
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

Replace way each [`KeywordArgument`] in a `WithClause` is processed with:

* For each `KeywordArgument` `argument` in this clause:

  * If the `argument`'s identifier name begins with `-`, throw an error.

    > Sass treats `_` and `-` as equivalent in identifiers.

  * Otherwise, let `value` be the result of evaluating `argument`'s expression.

  * Add a variable to `rule-config` with the same name as `argument`'s
    identifier and with `value` as its value.

[`KeywordArgument`]: ../spec/at-rules/use.md#semantics

### `@forward`

Replace the way each [`ForwardWithArgument`] in a `WithClause` is processed
with:

* For each `ForwardWithArgument` `argument` in this clause:

  * If the `argument`'s identifier name begins with `-`, throw an error.

    > Sass treats `_` and `-` as equivalent in identifiers.

  * Otherwise, if `argument` has a `!default` flag and a variable exists in
    `rule-config` with the same name as `argument`'s identifier, do nothing.

  * Otherwise, let `value` be the result of evaluating `argument`'s expression.

  * Add a variable to `rule-config` with the same name as `argument`'s
    identifier, and with `value` as its value.

[`ForwardWithArgument`]: ../spec/at-rules/forward.md#semantics

## Deprecation Process

The deprecation process will be divided into two phases:

### Phase 1

> This phase adds no breaking changes. Its purpose is to notify users of the
> upcoming changes to behavior and give them a chance to move towards passing
> future-proof units.

Phase 1 does not change the semantics of `with` clauses. However, we will emit a
deprecation warning named `with-clause-private-variable` when a `with` clause
privacy of Sass modules.

### Phase 2

Phase 2 implements the full changes described above. Per the Dart Sass
compatibility policy, it won't be released until at least three months after the
first release with the deprecation warning.
