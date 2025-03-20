# Disallow `!default` on Private Variables: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4034))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Semantics](#semantics)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)

## Background

> This section is non-normative.

By using [`!default`], a module's variable value becomes overridable by any file
that loads the module. The primary use case is to allow module authors to
provide default values for variables that can be overridden by users when
importing the module.

[`!default`]: ../accepted/module-system.md#configuring-libraries

However, the current behavior unintentionally allows !default to affect private
variables. This is inconsistent with the expected privacy of Sass modules.

## Summary

> This section is non-normative.

This proposal seeks to rectify the unintended behavior of the `!default` flag
affecting private variables in Sass modules. By disallowing `!default` on
private variables, we reinforce module encapsulation and prevent unexpected
overrides of internal module states.

## Semantics

Add the following to [the Semantics for "Executing a Variable Declaration"]
after validating that `resolved` is not a variable from a built-in module:

[the Semantics for "Executing a Variable Declaration"]: ../spec/variables.md#executing-a-variable-declaration

* Otherwise, if `name` begins with `-` and `declaration` has a
  `!default` flag, throw an error.

  > Sass treats `_` and `-` as equivalent in variable names.

## Deprecation Process

The deprecation process will be divided into two phases:

### Phase 1

> This phase adds no breaking changes. Its purpose is to notify users of the
> upcoming changes to behavior and give them a chance to move towards passing
> future-proof units.

Phase 1 does not change the semantics of variable declarations. However, if a
private variable (one whose name starts with `_` or `-`) has a `!default` flag,
emit a deprecation warning named `private-variable-default`.

### Phase 2

Phase 2 implements the full changes described above. Per the Dart Sass
compatibility policy, it won't be released until at least three months after the
first release with the deprecation warning.
