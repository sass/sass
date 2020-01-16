# Partial Namespaces: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/2800))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Procedures](#procedures)
  * [Determining a `@use` Rule's Namespace](#determining-a-use-rules-namespace)
* [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

In the new Sass [module system][] as written, the implicit namespace of a `@use`
rule that loads a partial with an explicit leading underscore contains that
underscore. This was unintended and confusing, making the namespace look like a
private identifier and making it different than the same file's namespace
imported without the underscore.

[module system]: ../accepted/module-system.md

## Summary

> This section is non-normative.

The underscore will be stripped from the namespace, so `@use "_styles"` will
have the implicit namespace `styles` rather than `_styles`.

## Procedures

### Determining a `@use` Rule's Namespace

This modifies the existing algorithm for [Determining a `@use` Rule's
Namespace][] to read as follows (new text in bold):

[Determining a `@use` Rule's Namespace]: ../spec/at-rules/use.md#determining-a-use-rules-namespace

* If `rule` has an `'as'` clause `as`:

  * If `as` has an identifier, return it.

  * Otherwise, return `null`. The rule is global.

* Let `path` be the `rule`'s URL's [path][URL path].

  [URL path]: https://url.spec.whatwg.org/#concept-url-path

* Let `basename` be the text after the final `/` in `path`, or the entire `path`
  if `path` doesn't contain `/`.

* Let `module-name` be the text before the first `.` in `path`, or the entire
  `path` if `path` doesn't contain `.`.

* **If `module-name` begins with `_`, remove the leading `_` and set
  `module-name` to the result.**

* If `module-name` isn't a Sass identifier, throw an error.

* Return `module-name`.

## Deprecation Process

Although this is technically a breaking change, it will be made without a
deprecation process for the following reasons:

* Including a leading underscore in a loaded URL is unnecessary, and very few
  stylesheets do it in practice.

* The module system is still very young and not yet super widely used, so it's
  even more unlikely that leading underscores are used in `@use` statements in
  particular.

* This is a fix for an unintended bug in the spec rather than a change to
  intended behavior that users were expected to rely upon.
