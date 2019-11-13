# Configuring Modules Through Imports: Draft 1

*([Issue](https://github.com/sass/sass/issues/2772))*

This proposal modifies the module system semantics to support configuring
libraries that have migrated to the module system through `@import` rules in
downstream stylesheets without requiring changes to those stylesheets.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Definitions](#definitions)
* [Procedures](#procedures)
* [Semantics](#semantics)
  * [Executing Files](#executing-files)
  * [Importing Files](#importing-files)

## Background

> This section is non-normative.

As it is, while configuration in a `@use` rule passes through `@forward` rules
automatically, there's no way for a stylesheet using `@import` to configure
members that are behind a `@forward` rule.

This makes it difficult for libraries with configurable variables to migrate to
the module system without breaking downstream users that haven't migrated yet.
This is especially true if the library removed a manual prefix from its members
during migration. When the [migrator][] does this, it creates an import-only file
that forwards the regular stylesheet with the prefix added back, but that
`@forward` rule means configuration doesn't work.

Because it is nearly impossible to migrate these cases incrementally, this
violates the module system's [backwards compatibility goal][]. Libraries with
prefixes and configuration variables are common, and without an incremental
migration solution, these libraries may be slow to start using the module
system, limiting its adoption by the ecosystem as a whole.

[migrator]: https://sass-lang.com/documentation/cli/migrator#remove-prefix
[backwards compatibility goal]: ../accepted/module-system.md#low-level

## Summary

> This section is non-normative.

This proposal modifies the semantics for configuring a module when `@import` is
involved to ensure that most downstream users of a library are not broken when
the library migrates to the module system.

When a file is loaded by an `@import` rule, a [configuration][] is created that
includes all variables declared in the current [import context][]. This
implicitly created configuration is a special type that can be distinguished
from other, explicitly created configurations.

When a `@forward` rule is encountered within a file that was loaded by an
`@import` rule, the implicit configuration is passed to it in the same way as an
explicit configuration from a `@use` rule would be.

Normally, when a module has already been executed, and is then loaded with a
configuration that is not empty, an error is thrown. However, if the
configuration is an implicit one, this error will be ignored and the executed
module will be returned in the same way as if the configuration were empty. If
an implicit configuration passes through a `@forward` rule with a prefix, then
new configuration created for that rule is also considered an implicit one and
retains this special property.

This proposal should allow most existing stylesheets using `@import` to continue
working unchanged after a library they depend on migrates to the module system.

[configuration]: ../accepted/module-system.md#configuration
[import context]: ../accepted/module-system.md#import-context

### Design Decisions

We considered a few alternatives in designing this proposal.

One alternative did not involve any language changes at all, instead
recommending that library authors add `@use` rules explicitly configuring their
variables to their [import-only files][] when migrating. For example:

```scss
// app.scss
$lib-color: blue;
@import "library";

// _library.scss
$color: green !default;

// _library.import.scss
@use "sass:meta";
@use "library" with (
  $color: if(meta.variable-defined("lib-color"), $lib-color, null)
);
@forward "library" as lib-*;
```

While this would work for simple libraries with a single entrypoint, libraries
with multiple components that depend on common sublibraries but can be imported
separately would often break, as this solution would attempt to configure some
modules more than once. The same would happen if you imported even a simple
library more than once.

An alternative to just ignoring subsequent implicit configurations would be to
(a) filter them to include only variables that are actually configurable and
(b) allow the subsequent configuration only if it exactly matched the previous
one, but doing this matching could hurt performance. This would also still cause
issues if the same library is imported more than once.

While the solution we settled on does not perfectly cover all use cases that
worked before the library migrated to the module system, we think it strikes a
good balance of supporting most existing use cases without hurting performance
or making the language specification and implementation overly complicated.

For example, if a downstream user imports a library twice and changes its
configuration between the two imports, the change will be ignored. However,
this is an edge case that is (a) probably not intended by the user, (b)
relatively easy to fix by moving all declared configuration variables before
all library imports, and (c) very difficult to support for a library using the
module system without compromising the module system's [import once goal][], as
handling this case would require modules in the library to be executed twice.

[import-only files]: ../accepted/module-system.md#import-compatibility
[import once goal]: ../accepted/module-system.md#low-level

## Definitions

This proposal modifies the definition of a [configuration][] within the
[module system spec][] to add the following:

A configuration is either *explicit* or *implicit*. When a configuration is
created, if the type is not specified, it is considered *explicit*.

[module system spec]: ../accepted/module-system.md

## Procedures

This proposal modifies the fourth bullet of the [Loading Modules][] procedure
within the [module system spec][] to read as follows:


* If `file` has already been [executed][]:

  * If `config` is **explicit and** not empty, throw an error.

  * Otherwise, return the module that execution produced.

[Loading Modules]: ../accepted/module-system.md#loading-modules
[executed]: ../accepted/module-system.md#executing-files

## Semantics

### Executing Files

This proposal modifies the first bullet of the semantics of [Executing Files][]
within the [module system spec][] to read as follows:

* If this file isn't being executed for a `@forward` **or `@import`** rule:

  * For every variable name `name` in `config`:

    * If neither `file` nor any source file for a module transitively forwarded
      or imported by `file` contains a variable declaration named `name` with a
      `!default` flag at the root of the stylesheet, throw an error.

This proposal also modifies the fifth bullet to read as follows:

* When a `@forward` rule `rule` is encountered:

  * If `rule` has an `AsClause` with identifier `prefix`:

    * Let `rule-config` be an empty configuration. **`rule-config` is implicit
      if `config` is implicit and explicit otherwise.**

    * For each variable `variable` in `config`:

      * If `variable`'s name begins with `prefix`:

        * Let `suffix` be the portion of `variable`'s name after `prefix`.

        * Add a variable to `rule-config` with the name `suffix` and with the
          same value as `variable`.

  * Otherwise, let `rule-config` be `config`.

  * Let `forwarded` be the result of [loading][] the module with `rule`'s URL
    and `rule-config`.

  * [Forward `forwarded`][] with `file` through `module`.

[Executing Files]: ../accepted/module-system.md#executing-files
[loading]: ../accepted/module-system.md#loading-modules
[Forward `forwarded`]: ../accepted/module-system.md#forwarding-modules

### Importing Files

This proposal modifies the semantics for [Importing Files][] within the
[module system spec][] to read as follows:

This algorithm takes a [source file][] `file`, an [import context][] `import`,
and a mutable [module][] `module`.

* If `file` is currently being executed, throw an error.

* **Let `config` be an implicit configuration containing every variable defined
  in `import`.**

  > If `file` does not contain any `@forward` rules, `config` will never be
  > used, so implementations may wish to skip this step and use the empty
  > configuration instead in that case for performance reasons.

* Let `imported` be the result of [executing][] `file` with ~~the empty
  configuration~~ **`config` as its configuration** and `import` as
  its import context, except that if the `@import` rule is nested within
  at-rules and/or style rules, that context is preserved when executing `file`.

* Let `css` be the result of [resolving extensions][] for
  `imported`, except that if the `@import` rule is nested within at-rules and/or
  style rules, that context is added to CSS that comes from modules loaded by
  `imported`.

* Add `css` to `module`'s CSS.

* Add `imported`'s [extensions][] to `module`.

* Add each member in `imported` to `import` and `module`.

[Importing Files]: ../accepted/module-system.md#importing-files
[source file]: ../accepted/module-system.md#source-file
[module]: ../accepted/module-system.md#module
[executing]: ../accepted/module-system.md#executing-files
[resolving extensions]: ../accepted/module-system.md#resolving-extensions
[extensions]: ../accepted/module-system.md#extension
