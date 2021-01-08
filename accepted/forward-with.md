# Reconfigurable Modules: Draft 1.1

*([Issues](https://github.com/sass/sass/issues/2744),
[Changelog](forward-with.changes.md))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Syntax](#syntax)
* [Semantics](#semantics)

## Background

> This section is non-normative.

In the existing module system each module can only be cofigured once,
the first time it is used. That works well for direct use of modules,
but doesn't allow for "middleware" modules to forward pre-configured,
and re-configurable modules. It is often useful for complex libraries to
provide a "core" module with unopinionated defaults, and then specialized
wrapper modules with more opinionated configurations and additional helpers.
That wrapper package needs to:

1. Set some or all origin-package configurations
2. Allow the user to *also* set some or all origin-package configurations,
   along with new middleware configurations
3. Use the configured origin-package to provide additional members
   based on the fully-configured origin module

Part 3 should be possible in the existing system by writing the `@forward`
rules before the `@use` rules, but parts 1 and 2 are not currently possible
in combination.

This proposal provides a syntax for middleware modules to add configuration
of the root module, without removing that option for end-users.

## Summary

> This section is non-normative.

Sass will add a `with` clause to `@forward`. The `@forward ... with` syntax is
based on the `@use ... with` syntax, but allows the addition of `!default` flags
similar to a [variable declaration][]. Unlike `@use ... with`, unconfigured
origin variables, and variables configured with a `!default` flag, will remain
configurable by any file importing the combined module. For example:

[variable declaration]: ../spec/variables.md#syntax

```scss
// _origin.scss
$hue: 0 !default;
$saturation: 50% !default;
```

```scss
// _middleware.scss
@forward "origin" with (
  $hue: 330 !default, // Can be overridden by importing users.
  $saturation: 70% // Cannot be overridden by importing users.
);
```

```scss
// entrypoint.scss
@use "middleware" with (
  $hue: 120 // override both the origin & middleware !default values
);

// middleware.$hue == 120
// middleware.$saturation == 70%
```

Keyword arguments in the configuration must reference variable names as
defined in the forwarded module, regardless of any concurent `as` clause:

```scss
// _origin.scss
$hue: 0 !default;
$color-hex: #ccc !default;
```

```scss
// _middleware.scss
@forward "origin" as color-* with (
  $hue: 330, // the color-* prefix is not referenced in configuration
  $color-hex: #966
);
```

```scss
// entrypoint.scss
@use "middleware" as m;
// m.$color-hue == 330
// m.$color-hex == #966
```

A `@forward` rule configuration is applied to the source module even if the
forwarding module acts as an entrypoint:

```scss
// _origin.scss
$hue: 0 !default;
```

```scss
// entrypoint.scss
@forward "origin" with (
  $hue: 330 !default
);

@use "origin"; // origin.$hue == 330
```

Multiple configurations can be chained in a single cascading "thread" that
contains zero or more `@forward` rules, and zero or one terminal `@use` rule.
Variables remain open to configuration in the chain as long as every mention
includes the `!default` flag. Multiple threads configuring a single module will
cause an error, even if they originate in the same file.

## Syntax

The new `WithClause` extends `@forward` to the follow grammar:

<x><pre>
**ForwardRule**     ::= '@forward' QuotedString AsClause? (ShowClause | HideClause)?  WithClause?
**WithClause**      ::= 'with' '('
&#32;                     KeywordArgument (',' KeywordArgument)\* ','?
&#32;                   ')'
**ForwardWithArgument** ::= '$' Identifier ':' Expression '!default'?
</pre></x>

## Semantics

The `@forward ... with` semantics builds on the existing proposal for
[Executing Files][], and should be understood as modifying and expanding upon
the existing execution process rather than being a comprehensive replacement.

[Executing Files]: ../accepted/module-system.md#executing-files

Given a source file `file`, a configuration `config`, and an import context
`import`:

* Let `module` be an empty module with the same URL as `file`.

* Let `uses` be an empty map from `@use` rules to [modules][].

* When a `@use` rule `rule` is encountered:

  * If `rule` has a namespace that's the same as another `@use` rule's namespace
    in `file`, throw an error.

  * Let `rule-config` be the empty configuration.

  * If `rule` has a `WithClause`:

    * For each `KeywordArgument` `argument` in this clause:

      * Let `value` be the result of evaluating `argument`'s expression.

      * Add a variable to `rule-config` with the same name as `argument`'s
        identifier and with `value` as its value.

  * Let `module` be the result of [loading][] the module with `rule`'s URL
    and `rule-config`.

  * If `rule` has a `WithClause` that contains any variables that aren't part of
    `module`'s public API or that weren't declared with a `!default` flag in
    `module`, throw an error.

  * Associate `rule` with `module` in `uses`.

* When a `@forward` rule `rule` is encountered:

  * If `rule` has an `AsClause` with identifier `prefix`:

    * Let `rule-config` be an empty configuration.

    * For each variable `variable` in `config`:

      * If `variable`'s name begins with `prefix`:

        * Let `suffix` be the portion of `variable`'s name after `prefix`.

        * Add a variable to `rule-config` with the name `suffix` and with the
          same value as `variable`.

  * Otherwise, let `rule-config` be `config`.

  * If `rule` has a `WithClause`:

    * For each `ForwardWithArgument` `argument` in this clause:

      * If `argument` has a `!default` flag and a variable exists in
        `rule-config` with the same name as `argument`'s identifier, do nothing.

      * Otherwise, let `value` be the result of evaluating `argument`'s
        expression.

      * Add a variable to `rule-config` with the same name as `argument`'s
        identifier, and with `value` as its value.

  * Let `forwarded` be the result of [loading][] the module with `rule`'s URL
    and `rule-config`.

  * If `rule` has a `WithClause` that contains any variables that aren't part of
    `forwarded`'s public API or that weren't declared with a `!default` flag in
    `forwarded`, throw an error.

  * [Forward `forwarded`][forwarding] with `file` through `module`.

> From this point on, the logic remains unchanged.

[modules]: ../accepted/module-system.md#module
[loading]: ../accepted/module-system.md#loading-modules
[forwarding]: ../accepted/module-system.md#forwarding-modules
