# Reconfigurable Modules via "@forward ... with": Draft 1

*([Issues](https://github.com/sass/sass/issues/2744))*

## Table of Contents

- [Reconfigurable Modules via "@forward ... with": Draft 1](#reconfigurable-modules-via-%22forward--with%22-draft-1)
  - [Table of Contents](#table-of-contents)
  - [Background](#background)
  - [Summary](#summary)
  - [Syntax](#syntax)
  - [Semantics](#semantics)

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
similar to `@use ... with`, but also allows additional `!default` flags,
similar to a [variable declaration][]. Unlike `@use ... with`, unconfigured
origin variables, and variables configured with a `!default` flag will remain configurable by any file importing the combined module. For example:

[variable declaration]: https://github.com/sass/sass/blob/master/spec/variables.md#syntax

```scss
// _origin.scss
$hue: 0 !default;
$saturation: 50% !default;

// _middleware.scss
@forward "origin" with (
  $hue: 330 !default, // Can be overridden by importing users.
  $saturation: 70% // Cannot be overridden by importing users.
);

// entrypoint.scss
@use "middleware" with (
  $hue: 120 // override both the origin & middleware !default values
  // middleware.$saturation will be 70%
);
```

Keyword arguments in the configuration must reference variable names as
defined in the forwarded module, regardless of any concurent `as` clause:

```scss
// _origin.scss
$hue: 0 !default;
$color-hex: #ccc !default;

// _middleware.scss
@forward "origin" as color-* with (
  $hue: 330, // the color-* prefix is not referenced in configuration
  $color-hex: #966
);

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

// entrypoint.scss
@forward "origin" with (
  $hue: 330 !default
);

@use "origin"; // origin.$hue == 330
```

Multiple configurations can be chained in a single cascading "thread" that
contains zero or more `@forward` rules, and zero or one terminal `@use` rule.
Variables remain open to configuration in the chain as long as every mention
includes the `!default` flag:

```scss
// _origin.scss
$hue: 0 !default;

// _middle1.scss
@forward "origin" with (
  $hue: 330 !default
);

// _middle2.scss
@forward "middle1" with (
  $hue: 120 !default
);

// entrypoint.scss
@use "middle2" with (
  $hue: 30
);

// middle2.$hue == 30
```

Multiple threads configuring a single module will cause an error, no matter
what combinations of `@forward` and `@use` are involved:

```scss
// _origin.scss
$hue: 0 !default;

// _middle1.scss
@forward "origin" with (
  $hue: 330 !default
);

// _middle2.scss
@use "origin" with (
  $hue: 120
);

// entrypoint.scss
@forward "middle1";
@use "middle2"; // ERROR
```

## Syntax

The new `WithClause` extends `@forward` to the follow grammar:

<x><pre>
**ForwardRule**     ::= '@forward' QuotedString AsClause? (ShowClause | HideClause)?  WithClause?
**AsClause**        ::= 'as' Identifier '*'
**ShowClause**      ::= 'show' MemberName (',' MemberName)*
**HideClause**      ::= 'hide' MemberName (',' MemberName)*
**MemberName**      ::= '$'? Identifier
**WithClause**      ::= 'with' '('
&#32;                     KeywordArgument (',' KeywordArgument)\* ','?
&#32;                   ')'
**KeywordArgument** ::= '$' Identifier ':' Expression ('!default')
</pre></x>

## Semantics
