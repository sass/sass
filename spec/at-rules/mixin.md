# `@mixin`, `@include`, and `@content`

## Table of Contents

* [`@include`](#include)
* [`@content`](#content)

## `@include`

The syntax for an `@include` rule is as follows:

<x><pre>
**IncludeRule**      ::= '@include' [\<ident-token>][] ArgumentInvocation? ContentBlock?
**ContentBlock**     ::= UsingDeclaration? '{' Statements '}'
**UsingDeclaration** ::= 'using' ArgumentDeclaration
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram

## `@content`

The `@content` rule runs a block of styles provided by the user who invoked the
current mixin. Its syntax is as follows:

<x><pre>
**ContentRule** ::= '@content' ArgumentInvocation?
</pre></x>

As with all statement, a `ContentRule` must be separated from other statements
with a semicolon.

When evaluating a `@content` rule `content` within a mixin that's invoked with
an `@include` rule `include`:

> `@content` rules are syntactically guaranteed to only appear in mixin bodies,
> and mixins must be invoked using `@include`, so `include` is guaranted to
> exist.

* Let `invocation` be `content`'s `ArgumentInvocation`, or an invocation with no
  arguments if `content` has no `ArgumentInvocation`.

  > This means that `@content` and `@content()` are interpreted identically.

* Let `declaration` be `include`'s `UsingDeclaration`'s `ArgumentDeclaration`,
  or a declaration with no arguments if `include` has no `UsingDeclaration`.

  > This means that `@include foo { ... }` and `@include foo using () { ... }`
  > are interpreted identically.

* Let `arguments` be the result of applying `invocation` to `declaration`.

  > This means `arguments` is a mapping from variable names to values. If
  > `invocation` isn't a valid invocation of `declaration`, this will throw an
  > error that should be surfaced to the user.

* If `include` has no `ContentBlock`, do nothing.

  > Exiting here rather than earlier means that `@content(value)` is an error if
  > `include` has no content block.

* Otherwise, let `scope` be a new scope that's a child of `include`'s scope.

* For each pair `variable` and `value` in `arguments`:

  * Set `variable` to `value` in `scope`.

* Evaluate `include`'s `ContentBlock` statements in `scope`.
