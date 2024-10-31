# `@mixin`, `@include`, and `@content`

## Table of Contents

* [`@mixin`](#mixin)
  * [Syntax](#syntax)
  * [Semantics](#semantics)
* [`@include`](#include)
  * [Syntax](#syntax-1)
  * [Semantics](#semantics-1)
* [`@content`](#content)
  * [Syntax](#syntax-2)
  * [Semantics](#semantics-2)

## `@mixin`

### Syntax

<x><pre>
**MixinRule** ::= '@mixin' [\<ident-token>] ArgumentDeclaration? [Block]
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
[Block]: ../statement.md#block

No whitespace is allowed between the `Identifier` and the `ArgumentDeclaration`
in `MixinRule`.

### Semantics

To execute a `@mixin` rule `rule`:

* Let `name` be the value of `rule`'s `Identifier`.

* If `name` begins with `--`, throw an error.

* Let `parent` be the [current scope].

  [current scope]: ../spec.md#scope

* Let `mixin` be a [mixin] named `name` which accepts a content block if `rule`
  contains a `@content` rule. To execute this mixin with `args`:

  [mixin]: ../types/mixins.md

  * With the current scope set to an empty [scope] with `parent` as its parent:

    * Evaluate `args` with `rule`'s `ArgumentDeclaration`.

    * Execute each statement in `rule`.

  [scope]: ../spec.md#scope

* If `rule` is outside of any block of statements:

  * If `name` *doesn't* begin with `-` or `_`, set [the current module]'s
    mixin `name` to `mixin`.

    > This overrides the previous definition, if one exists.

  * Set [the current import context]'s mixin `name` to `mixin`.

    > This happens regardless of whether or not it begins with `-` or `_`.

  [the current module]: ../spec.md#current-module
  [the current import context]: ../spec.md#current-import-context

* Otherwise, set the [current scope]'s mixin `name` to `mixin`.

  > This overrides the previous definition, if one exists.

## `@include`

### Syntax

<x><pre>
**IncludeRule**      ::= '@include' [NamespacedIdentifier] ArgumentInvocation?
&#32;                    ContentBlock?
**ContentBlock**     ::= UsingDeclaration? [Block]
**UsingDeclaration** ::= 'using' ArgumentDeclaration
</pre></x>

[NamespacedIdentifier]: ../modules.md#syntax

No whitespace is allowed between the `NamespacedIdentifier` and the
`ArgumentInvocation` in `IncludeRule`.

### Semantics

To execute an `@include` rule `rule`:

* Let `name` be `rule`'s `NamespacedIdentifier`.

* Let `mixin` be the result of [resolving a mixin] named `name`. If this returns
  null, throw an error.

  [resolving a mixin]: ../modules.md#resolving-a-member

* Execute `mixin` with `rule`'s `ArgumentInvocation`.

## `@content`

The `@content` rule runs a block of styles provided by the user who invoked the
current mixin.

### Syntax

<x><pre>
**ContentRule** ::= '@content' ArgumentInvocation?
</pre></x>

As with all statements, a `ContentRule` must be separated from other statements
with a semicolon.

### Semantics

To execute a `@content` rule `content` within a mixin that's invoked with
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

* Execute `include`'s `ContentBlock` statements in `scope`.
