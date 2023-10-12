# `@function`

## Table of Contents

* [Syntax](#syntax)
* [Semantics](#semantics)

## Syntax

<x><pre>
**FunctionRule** ::= '@function' [\<ident-token>][] ArgumentDeclaration '{' Statements '}'
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram

No whitespace is allowed between the `Identifier` and the `ArgumentDeclaration`
in `FunctionRule`.

## Semantics

To execute a `@function` rule `rule`:

* Let `name` be the value of `rule`'s `Identifier`.

* If `name` is `calc`, `element`, `expression`, `url`, `and`, `or`, or `not`, or
  if `name` has a [vendor prefix] and the unprefixed identifier is one of those
  strings, throw an error.

  [vendor prefix]: ../syntax.md#vendor-prefix

* Let `parent` be the [current scope].

  [current scope]: ../spec.md#scope

* Let `function` be a [function] named `name` which does the following when
  executed with `args`:

  [function]: ../types/functions.md

  * With the current scope set to an empty [scope] with `parent` as its parent:

    * Evaluate `args` with `rule`'s `ArgumentDeclaration`.

    * Execute each statement in `rule`.

    * Return the value from the `@return` rule if one was executed, or throw an
      error if no `@return` rule was executed.

  [scope]: ../spec.md#scope

* If `rule` is outside of any block of statements:

  * If `name` *doesn't* begin with `-` or `_`, set [the current module][]'s
    function `name` to `function`.

    > This overrides the previous definition, if one exists.

  * Set [the current import context]'s function `name` to `function`.

    > This happens regardless of whether or not it begins with `-` or `_`.

  [the current module]: ../spec.md#current-module
  [the current import context]: ../spec.md#current-import-context

* Otherwise, set the [current scope]'s function `name` to `function`.
