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

* If `rule` is outside of any block of statements:

  * If `name` *doesn't* begin with `-` or `_`, set [the current module][]'s
    function `name` to `rule`.

    > This overrides the previous definition, if one exists.

  * Set [the current import context][]'s function `name` to `rule`.

    > This happens regardless of whether or not it begins with `-` or `_`.

  [the current module]: ../spec.md#current-module
  [the current import context]: ../spec.md#current-import-context

* Otherwise, set the innermost block's [scope][]'s function `name` to `value`.

  [scope]: ../variables.md#scope
