# `@function`

## Table of Contents

* [Syntax](#syntax)
* [Semantics](#semantics)

## Syntax

<x><pre>
**FunctionRule** ::= '@function' Identifier ArgumentDeclaration '{' Statements '}'
</pre></x>

No whitespace is allowed between the `Identifier` and the `ArgumentDeclaration`
in `FunctionRule`.

## Semantics

To execute a `@function` rule `rule`:

* Let `name` be the value of `rule`'s `Identifier`.

* If `rule` is outside of any block of statements:

  * If `name` *doesn't* begin with `-` or `_`, set [the current module][]'s
    function `name` to `rule`.

    [the current module]: ../spec.md#current-module

    > This overrides the previous definition, if one exists.

  * Set [the current import context][]'s function `name` to `rule`.

    [the current import context]: ../spec.md#current-import-context

    > This happens regardless of whether or not it begins with `-` or `_`.

* Otherwise, set the innermost block's [scope][]'s function `name` to `value`.

  [scope]: ../variables.md#scope
