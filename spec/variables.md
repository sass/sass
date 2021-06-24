# Variables

## Table of Contents

* [Syntax](#syntax)
* [Definitions](#definitions)
  * [Scope](#scope)
  * [Global Scope](#global-scope)
* [Semantics](#semantics)
  * [Executing a Variable Declaration](#executing-a-variable-declaration)
  * [Evaluating a Variable](#evaluating-a-variable)

## Syntax

<x><pre>
**Variable**            ::= PlainVariable | NamespacedVariable
**PlainVariable**       ::= '$' [\<ident-token>][]
**NamespacedVariable**  ::= [\<ident-token>][] '.$' [PublicIdentifier][]
**VariableDeclaration** ::= Variable ':' Expression ('!global' | '!default')*
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
[PublicIdentifier]: modules.md#syntax

No whitespace is allowed after the `$` in `PlainVariable` or before or after
the `.$` in `NamespacedVariable`. Each of `!global` and `!default` is allowed
at most once in `VariableDeclaration`. As with all statements, a
`VariableDeclaration` must be separated from other statements with a semicolon.

## Definitions

### Scope

A *scope* is a mapping from variable names to values. Every block of statements
delimited by `{` and `}` in SCSS or by indentation in the indented syntax has an
associated scope.

### Global Scope

The *global scope* is the scope shared among the top level of all Sass files.

## Semantics

### Executing a Variable Declaration

To execute a `VariableDeclaration` `declaration`:

* Let `value` be the result of evaluating `declaration`'s `Expression`.

* Let `name` be `declaration`'s `Variable`.

* Let `resolved` be the result of [resolving a variable][] named `name`.

  [resolving a variable]: ../spec/modules.md#resolving-a-member

* If `name` is a `NamespacedVariable` and `declaration` has a `!global` flag,
  throw an error.

* Otherwise, if `resolved` is a variable from a built-in module, throw an
  error.

* Otherwise, if `declaration` is outside of any block of statements, *or*
  `declaration` has a `!global` flag, *or* `name` is a `NamespacedVariable`:

  * If `declaration` has a `!default` flag, `resolved` isn't null, *and*
   `resolved`'s value isn't null, do nothing.

  * Otherwise, if `resolved` is a variable in another module:

    * Evaluate `declaration`'s value and set `resolved`'s value to the result.

  * Otherwise:

    * If `declaration` is outside of any block of statements, it has a
      `!default` flag, *and* `config` contains a variable named `name` whose
      value is not null:

      * Let `value` be the value of `config`'s variable named `name`.

    * Otherwise, let `value` be the result of evaluating `declaration`'s value.

    * If `name` *doesn't* begin with `-` or `_`, add a variable with name `name`
      and value `value` to `module`.

      > This overrides the previous definition, if one exists.

    * Add a variable with name `name` and value `value` to `import`.

      > This also overrides the previous definition.

* Otherwise, if `declaration` is within one or more blocks associated with
  `@if`, `@each`, `@for`, and/or `@while` rules *and no other blocks*:

  * If `resolved` is not null:

    * If `declaration` has a `!default` flag and `resolved`'s value isn't
      null, do nothing.

    * Otherwise, let `value` be the result of evaluating `declaration`'s value.

    * If `name` *doesn't* begin with `-` or `_`, add a variable with name `name`
      and value `value` to `module`.

      > This overrides the previous definition, if one exists.

    * Add a variable with name `name` and value `value` to `import`.

      > This also overrides the previous definition.

* Otherwise, if `resolved` is null, get the innermost block containing
  `declaration` and set its scope's variable `name` to `value`.

* Otherwise, set `resolved`'s value to `value`.

### Evaluating a Variable

To evaluate a `Variable` `variable`:

* Let `definition` be the result of [resolving a variable][] named `variable`.
  If this returns null, throw an error.

* Return `definition`'s value.
