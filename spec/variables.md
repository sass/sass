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
**Variable**            ::= '$' Identifier | NamespacedVariable
**NamespacedVariable**  ::= Identifier '.$' [PublicIdentifier][]
**VariableDeclaration** ::= Variable ':' Expression ('!global' | '!default')*
</pre></x>

[PublicIdentifier]: modules.md#syntax

No whitespace is allowed after the `$` or before or after the `.$` in
`Variable`. Each of `!global` and `!default` is allowed at most once in
`VariableDeclaration`. As with all statements, a `VariableDeclaration` must be
separated from other statements with a semicolon.

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

* If `name` is a `NamespacedVariable` and `declaration` has a `!global` flag,
  throw an error.

* Otherwise, if `declaration` is outside of any block of statements, *or*
  `declaration` has a `!global` flag, *or* `name` is a `NamespacedVariable`:

  * Let `resolved` be the result of [resolving a variable][] named `name` using
    `file`, `uses`, and `import`.

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

  [resolving a variable]: modules.md#resolving-a-member

* Otherwise, if `declaration` is within one or more blocks associated with
  `@if`, `@each`, `@for`, and/or `@while` rules *and no other blocks*:

  * Let `resolved` be the result of [resolving a variable][] named `name`.

  * If `resolved` is not null:

    * If `declaration` has a `!default` flag and `resolved`'s value isn't
      null, do nothing.

    * Otherwise, let `value` be the result of evaluating `declaration`'s value.

    * If `name` *doesn't* begin with `-` or `_`, add a variable with name `name`
      and value `value` to `module`.

      > This overrides the previous definition, if one exists.

    * Add a variable with name `name` and value `value` to `import`.

      > This also overrides the previous definition.

* Otherwise, if no block containing `declaration` has a [scope](#scope) with a
  variable named `name`, set the innermost block's scope's variable `name` to
  `value`.

* Otherwise, let `scope` be the scope of the innermost block such that `scope`
  already has a variable named `name`.

* Set `scope`'s variable `name` to `value`.

### Evaluating a Variable

To evaluate a `Variable` `variable`:

* Let `definition` be the result of [resolving a variable][] named `variable`.
  If this returns null, throw an error.

* Return `definition`'s value.
