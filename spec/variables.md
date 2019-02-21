# Variables

## Table of Contents

* [Syntax](#syntax)
* [Definitions](#definitions)
  * [Scope](#scope)
  * [Global Scope](#global-scope)
* [Procedures](#procedures)
  * [Assigning to a Variable](#assigning-to-a-variable)
  * [Referencing a Variable](#referencing-a-variable)
  
## Syntax

<x><pre>
**Variable**            ::= '$' Identifier
**VariableDeclaration** ::= Variable ':' Expression ('!global' | '!default')*
</pre></x>

No whitespace is allowed after `$`. Each of `!global` and `!default` is allowed
at most once. As with all statements, a `VariableDeclaration` must be separated
from other statements with a semicolon.

## Definitions

### Scope

A *scope* is a mapping from variable names to values. Every block of statements
delimited by `{` and `}` in SCSS or by indentation in the indented syntax has an
associated scope.

### Global Scope

The *global scope* is the scope shared among the top level of all Sass files.

## Procedures

### Assigning to a Variable

Given a `VariableDeclaration` `declaration` and a SassScript value `value`:

* Let `name` be `declaration`'s `Variable`'s name.

* If `declaration` has the `!global` flag:

  * If the [global scope](#global-scope) already has a variable named `name`,
    set it to `value`.

  * Otherwise, throw an error.

    > Older versions of Sass created a new global variable here rather than
    > throwing an error. Implementations are encouraged to do so while emitting
    > a deprecation warning before transitioning to the new behavior.

* If `declaration` is outside of any block of statements, set the [global
  scope](#global-scope)'s variable `name` to `value`.

* Otherwise, if `declaration` is within one or more blocks associated with
  `@if`, `@each`, `@for`, and/or `@while` rules *and no other blocks*, and if
  the global scope already has a variable named `name`, set
  that variable to `value`.

  > This makes it possible to write
  >
  > ```scss
  > $variable: value1;
  > @if $condition {
  >   $variable: value2;
  > }
  > ```
  >
  > without needing to use `!global`.

* Otherwise, if no block containing `declaration` has a [scope](#scope) with a
  variable named `name`, set the innermost block's scope's variable `name` to
  `value`.

* Otherwise, let `scope` be the scope of the innermost block such that `scope`
  already has a variable named `name`. Set `scope`'s variable `name` to `value`.
  
### Referencing a Variable

This algorithm takes a `Variable` `variable` and returns a SassScript value:

* Let `name` be `variable`'s `name`.

* Let `scope` be the [scope](#scope) of the innermost block containing
  `variable` such that `scope` has a variable named `name`, or `null` if no such
  scope exists.

* If `scope` is not `null`, return `scope`'s variable value for `name`.

* Otherwise, if the [global scope](#global-scope) has a variable named `name`,
  return that variable's value.

* Otherwise, throw an error.
