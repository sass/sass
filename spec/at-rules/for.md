# `@for`

## Table of Contents

* [Syntax](#syntax)
* [Semantics](#semantics)

## Syntax

<x><pre>
**ForRule**            ::= '@for' [PlainVariable][] FromDeclaration
&#32;                      (ToDeclaration | ThroughDeclaration) ForBlock
**FromDeclaration**    ::= 'from' Expression
**ToDeclaration**      ::= 'to' Expression
**ThroughDeclaration** ::= 'through' Expression
**ForBlock**           ::= '{' Statements '}'
</pre></x>

[PlainVariable]: ../variables.md#syntax

## Semantics

To execute a `@for` rule `rule`:

* Let `from` be the result of evaluating the expression in `FromDeclaration`.

* If `rule` has a `ToDeclaration`:

  * Let `to` be the result of evaluating the expression in `ToDeclaration`.

  * Let `exclusive` be `true`.
  
* Otherwise:

  * Let `to` be the result of evaluating the expression in `ThroughDeclaration`.

  * Let `exclusive` be `false`.
  
* If `from` and `to` aren't numbers, throw an error.

* Let `to` be the result of [converting] `to` to `from`'s unit allowing unitless.

  [converting]: ../types/number.md#converting-a-number-to-a-unit

* If `from` and `to` aren't integers, throw an error.

* If `from` is greater than `to`, set `direction` to `-1`. Otherwise, set
  `direction` to `1`.

* If `exclusive` is `false`, set `to` to `to + direction`.

* Let `i` be `from`.

* While `i` is not equal to `to`:

  * Let `scope` be a new [scope].

  * Add a variable with `rule`'s `VariableName` as its name and `i` as its value
    to `scope`.

    > Note that this variable will have the same unit that `from`.
  
  * Execute the `ForBlock`'s statements in `scope`.
  
  * Set `i` to `i + direction`.

  [scope]: ../variables.md#scope
