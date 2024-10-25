## Declarations

## Table of Contents

* [Syntax](#syntax)
* [Definitions](#definitions)
* [Current Declaration Name](#current-declaration-name)
* [Semantics](#semantics)

## Syntax

<x><pre>
**Declaration**         ::= StandardDeclaration | CustomDeclaration
**StandardDeclaration** ::= [InterpolatedIdentifier]¹ ':' (Value | Value? Block )
**CustomDeclaration**   ::= [InterpolatedIdentifier]² ':' InterpolatedDeclarationValue
</pre></x>

1. This may not begin with "--".
2. This *must* begin with "--".

[InterpolatedIdentifier]: syntax.md#interpolatedidentifier

## Definitions

### Current Declaration Name

The *current declaration name* is the innermost value declared as such when
[executing a declaration].

[executing a declaration]: #semantics

### Semantics

To execute a declaration `declaration`:

* Let `parent-name` be the [current declaration name], if one exists.

  [current declaration name]: #current-declaration-name

* If `name` is set and `declaration` is a [`CustomDeclaration`], throw an error.

  [`CustomDeclaration`]: #syntax

* Let `name` be the result of evaluating the all interpolation in
  `declaration`'s name.

* If `parent-name` exists, set `name` to `parent-name + "-" + name`.

* Declare `name` as the [current declaration name] for the duration of executing
  `declaration`.

* If `declaration` has a `Value`:

  * Let `value` be the result of evaluating that `Value`.

* Otherwise, if `declaration` is a `CustomDeclaration`:

  * Let `value` be an unquoted string whose value is the result of evaluating
    `declaration`'s `InterpolatedDeclarationValue`.

  * If `value` is empty, throw an error.

    > Note that `value` being only whitespace is allowed, including `--foo: ;`.

* Let `parent` be the [current style rule], [keyframe block], or at-rule; or
  the innermost if multiple exist.

  [current style rule]: style-rules.md#current-style-rule
  [keyframe block]: style-rules.md#current-style-rule

  > Parsing guarantees that a declaration will have at least one parent.

* If `value` is set, and it's neither null nor an empty unquoted string:

  * Let `css` be a CSS declaration with name `name` and value `value`.

  * Append `css` to `parent`.
  
  * If `parent` isn't the last statement in its parent:

    * Let `copy` by a copy of `parent` without any children.

    * Append `copy` to `parent`'s parent.

    * Set the [current style rule], [keyframe block], or at-rule (according to
      `copy`'s type) to `copy`, for the remaining duration of its previous value.

    * Set `parent` to `copy`.

* Evaluate each child in `declaration`'s `Statements` if it exists.
