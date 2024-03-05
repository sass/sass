# Numbers

## Table of Contents

* [Definitions](#definitions)
  * [List](#list)
  * [List Value](#list-value)
  * [Index](#index)

## Definitions

### List

A *SassScript list* (usually referred to as just a *list*) is an ordered
sequence of SassScript values. A list may or may not be *bracketed*, and a list
has a *separator* which is one of "space", "comma", "slash", or "undecided".
Only lists with zero or one elements may have an "undecided" separator.

### List Value

A SassScript value's *list value* is the interpretation of that value as a
SassScript list. This differs from type to type:

* The list value of a list is the list itself.
* The list value of a map is an unbracketed comma-separated list whose elements
  are the key/value pairs in the map as two-element unbracketed space-separated
  lists.
* The list value of any other value is an unbracketed undecided-separator list
  containing only that value.

### Index

An *index* is a unitless [integer] that refers to a specific location in a list.
Positive integers count from the beginning of the list, and negative integers
count from the end of the list. The referenced value is said to be *indexed by*
the index. An integer is an *invalid index* for a given list if it's 0 or if its
absolute value is larger than the length of that list.

> For example, in the values in the list `["a", "b", "c"]` are referred to by
> the following indices:
>
> * `"a"`: 1, -3
> * `"b"`: 2, -2
> * `"c"`: 3, -1

[integer]: number.md#integer

## Syntax

<x><pre>
**BracketedListExpression** ::= '[' ContainedListExpression ']'
**ContainedListExpression** ::= CommaListExpression ','?
**CommaListExpression**     ::= SlashListExpression (',' SlashListExpression)*
**SlashListExpression**     ::= SpaceListExpression ('/' SpaceListExpression)*
**SpaceListExpression**     ::= SumExpression+
</pre></x>

> Note that `/` may *not* be used in single-element lists the way `,` is. That
> is, `(foo,)` is valid, but `(foo/)` is not.
>
> This defines `/` to bind tighter than `,` but looser than space-separated
> lists. This was chosen because most common uses of `/` in CSS conceptually
> bind looser than space-separated values. The only exception is the [`font`
> shorthand syntax], which is used much more rarely will still work (albeit with
> an unintuitive SassScript representation) with a loose-binding `/`.
>
> [`font` shorthand syntax]: https://developer.mozilla.org/en-US/docs/Web/CSS/font

## Semantics

### `SlashListExpression`

To evaluate a `SlashListExpression`, evaluate each of its `SpaceListExpression`s
and return a slash-separated list that contains each of the results in order.


