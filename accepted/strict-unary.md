# Strict Unary Operators: Draft 1

*([Issue](https://github.com/sass/sass/issues/1912))*

This proposal forbids the syntax `$a -$b`, which surprises many users by parsing
equivalently to `$a - $b` instead of `$a (-$b)`. It does the same for `+`, which
also has a unary form.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Alternatives Considered](#alternatives-considered)
  * [Spaces on Both Sides](#spaces-on-both-sides)
* [Syntax](#syntax)
* [Deprecation Process](#deprecation-process)

## Background

> This section is non-normative.

Sass is in an awkward syntactic position. On one hand, it's beholden to CSS's
syntax, including the frequent use of space-separated lists of values. On the
other, it wants to provide mathematical operations in a naturalistic way that
matches user expectations from other programming languages and from everyday
mathematical notation.

This is a particular problem when dealing with operators like `-` and `+` that
can be both *binary*, appearing between two operands like `$a - $b` or `$a +
$b`; or *unary*, appearing before a single operand like `-$a` or `-$b`. In most
programming languages it's possible to parse both of these unambiguously with
any combination of whitespace, but in Sass a construct like `$a -$b` could be
reasonably parsed as either the binary operation `$a - $b` or the unary
operation `$a (-$b)`.

In practice, we chose to parse it as the binary operation under the logic that
whitespace shouldn't affect the parsing of operators. This logic is sound in
isolation, but in practice it produces surprising and unpleasant behavior for
users.

## Summary

> This section is non-normative.

We will address the confusion by forbidding the ambiguous case entirely. Any
expression of the form `$a -$b` or `$a +$b` will produce an error that will
suggest the user disambiguate by either writing `$a - $b` or `$a (-$b)`, which
clearly represent the intention to use a binary or unary operator, respectively.
Other constructs such as `($a)-$b` will still be allowed.

As with any breaking change, we will begin by deprecating the old behavior.
Since this isn't a CSS compatibility issue, the breaking change won't land until
the next major revision of each implementation.

## Alternatives Considered

> This section is non-normative.

### Spaces on Both Sides

We considered the possibility of requiring spaces on *both* sides of binary
operators, so that `($a)-$b` would also be forbidden. However, this form is much
more likely to be interpreted as a binary operator by users, and we want to
limit how much behavior we deprecate as much as possible.

## Syntax

This proposal modifies the existing `SumExpression` production to forbid
this particular case:

<x><pre>
**SumExpression** ::= (SumExpression ('+' | '-')¹)? ProductExpression
</pre></x>

1: If there's whitespace before but not after the operator, emit a syntax error.

## Deprecation Process

Before an implementation releases its next major version, it should emit a
deprecation warning instead of a syntax error.
