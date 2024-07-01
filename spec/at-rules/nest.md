# `@nest` Rule

Sass has special semantics for the `@nest` rule. Although this rule is primarily
intended to give the CSSOM a consistent representation for interleaved
declarations and is never required to be written explicitly, it *is* valid CSS
and Sass must ensure that its use preserves the existing CSS semantics.

## Syntax

<x><pre>
**NestRule** ::= '@nest'¹ '{' Statements '}'
</pre></x>

1: This is case-insensitive.

## Semantics

To execute a `@nest` rule `rule`:

* If there's a [current keyframe block], throw an error.

  [current keyframe block]: ../style-rules.md#current-keyframe-block

* If there's a [current style rule], evaluate each child in `rule`'s
  `Statement`s.

  [current style rule]: ../style-rules.md#current-style-rule

* Otherwise, [evaluate `rule` as an unknown at-rule] with
  `InterpolatedIdentifier` "nest", no `InterpolatedValue`, and the same
  `Statements`.

  [evaluate `rule` as an unknown at-rule]: ../at-rules/unknown.md
