# Interpolation in At-Rule Names: Draft 1

*([Issue](https://github.com/sass/sass/issues/429))*

This proposal defines support for interpolation in at-rule names.

## Table of Contents

* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Syntax](#syntax)

## Summary

> This section is non-normative.

Any at-rule may include interpolation in its name. This interpolation
automatically causes it to be parsed as an unknown at-rule, even if the resolved
name of the rule is known to Sass. The only exception is the `@keyframes` rule,
which will still allows sub-rules with keyframes selectors such as `10%`.

### Design Decisions

It would be possible to treat CSS at-rules that Sass knows about, like `@media`,
specially even when they're generated from interpolated at-rule names. However,
this would add a lot of implementation complexity, since implementations would
need to be able to re-parse those at-rules' values at runtime.

It's also unclear that there's any value to be gained in return for this
complexity. At-rule interpolation is primarily useful for adding vendor
prefixes, and the two CSS at-rules that Sass has special support for (`@media`
and `@supports`) don't use vendor prefixes.

## Syntax

This proposal defines a replacement for the production `UnknownAtRule`. The
grammar for this production is:

<x><pre>
**UnknownAtRule** ::= '@' InterpolatedIdentifier InterpolatedValue?
&#32;                   ('{' Statements '}')?
</pre></x>

No whitespace is allowed after `@`. As with all statements, an `UnknownAtRule`
without a block must be separated from other statements with a semicolon.

> Note that this is the same as the previous syntax, except that the at-rule's
> name can be interpolated.

When an at-rule is evaluated, its name is evaluated to produce an unquoted
string which is used as the name of the generated at-rule. Then that generated
name is checked to see if it's an at-rule that has special runtime handling.

> Note that only `@keyframes` has special runtime handling that's triggered
> here. Other CSS at-rules that Sass handles specially, like `@media` or
> `@supports`, are detected at parse-time. This means that `@m#{ed}ia` will be
> treated as an unknown at-rule rather than a media rule.
