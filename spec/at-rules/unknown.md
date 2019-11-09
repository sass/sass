# Unknown At-Rules

In order to be flexible in its compatibility with future additions to CSS, Sass
supports *all* at-rule names with a default syntax that's highly liberal in the
structures it allows. It uses the following grammar:

<x><pre>
**UnknownAtRule** ::= '@' [InterpolatedIdentifier][] InterpolatedValue?
&#32;                   ('{' Statements '}')?
</pre></x>

[InterpolatedIdentifier]: ../syntax.md#interpolatedidentifier

No whitespace is allowed after `@`. As with all statements, an `UnknownAtRule`
without a block must be separated from other statements with a semicolon.

When an at-rule is executed, its name is evaluated to produce an unquoted string
which is used as the name of the generated at-rule. Then that generated name is
checked to see if it's an at-rule that has special runtime handling.

> Note that only `@keyframes` has special runtime handling that's triggered
> here. Other CSS at-rules that Sass handles specially, like `@media` or
> `@supports`, are detected at parse-time. This means that `@m#{ed}ia` will be
> treated as an unknown at-rule rather than a media rule.
