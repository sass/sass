# Indented Syntax Improvements: Draft 1.1

*([Issue](https://github.com/sass/sass/issues/216))*

This proposal improves the indented syntax format, allowing multiline
expressions and semicolons.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Line breaks](#line-breaks)
  * [Semicolons](#semicolons)
  * [Ending statements](#ending-statements)
    * [After a SassScript value](#after-a-sassscript-value)
    * [After a non-enclosed list begins](#after-a-non-enclosed-list-begins)
    * [At-rules](#at-rules)
  * [Design Decisions](#design-decisions)
* [Syntax](#syntax)
  * [IndentedStatements](#indentedstatements)
  * [WhitespaceComment](#whitespacecomment)
  * [Whitespace](#whitespace)

## Background

> This section is non-normative.

The indented `.sass` syntax uses whitespace to end statements (linebreaks) and
indicate nesting (indentation). While this syntax is preferred by some authors,
it presents authoring challenges, specifically around long lists.

## Summary

> This section is non-normative.

This proposal adds support for multiline statements semicolons in the indented
syntax.

### Line breaks

In the Sass indented syntax, line breaks always end a statement, which limits
how authors can format their code. However, the parser can tell from context
whether a statement can end at a given point. This document proposes that line
breaks only end statements when a statement can end, and in any other case, a
line break is treated as non-meaningful white space.

In this example, statements can not end after `in`, inside the interpolation, or
after the multiplication operator, so those line breaks will not end the
statement.

```sass
@each $item in 
    1 2 3
  .item-#{
    $item
  }
    content: $item *
        10
````

It will also be possible to include line breaks by wrapping the expression in
parentheses.

```sass
@font-face
  font-family: "Leatherman"
  src: (
    local("Leatherman"),
    url("leatherman-COLRv1.otf") format("opentype") tech(color-COLRv1),
    url("leatherman-outline.otf") format("opentype"),
    url("leatherman-outline.woff") format("woff")
  )
```

### Semicolons

In the indented syntax, authors can use a semicolon to explicitly end a
statement. Subsequent lines in the same block still must have the same level of
indentation. This means that the indented format won't support multiple
statements on a single line, even if they are separated by a semicolon.

```sass
$font-stack: Helvetica, sans-serif;
// or
$primary-color: #333
```

### Ending statements

When line breaks do not end statements, the line breaks are treated as white
space, and do not have any requirements about the amount of indentation. The
next statement's indentation is compared to the indentation at the start of the
current statement to determine nesting or block closure.

By design, line breaks are ignored as meaningless white space except in
contexts where the semantics define that a statement may end. When an author
inserts a line break they intend to be meaningless in a position where a
statement may end, the statement will end, and parsing will most likely fail on
the subsequent text. The following places are where line breaks will cause
statements to end.

#### After a SassScript value

In a simple declaration, `$foo: bar \n`, the line break must cause the statement
to end.

This may be surprising in more complex situations, for instance, with binary
operators. `$foo: 3\n+ 4\n` ends the statement after `3`, but `$foo: 3 +\n4\n`
ends the statement after `4`. Wrapping with parentheses allows authors more
flexibility with `$foo: (3\n+ 4)`.

This also applies to flow control at-rules. `@if $a \n and $b` would end the
statement after `$a`, but `@if ($a \n and $b)` can be parsed.

#### After a non-enclosed list begins

A line break in a list that is not enclosed in `[]` or `()` must cause a
statement break.

`$var: 1 2\n3` and `$var: 1, 2\n, 3` can not be parsed to determine when the
statement has ended. Alternatives `$var: (1 2\n3)`, `$var: [1 2\n3]`, and `$var:
(1, 2,\n 3)` can be parsed.

Comma separated lists can not use a trailing comma to signify that a list will
continue after the line break. Trailing commas are allowed in Sass lists, so
this would break existing stylesheets with trailing commas.

Because arguments to functions and mixins are already wrapped in `()`, line
breaks in arguments do not need to cause a statement break. Interpolations are
wrapped in `#{}` so line breaks do not need to end statements.

#### At-rules

For any at-rule that is supported by native CSS, line breaks after the `@` and
before a block or statement are not supported. An exception is that line breaks
are supported inside parentheses. This includes `@import`, (which overlaps with
Sass), `@supports`, `@media`, `@keyframes` and any unknown at-rule.

These rules should be emitted as is, with no special handling from Sass,
including all parentheses.

### Design Decisions

While some CSS at-rules may have contexts where a line break would not be
meaningful, custom handling of line breaks is outside of the scope of this
proposal. For instance, `@media (hover: hover) and \n (color)` is not supported,
even though line breaks do not end statements after boolean operators in general
SassScript. This avoids a requirement for Sass to add custom support for new CSS
at-rules.

We considered borrowing alternate syntax from other languages, such as a leading
`>` or `|` from YAML or a trailing `|` from HAML. These introduce syntax that is
novel to Sass, and we instead opted to borrow syntax from the SCSS format. It
also could introduce incompatibilities with future CSS features.

This proposal introduces the changes for all authors using the indented syntax,
as opposed to introducing a separate syntax or compiler flag. As the proposal is
addititive to existing syntax, authors can choose to not use the new syntax, and
can choose to limit the syntax with linters.

The initial draft of this proposal included a SCSS-in-Sass syntax, where rules
inside curly braces in the indented syntax would be parsed with the SCSS syntax.
This was removed due to feedback from tooling authors in
[#3991](https://github.com/sass/sass/issues/3991).

## Syntax

### IndentedStatements

<x><pre>
**IndentedStatements**  ::= (Statement ';'? [IndentSame])\* Statement ';'?
</pre></x>

[IndentSame]: ../spec/statement.md#indentation

Remove the following prose:

The `Statement` productions may not include newlines outside of `IndentSame`
productions.

### WhitespaceComment

In footnote 2, remove:

In the indented syntax, this may not contain newlines.

### Whitespace

Replace footnote 1 with:

1. In the indented syntax, [`LineBreak`] is not whitespace in the
   `ImportAtRule`, `SupportsAtRule` or [`MediaAtRule`], except inside of
   parentheses or square brackets as defined by CSS syntax. If a [`LineBreak`]
   in [`Whitespace`] would cause it to be ambiguous with an [`IndentSame`] or
   [`IndentMore`] production, parse it preferentially as [`IndentSame`] or
   [`IndentMore`].

[`MediaAtRule`]: ../spec/at-rules/media.md
[`LineBreak`]: ../spec/statement.md#whitespace
[`Whitespace`]: ../spec/statement.md#whitespace
[`IndentSame`]: ../spec/statement.md#indentation
[`IndentMore`]: ../spec/statement.md#indentation
