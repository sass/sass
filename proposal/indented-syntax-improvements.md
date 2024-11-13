# Indented Syntax Improvements

*([Issue](https://github.com/sass/sass/issues/216))*

This proposal improves the indented syntax format, allowing multiline
expressions, semicolons, and curly braces.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Line breaks](#line-breaks)
  * [Semicolons](#semicolons)
  * [SCSS-in-Sass](#scss-in-sass)
  * [Ending statements](#ending-statements)
    * [After a SassScript value](#after-a-sassscript-value)
    * [After a non-enclosed list begins](#after-a-non-enclosed-list-begins)
    * [At-rules](#at-rules)
  * [Design Decisions](#design-decisions)
* [Syntax](#syntax)
  * [IndentedStatements](#indentedstatements)
  * [Block](#block)
  * [WhitespaceComment](#whitespacecomment)
  * [Whitespace](#whitespace)

## Background

> This section is non-normative.

The indented `.sass` syntax uses whitespace to end statements (linebreaks) and
indicate nesting (indentation). While this syntax is preferred by some authors,
it presents authoring challenges, specifically around long lists.

## Summary

> This section is non-normative.

This proposal changes where line breaks cause statement breaks, allows the use
of semicolons, and adds a method to opt-in to SCSS formatting to the indented
syntax.

### Line breaks

In the Sass indented syntax, line breaks always end a statement, which limits
how authors can format their code. However, the parser can tell from context
whether a statement can end at a given point. This document proposes that line
breaks only end statements when a statement can end, and in any other case, a
line break is treated as continuing white space.

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

In the indented syntax outside of curly braces, authors can use a semicolon to
explicitly end a statement. Subsequent lines in the same block still must have
the same level of indentation. This means that the indented format won't support
multiple statements on a single line, even if they are separated by a semicolon.

```sass
$font-stack: Helvetica, sans-serif;
// or
$primary-color: #333
```

### SCSS-in-Sass

Alternatively, authors can use SCSS block syntax with braces and semicolons
within an indented document.

```sass
a
  color: blue

.grid {
  display: grid;
  grid-template:
    'logo title copy' auto
    'alert alert alert' minmax(0, auto) / 1fr 1fr 1fr;
}
```

The braces essentially let authors opt in to SCSS format for part of the
document. This means that authors must use semicolons to separate statements
inside that block. Indentation rules do not apply and multiple statements can be
on a single line, separated by semicolons.

```sass
$font-stack: Helvetica, sans-serif
$primary-color: #333
body {
  font: 100% $font-stack;
  color: $primary-color; background: blue;
}
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

A line break in a list that is not in a `BracketedListExpression` or enclosed in
`()` must cause a statement break.

`$var: 1 2\n3` and `$var: 1, 2\n, 3` can not be parsed to determine when the
statement has ended. Alternates `$var: (1 2\n3)`, `$var: [1 2\n3]`, and `$var:
(1, 2,\n 3)` can be parsed.

Existing syntax allows for trailing commas in Sass lists. However, comma
separated lists can not use a trailing comma to signify that a list will
continue after the line break, as this would break existing stylesheets with
trailing commas.

Because arguments to functions and mixins are already wrapped in `()`, line
breaks in arguments do not need to cause a statement break. Interpolations are
wrapped in `#{}` so line breaks do not need to end statements.

#### At-rules

For any at-rule that is supported by native CSS, line breaks after the `@` and
before a block or statement are not supported. An exception is that line breaks
are supported inside parentheses. This includes
`@import`, (which overlaps with Sass), `@supports`, `@media`, `@keyframes` and
any unknown at-rule.

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

Instead of just allowing optional curly braces around indented syntax, we opted
to require SCSS syntax inside of the braces. This is intentionally
unidirectional to prevent issues with multiple nested formats.

## Syntax

### IndentedStatements

<x><pre>
**IndentedStatements**  ::= (Statement ';'? [IndentSame])\* Statement ';'?
</pre></x>

[IndentSame]: ../spec/statement.md#indentation

Remove the following prose:

The `Statement` productions may not include newlines outside of `IndentSame`
productions.

### Block

Replace footnote 1 with:

1: In the SCSS syntax, only the `ScssBlock` production is valid.

### WhitespaceComment

In footnote 2, remove:

In the indented syntax, this may not contain newlines.

### Whitespace

Replace footnote 1 with:

1. In the indented syntax, [`LineBreak`] is not whitespace in the
   `ImportAtRule`, `SupportsAtRule`, [`MediaAtRule`], `KeyframesAtRule`, or
   [`UnknownAtRule`], except inside of parentheses or square brackets as defined
   by CSS syntax. If a [`LineBreak`] in [`Whitespace`] would cause it to be
   ambiguous with an [`IndentSame`] or [`IndentMore`] production, parse it
   preferentially as [`IndentSame`] or [`IndentMore`].

[`MediaAtRule`]: ../spec/at-rules/media.md
[`UnknownAtRule`]: ../spec/at-rules/unknown.md
[`LineBreak`]: ../spec/statement.md#whitespace
[`Whitespace`]: ../spec/statement.md#whitespace
[`IndentSame`]: ../spec/statement.md#indentation
[`IndentMore`]: ../spec/statement.md#indentation
