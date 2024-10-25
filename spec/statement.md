# Statement

## Table of Contents

* [Definitions](#definitions)
  * [Current indentation level](#current-indentation-level)
  * [Document indentation character](#document-indentation-character)
* [Syntax](#syntax)
  * [ScssStatements](#scssstatements)
  * [IndentedStatements](#indentedstatements)
  * [Statements](#statements)
  * [Block](#block)
  * [Whitespace](#whitespace)
  * [Indentation](#indentation)

## Definitions

### Current indentation level

The `current indentation level` is the count of the [`documentation indentation
character`] between a new line and any non-whitespace characters of the last
consumed statement. Lines that only contain whitespace or do not start a
statement do not impact the `current indentation level`.

A document parsed in the indented syntax may not begin with whitespace, and the
initial `current indentation level` for any document is `0`.

> Changes in the indentation level are used by the indented syntax to start and
> end blocks of statements.

[`documentation indentation character`]: #document-indentation-character

### Document indentation character

The `document indentation character` is defined as the first tab or space
character used as an [`IndentMore`] production in a document.

[`IndentMore`]: #indentation

> The `document indentation character` is the character used for calculating the
> [`current indentation level`]. In the indented syntax, no character other than
> the `document indentation character` may be used for indentation.

[`current indentation level`]: #current-indentation-level

## Syntax

### ScssStatements

<x><pre>
**ScssStatements**      ::= (Statement ';'?¹)* Statement?
</pre></x>

1: This production is mandatory unless the previous `Statement` is a
`LoudComment` or `SilentComment`, or after the final production in a
`ScssStatements` production, which is either at the end of a `Block` or the end
of the document.

If a `WhitespaceComment` would be ambiguous with a `Statement` in the
`ScssStatements` rule, parse it preferentially as a `Statement`.

### IndentedStatements

<x><pre>
**IndentedStatements**  ::= (Statement IndentedSame?¹)* Statement?
</pre></x>

1: This production is mandatory unless the previous `Statement` is a
`LoudComment` or `SilentComment`, or after the final production in a
`IndentedStatements` production, which is either at the end of a `Block` or the
end of the document.

If a `WhitespaceComment` would be ambiguous with a `Statement` in the
`IndentedStatements` rule, parse it preferentially as a `Statement`.

### Statements

<x><pre>
**Statements**¹          ::= [ScssStatements] | [IndentedStatements]
</pre></x>

[ScssStatements]: #scssstatements
[IndentedStatements]: #indentedstatements

1. Only the production for the current syntax is valid.

### Block

<x><pre>
**ScssBlock**      ::= '{' [ScssStatements] '}'
**IndentedBlock**  ::= [IndentMore] [IndentedStatements] [IndentLess]
**Block**          ::= ScssBlock | IndentedBlock¹
</pre></x>

[IndentMore]: #indentation
[IndentLess]: #indentation

1: Only the production for the current syntax is valid.

### Whitespace

> Whitespace separates productions inside a statement when disambiguation is
> necessary. The Whitespace productions do not separate Statements.

<x><pre>
**LineBreak**               ::= CarriageReturn | LineFeed | FormFeed
**ScssWhitespace**          ::= LineBreak | Space | Tab
**IndentedWhitespace**      ::= Space | Tab
</pre></x>

### Indentation

<x><pre>
**IndentCharacter**         ::= Space | Tab
**IndentSame**              ::= [LineBreak] [IndentCharacter]{ Current }
**IndentLess**              ::= [LineBreak] [IndentCharacter]{ 0, Current - 1 }
**IndentMore**              ::= [LineBreak] [IndentCharacter]{ Current + 1, }
</pre></x>

[LineBreak]: #whitespace
[IndentCharacter]: #whitespace

The [IndentCharacter] must be the [document indentation character].

[document indentation character]: #document-indentation-character

`Current` is the [current indentation level] for a document. After consuming an
`IndentSame`, `IndentLess`, or `IndentMore` production, the [current indentation
level] is set to the number of [IndentCharacter]s found. At the end of a
document, the [current indentation level] is set to 0.

[current indentation level]: #current-indentation-level
