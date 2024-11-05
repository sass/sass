# Statement

## Table of Contents

* [Definitions](#definitions)
  * [Current indentation level](#current-indentation-level)
  * [Document indentation character](#document-indentation-character)
* [Syntax](#syntax)
  * [ScssStatements](#scssstatements)
  * [IndentedStatements](#indentedstatements)
  * [Stylesheet](#stylesheet)
  * [Block](#block)
  * [Comments](#comments)
    * [LoudComment](#loudcomment)
    * [SilentComment](#silentcomment)
    * [WhitespaceComment](#whitespacecomment)
  * [Whitespace](#whitespace)
  * [Indentation](#indentation)

## Definitions

### Current indentation level

The *current indentation level* is the number of [documentation indentation
characters] before the first non-whitespace character of the last consumed
statement. The initial current indentation level for any document is 0.

> Lines that only contain whitespace or do not start a statement do not impact
> the current indentation level. Changes in the indentation level are used by
> the indented syntax to start and end blocks of statements.

[documentation indentation characters]: #document-indentation-character

### Document indentation character

The *document indentation character* is the first tab or space character used in
an [`IndentMore`] production in a document.

[`IndentMore`]: #indentation

> This is the character used for calculating the [current indentation level]. In
> the indented syntax, no character other than the document indentation
> character may be used for indentation.

[current indentation level]: #current-indentation-level

## Syntax

### ScssStatements

<x><pre>
**ScssStatements** ::= (Statement ';'?¹)* Statement?
</pre></x>

1: This production is mandatory unless the previous `Statement` is a
[`LoudComment`], [`SilentComment`], or ends in a `Block`.

[`LoudComment`]: #loudcomment
[`SilentComment`]: #silentcomment

If a [`WhitespaceComment`] would be ambiguous with a `Statement` in the
`ScssStatements` rule, parse it preferentially as a `Statement`.

[`WhitespaceComment`]: #whitespacecomment

### IndentedStatements

<x><pre>
**IndentedStatements** ::= (Statement [IndentSame])* Statement
</pre></x>

[IndentSame]: #indentation

The `Statement` productions may not include newlines outside of `IndentSame`
productions.

### Stylesheet

<x><pre>
**Stylesheet** ::= U+FEFF? ([ScssStatements] | [IndentedStatements])¹
</pre></x>

[ScssStatements]: #scssstatements
[IndentedStatements]: #indentedstatements

1: Only the production for the current syntax is valid.

### Block

<x><pre>
**ScssBlock**     ::= '{' [ScssStatements] '}'
**IndentedBlock** ::= [IndentMore] [IndentedStatements]
**Block**         ::= (ScssBlock | IndentedBlock)¹
</pre></x>

[IndentMore]: #indentation

1: Only the production for the current syntax is valid.

### Comments

#### LoudComment

<x><pre>
**ScssLoudComment**         ::= '/\*' (.\*¹ | Interpolation)\* '\*/'
**InterpolatedCommentText** ::= (.\*² | Interpolation)\*
**IndentedLoudChildren**    ::= (InterpolatedCommentText [IndentSame])\*
&#32;                           InterpolatedCommentText
**IndentedLoudComment**     ::= '/\*' InterpolatedCommentText
&#32;                           ([IndentMore] IndentedLoudChildren)?
**LoudComment**             ::= (ScssLoudComment | IndentedLoudComment)³
</pre></x>

1: This may not contain `#{` or `*/`.

2: This may not contain `#{` or newlines.

3: Only the production for the current syntax is valid.

#### SilentComment

<x><pre>
**ScssSilentComment**      ::= '//' .\*¹
**CommentText**            ::= .\*¹
**IndentedSilentChildren** ::= (CommentText [IndentSame])\* CommentText
**IndentedSilentComment**  ::= '//' CommentText
&#32;                          ([IndentMore] IndentedSilentChildren)?
**SilentComment**          ::= (ScssSilentComment | IndentedSilentComment)²
</pre></x>

1: This may not contain newlines.

2: Only the production for the current syntax is valid.

#### WhitespaceComment

<x><pre>
**WhitespaceComment** ::= ('//' .\*¹) | ('/\*' .\*² '\*/')
</pre></x>

1: This may not contain newlines.

2: This may not contain `*/`. In the indented syntax, this may not contain
   newlines.

### Whitespace

<x><pre>
**LineBreak**  ::= CarriageReturn | LineFeed | FormFeed
**Whitespace** ::= LineBreak¹ | Space | Tab | [WhitespaceComment]
</pre></x>

1: This is not allowed in the indented syntax.

[WhitespaceComment]: #whitespacecomment

### Indentation

<x><pre>
**WhitespaceOnlyLine** ::= [Whitespace]\* [LineBreak]
**IndentSame**         ::= [LineBreak] WhitespaceOnlyLine\*
&#32;                      IndentCharacter{ Current }
**IndentCharacter**    ::= Space | Tab
**IndentMore**         ::= WhitespaceOnlyLine\* [LineBreak]
&#32;                      IndentCharacter{ ≥ Current + 1 }
</pre></x>

[Whitespace]: #whitespace
[LineBreak]: #whitespace

The IndentCharacter must be the [document indentation character].

[document indentation character]: #document-indentation-character

`Current` is the [current indentation level] for a document. After consuming an
`IndentSame` or `IndentMore` production, the [current indentation level] is set
to the number of IndentCharacters found.
