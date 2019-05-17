# Normalizing Free Interpolation in SassScript

*([Issue](https://github.com/sass/sass/issues/1778))*

Spurred by [#1774][], I started thinking about how messy interpolation is in
SassScript at the moment and how to clean it up. This issue is the result of
that thought process.

[#1774]: https://github.com/sass/sass/issues/1774

## Table of Contents

* [History](#history)
* [Proposal](#proposal)
* [Design decisions](#design-decisions)
  * [Interpolation in unquoted strings](#interpolation-in-unquoted-strings)
  * [Interpolation outside of strings](#interpolation-outside-of-strings)
* [Deprecation process](#deprecation-process)
  * [Old interpolation rules](#old-interpolation-rules)
  * [Deprecation warnings](#deprecation-warnings)
    * [CSS-equivalent values](#css-equivalent-values)
    * [Different stringifications](#different-stringifications)
    * [Adjacent expressions](#adjacent-expressions)
    * [Quoted strings](#quoted-strings)
    * [Same stringifications with different types](#same-stringifications-with-different-types)

## History

Long ago, when only the indented syntax existed, SassScript couldn't be used
directly in property values. In order to give properties dynamically-generated
values, they had to be interpolated using `#{}`. Eventually, we figured out how
to make SassScript compatible enough with CSS property values that we decided to
just let properties use it directly. For backwards compatibility, these
properties still needed to support interpolation, so we came up with a way to
have interpolation work more or less anywhere in a SassScript expression.

Unfortunately, working "more or less anywhere" was a parsing nightmare, and the
specifics of where interpolation can be used and its effect on the surrounding
script are bizarre and arcane. Chris and I want to fix that by substantially
limiting the places `#{}` can appear and clarifying exactly what it does to the
surrounding script.

## Proposal

I propose that, as today, `#{}` be allowed either within strings (quoted or
unquoted) or on its own. However, its effect will be limited to the strings that
contain it or to its own value. Specifically:

- When parsing or evaluating a quoted string, treat interpolation the same way
  it's treated today.
- When parsing an identifier, treat interpolation as though it's an alphabetic
  character. When evaluating an interpolated unquoted string, concatenate the
  literal identifier characters with the values of the interpolated segments.
- Otherwise, parse an interpolation as an individual expression. When evaluating
  it, return its value as an unquoted string.

Here are some examples (I'm including quotes for unquoted strings in the output
to clarify their extents):

- `"a #{b} c"` would continue to produce `"a b c"`.
- `a#{b}c` would continue to produce `"abc"`.
- `a #{b}c` currently produces `"a bc"` but would produce `"a" "bc"`.
- `a#{b} c` currently produces `"ab c"` but would produce `"ab" "c"`.
- `a b#{c}d e` currently produces `"a bcd e"` but would produce `"a" "bcd" "e"`.
- `a #{b} c` currently produces `"a b c"` but would produce `"a" "b" "c"`.

## Design decisions

The primary question when figuring out how to handle this was how much
interpolation should be restricted. Chris and I agree that interpolation in
SassScript reads strangely in many situations, but we ended up deciding to
continue allowing it in most places. One major reason for this is
backwards-compatibility: no matter what we do, the process of making this change
will be painful, and any functionality we can preserve will help mitigate that
pain. But there were also compelling use cases for retaining interpolation in
various situations.

### Interpolation in unquoted strings

It was tempting to restrict interpolation for use _only_ in quoted strings.
Interpolation in unquoted strings can be mimicked using `+`, and allowing it in
unquoted strings could produce the incorrect impression that interpolation is
performed before any other SassScript resolution. However, we decided to allow
this for several reasons:

- Backwards compatibility, as described above.
- Similarity with quoted strings. It's not always obvious that unquoted strings
  and quoted strings are the same sorts of value under the hood, but sharing
  capabilities helps reinforce that idea.
- Similarity with other identifiers. Interpolation can be used in almost all
  most non-SassScript contexts where identifiers appear, most notably property
  names, so it's natural that users would think that all Sass identifiers can be
  interpolated.
- Vendor prefixes. It would be very difficult to dynamically choose vendor
  prefixes for function names or other values, since `-` on its own is not an
  identifier.
- Aesthetics. Although `font-stretch: $amount + -condensed` is legal, it's less
  clear and less pleasant than `font-stretch: #{$amount}-condensed`.

### Interpolation outside of strings

The other big decision was whether to allow a bare interpolation expression that
wasn't attached to any string at all. Both of us were fine with deprecating this
until we remembered one situation where it's by far the best solution: a slash
delimited. Right now when users want a slash delimiter for the values of
properties such as `font`, and they want one of its values to be dynamic, by far
the best way to do that is with interpolation: `font: 12pt/#{$var} sans-serif`.

We considered coming up with a new way to produce a literal slash without using
interpolation, but we didn't find anything that was clear enough to warrant the
migration cost for all the stylesheets using the current method. In the end, we
decided that since the current method looks pretty decent and can work with a
more reasonable definition of standalone interpolation, we would leave it as-is.

## Deprecation process

Any change we make here will be backwards-incompatible. Since interpolation is
such an old feature, we have to be very careful to only surface deprecation
warnings to people whose stylesheet semantics will actually change (or as close
as possible), and to provide them with actionable ways to fix those stylesheets.
This is complicated by the fact that the effects of this change are difficult to
reason about locally; an expression like `a #{b} c` remains valid, and whether
it's problematic in practice depends on things like whether its value is used in
some string-specific way.

Let S1 be the value of an expression containing interpolation under the old
rules, and E the value of the same expression under the new rules. Let S2 be the
conversion of E to CSS. For example, suppose the expression in question is
`a #{b} + c`. S1 is `"a b + c"`, E is `"a" "bc"`, and S2 is `"a bc"`.

- If S1 and S2 aren't semantically identical when interpreted as CSS, issue a
  warning. This means that `#{a} + b` would emit a warning since S1 is `"a + b"`
  but S2 is `"ab"`. However, `#{a} b c` would not emit a warning, since S1 and
  S2 are both `"a b c"`. Note that an expressions like `#{a} / b` _should not_
  emit a warning here, since we know that it will produce `a/b` under the new
  semantics.
- Otherwise, if E is not a string, set an "interpolated" flag on S1. If any
  operation is performed on S1 that wouldn't first convert it to a string, emit
  a warning.

Formalizing this requires a more explicit notion of how to detect when S1 and S2
are CSS-semantically identical, and how to tell which operations would be a
problem in the second case, which we'll get to below.

### Old interpolation rules

In service of determining how to go about deprecating the current semantics of
SassScript interpolation, I want to precisely define them. For our purposes, we
only care about _free interpolation_—that is, interpolation outside the context
of a string or a special function (e.g. `calc()`) that's parsed like a string.

The grammar for interpolation is straightforward. Note that the representation
below elides much of the unrelated complexity of the SassScript grammar. The
`Operation` and `UnaryOperation` productions should be understood to encompass
all binary and unary operations supported by SassScript, except for `,` which is
handled by the `CommaList` production. Note that this _includes_ the implicit
adjacency operator that normally creates space-separated lists. `Value` should
be understood to encompass literals, parenthesized expressions, maps, and
function calls.

```
CommaList      ::= Operation (',' Operation)*

Operation      ::= UnaryOperation ('•' UnaryOperation)*

UnaryOperation ::= '~'? Expression

Expression     ::= Value | Interpolation

Interpolation  ::= '#{' CommaList '}'
```

The complexity lies in how this representation is evaluated. Because the
semantics of string interpolation is already clear, I'll describe the evaluation
of free interpolation in terms of its **equivalent string interpolation** (or
"ESI" for short). To clarify that the strings returned by the ESI should be
unquoted, I'll use backticks instead of double quotes to delimit them (so
``` `foo` ``` would have the same value as `unquote("foo")`).

The ESI for an `Interpolation` production is, predictably,
``` `#{CommaList}` ```.

Similarly, for a `UnaryOperation` with an operator and an `Interpolation`
operand, the ESI is ``` `~` + ESI(Interpolation) ``` = ``` `~#{CommaList}` ```.
If there was any whitespace in the source text between the operator and the
`Interpolation`, a single space is also added after the operator in ESI.

| SassScript | ESI | CSS |
| --- | --- | --- |
| `-1` | `-1` | `-1` |
| `- 1` | `-1` | `-1` |
| `-#{1}` | ``` `-#{1}` ``` | `-1` |
| `- #{1}` | ``` `- #{1}` ``` | `- 1` |

For an `Operation` production, all _adjacent_ `UnaryOperation` sub-expressions
that are _not_ `Interpolation`s are parsed as normal, and interpolated into the
ESI alongside the `Interpolation` subexpressions, separated by the operation in
question. As with a `UnaryOperation`, a space will be included before or after
the `Interpolation`s depending on whether whitespace appeared in the
corresponding location in the source. This is also what allows interpolation in
identifiers to work, since adjacent expressions are considered to implicitly
have a space operator.

| SassScript | ESI | CSS |
| --- | --- | --- |
| `1 + 2 + 3` | `1 + 2 + 3` | `6` |
| `1 + #{2} + 3` | ``` `#{1} + #{2} + #{3}` ``` | `1 + 2 + 3` |
| `1 +#{2}+ 3` | ``` `#{1} +#{2}+ #{3}` ``` | `1 +2+ 3` |
| `1 + 2 + #{3}` | ``` `#{1 + 2} + #{3}` ``` | `3 + 3` |
| `#{1} + 2 + 3` | ``` `#{1 + 2} + #{3}` ``` | `3 + 3` |
| `1 #{2} 3` | ``` `#{1} #{2} #{3}` ``` | `1 2 3` |
| `a#{b}c` | ``` `#{a}#{b}#{c}` ``` | `abc` |

Finally, `CommaList` productions behave almost the same as `Operation`s. The
only difference is that if _only_ the first `Operation` sub-expression is an
`Interpolation`, the rest of the list isn't included in the interpolation.

| SassScript | ESI | CSS |
| --- | --- | --- |
| `1, #{2}, 3` | ``` `#{1}, #{2}, #{3}` ``` | `1, 2, 3` |
| `1, 2, #{3}` | ``` `#{1, 2}, #{3}` ``` | `1, 2, 3` |
| `#{1}, 2, 3` | ``` `#{1}`, 2, 3 ``` | `1, 2, 3` |
| `#{1}, #{2}, 3` | ``` `#{1}, #{2}, #{3}` ``` | `1, 2, 3` |

### Deprecation warnings

Now that we (hopefully) have a clear idea of how free interpolation works right
now, we can start figuring out the surface area that needs deprecation warnings
when moving to the new semantics.

Ideally, we want to warn only when the new semantics will produce _semantically
different_ CSS output. In practice determining this exactly isn't always
feasible, since free interpolation produces values that can be used in many
heterogeneous ways, so instead we'll warn if the values they produce are ever
used in a way that will change behavior under the new semantics.

#### CSS-equivalent values

There are some expressions containing free interpolation whose values under both
the old and the new semantics are CSS-equivalent strings. These include
expressions where there are no operators (which also means no implicit list
operators), as well as expressions with operators that will produce strings with
identical semantics.

| SassScript | Old Value | New Value |
| --- | --- | --- |
| `#{a}` | ``` `a` ``` | ``` `a` ``` |
| `a#{b}c` | ``` `abc` ``` | ``` `abc` ``` |
| `+ #{a}` | ``` `+ a` ``` | ``` `+a` ``` |
| `/ #{a}` | ``` `/ a` ``` | ``` `/a` ``` |
| `1 / #{a}` | ``` `1 / a` ``` | ``` `1/a` ``` |
| `1 = #{a}` | ``` `1 = a` ``` | ``` `1=a` ``` |
| `-#{a}`* | ``` `-a` ``` | ``` `-a` ``` |
| `1-#{a}`* | ``` `1-a` ``` | ``` `1-a` ``` |

&ast; <sup>Because `-` is an identifier character, the `-` operator only
produces CSS-equivalent strings if it has no whitespace between it and the
interpolation.</sup>

#### Different stringifications

There are also expressions whose values have different **stringifications**
under the old and new semantics. The stringification of an expression is the
string representation of the result of its evaluation—for example, the
stringification of `1 + 2` is ``` `3` ```. These expressions are very likely to
change behavior; even if their values are immediately used in CSS, they'll have
different meanings. This includes any operators that don't insert their own
textual representation when operating on a string.

The following operators and their inverses should produce warnings immediately.
Note that _any expression_ containing free interpolation whose new ESI contains
these operators should have an immediate warning, even if they also include
other operators.

| SassScript | Old Stringification | New Stringification |
| --- | --- | --- |
| `not #{a}` | ``` `not a` ``` | ``` `false` ``` |
| `1 and #{a}` | ``` `1 and a` ``` | ``` `a` ``` |
| `1 or #{a}` | ``` `1 or a` ``` | ``` `1` ``` |
| `1 == #{a}` | ``` `1 == a` ``` | ``` `false` ``` |
| `1 != #{a}` | ``` `1 != a` ``` | ``` `true` ``` |
| `1 > #{a}` | ``` `1 > a` ``` | error |
| `1 >= #{a}` | ``` `1 >= a` ``` | error |
| `1 < #{a}` | ``` `1 < a` ``` | error |
| `1 <= #{a}` | ``` `1 <= a` ``` | error |
| `1 + #{a}` | ``` `1 + a` ``` | ``` `1a` ``` |
| `1 * #{a}` | ``` `1 * a` ``` | error |
| `1 % #{a}` | ``` `1 % a` ``` | error |
| `- #{a}`* | ``` `- a` ``` | ``` `-a` ``` |
| `1 - #{a}`* | ``` `1 - a` ``` | ``` `1-a` ``` |
| `1- #{a}`* | ``` `1 - a` ``` | ``` `1-a` ``` |

&ast; <sup>Because `-` is an identifier character, the `-` operator only
produces non-equivalent strings if it has whitespace between it and the
interpolation.</sup>

#### Adjacent expressions

Another case needs to be considered here: expressions that are adjacent to
interpolation without any whitespace intervening.

| SassScript | ESI | CSS |
| --- | --- | --- |
| `a#{b}` | ``` `a#{b}` ``` | `ab` |
| `$var#{b}` | ``` `#{$var}#{b}` ``` | `valueb` |
| `(1 + 2)#{b}` | ``` `#{1 + 2}#{b}` ``` | `3b` |
| `#{b}a` | ``` `#{b}a` ``` | `ba` |
| `#{b}$var` | ``` `#{b}#{$var}` ``` | `bvalue` |
| `#{b}(1 + 2)` | ``` `#{b}#{1 + 2}` ``` | `b3` |

Under the new rules, some of these would be parsed as interpolated identifiers
while others would be parsed as space-separated lists.

| SassScript | New ESI | New Stringification |
| --- | --- | --- |
| `a#{b}` | ``` `a#{b}` ``` | `ab` |
| `$var#{b}` | ``` ($var `#{b}`) ``` | `value b` |
| `(1 + 2)#{b}` | ``` ((1 + 2) `#{b}`) ``` | `3 b` |
| `#{b}a` | ``` `#{b}a` ``` | `ba` |
| `#{b}$var` | ``` (`#{b}` $var) ``` | `bvalue` |
| `#{b}(1 + 2)` | ``` (`#{b}` (1 + 2)) ``` | `b3` |

The second, third, fifth, and sixth examples should produce deprecation errors;
the first and fourth should not, as their stringifications remain the same.

#### Quoted strings

There is one case where the new behavior differs from the old. It comes up when
a dynamic value is included in an interpolated string without an explicit
`#{}`—that is, for every location that doesn't have a `#{}` in the SassScript
but does in the ESI. _If that value is a quoted string_, it will retain its
quotes, where if it were explicitly interpolated it would lose them. For
example:

| SassScript | ESI | Current CSS | CSS for ESI |
| --- | --- | --- | --- |
| `"foo" #{a}` | ``#{"foo"} #{a}`` | ``"foo" a`` | ``foo a`` |
| `$var: "foo"; $var + #{a}` | ``#{$var} + #{a}`` | ``"foo" + a`` | ``foo + a`` |

What this means is that the ESI is no longer actually equivalent in all cases,
because any of the newly interpolated values may or may not be a quoted string.
We can detect this in simple cases like the first example, but not in general.

Hopefully, not too many people are relying on cases we can't detect in practice.
I think we should still move forward with the deprecation and accept that our
heuristic isn't perfect, but I wanted to put this out there and get people's
opinions.

#### Same stringifications with different types

Finally, there are expressions that produce values with the same
stringifications but different types under the old and new semantics. In
practice the only operators that fall into this category are the list operators,
`,` and ` `. Note that we are once again examining the values of expressions
rather than their stringifications. For clarity, I'll include parentheses around
space-separated lists.

| SassScript | Old Value | New Value |
| --- | --- | --- |
| `#{a} #{b} #{c}` | ``` `a b c` ``` | `(`a` `b` `c`)` |
| `#{a}, #{b}, #{c}` | ``` `a, b, c` ``` | ``` `a`, `b`, `c` ``` |
| `1 2 #{a}` | ``` `1 2 a` ``` | ``` (1 2 `a`) ``` |
| `1, 2, #{a}` | ``` `1, 2, a` ``` | ``` 1, 2, `a` ``` |
| `1 -#{a}` | ``` `1 -a` ``` | ``` 1 `-a` ``` |

Most of the time, these values are benign. As long as they're included directly
in the stylesheet without any further manipulation, they'll have the same
behavior under the old and new semantics. But if they are manipulated in a way
that won't work for the new value, that's a problem and we need to issue a
warning.

Fortunately, the only way such a manipulation can occur is by passing the value
to a built-in function that will treat it differently as a string than it will
as a list. When passing an interpolation value produced via a list operator to
such a function, the implementation should emit a deprecation warning. Of the
canonical Sass functions, this includes:

- `unquote()`
- `quote()`
- `str-length()`
- The first or second argument of `str-insert()`
- `str-index()`
- The first argument of `str-slice()`
- `to-upper-case()`
- `to-lower-case()`
- `length()`
- The first argument of `nth()`
- The first argument of `set-nth()`
- `join()`
- The first or last argument of `append()`
- `zip()`
- The first argument of `index()`
- `list-separator()`
- `feature-exists()`
- `variable-exists()`
- `global-variable-exists()`
- `function-exists()`
- `mixin-exists()`
- `inspect()`
- `type-of()`
- The first argument of `call()`

It's up to each implementation to determine whether to emit warnings for which
user-defined functions.

Any expression that produces a deprecation warning can be converted to an
expression that will produce the same value and will work under the new
semantics by taking the ESI, making the quotes explicit, and wrapping it in
`unquote()`—for example, `1 + #{2} + 3` can be converted to
`unquote("1 + #{2} + 3")`. When emitting deprecation messages, we should do this
translation on the user's behalf.
