# Forward Slash as a Separator: Draft 3

*([Issue](https://github.com/sass/sass/issues/2565), [Changelog](slash-separator.changes.md))*

This proposal modifies the `/` character to be used exclusively as a separator,
and lays out a process for deprecating its existing usage as a division
operator.

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Alternatives Considered](#alternatives-considered)
  * [Different Operator Syntax](#different-operator-syntax)
  * [First-Class `calc()`](#first-class-calc)
  * [`math()` Syntax](#math-syntax)
* [Existing Behavior](#existing-behavior)
* [Syntax](#syntax)
* [Semantics](#semantics)
  * [Slash-Separated Lists](#slash-separated-lists)
  * [`math.div()` Function](#mathdiv-function)
  * [`list.slash()` Function](#listslash-function)
  * [`rgb()` Function](#rgb-function)
  * [`hsl()` Function](#hsl-function)
  * [Selector Functions](#selector-functions)
  * [Slash-Free Numbers](#slash-free-numbers)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)
  * [Phase 3](#phase-3)
* [Timeline](#timeline)

## Background

> This section is non-normative.

Early on in Sass's history, the decision was made to use `/` as a division
operator, since that was (and is) by far the most common representation across
programming languages. The `/` character was used in very few plain CSS
properties, and for those it was an optional shorthand. So Sass defined [a set
of heuristics][] that defined when `/` would be rendered as a literal slash
versus treated as an operator.

[a set of heuristics]: https://sass-lang.com/documentation/operators/numeric#slash-separated-values

For a long time, these heuristics worked pretty well. In recent years, however,
new additions to CSS such as [CSS Grid][] and [CSS Color Level 4][] have been
using `/` as a separator increasingly often. Using the same character for both
division and slash-separation is becoming more and more annoying to users, and
will likely eventually become untenable.

[CSS Grid]: https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row
[CSS Color Level 4]: https://drafts.csswg.org/css-color/#rgb-functions

## Summary

> This section is non-normative.

We will redefine `/` to be *only* a separator. Rather than creating an unquoted
string (as it currently does when at least one operand isn't a number), it will
create a slash-separated list. As such, lists will now have three possible
separators: space, comma, and slash.

Division will instead be written as a function, `math.div()`. Eventually, it
will also be possible to write Sass-compatible division in `calc()` expressions;
however, this is not going to be implemented immediately and is outside the
scope of this proposal.

[the new module system]: module-system.md

This is a major breaking change to existing Sass semantics, so we'll roll it out
in a three-stage process:

1. The first stage won't introduce any breaking changes. It will:

   * Add a `math.div()` function which will work exactly like the `/` operator
     does today, except that it will produce deprecation warnings for any
     non-number arguments.

   * Add slash-separated lists to Sass's object models, *without* a literal
     syntax for creating them. That will come later, since it would otherwise be
     a breaking change.

   * Add a `list.slash()` function that will create slash-separated lists.

   * Produce deprecation warnings for all `/` operations that are interpreted as
     division.

2. The second stage *will* be a breaking change. It will:

   * Make `/` exclusively a list separator.

   * Make `math.div()` throw errors for non-number arguments.

   * Deprecate the `list.slash()` function, since it will now be redundant.

3. The third stage will just remove the `list.slash()` function. This is not a
   priority, and will be delayed until the next major version release.

## Alternatives Considered

> This section is non-normative.

### Different Operator Syntax

One other possible fix would be to change the syntax for division to another
punctuation-based operator. We ended up deciding that anything we chose would be
so different from every other programming language as to be unreadable for
anyone unfamiliar with the language.

In addition, the best candidate operator we found was `~`, as an ASCII character
that wasn't already in use in CSS or Sass value syntax. But `~` is difficult to
type on many non-English keyboard layouts, which makes it only marginally more
efficient to write than a function call for many Sass users.

### First-Class `calc()`

We eventually want to add native Sass support for parsing `calc()` expressions,
resolving them at compile-time if possible, and producing a new Sass value that
can have arithmetic performed on it if necessary. This is known as [first-class
`calc()`][], and it would mean that division could be written unambiguously
using `/` in the context of a `calc()` expression. For example, `$width / 2`
would be instead written `calc($width / 2)`.

[first-class `calc()`]: https://github.com/sass/sass/issues/2186

However, first-class `calc()` is likely to be a very complex feature to design
and implement. Most of the resources available for large-scale language features
are currently focused on [the new module system][], so it's likely that a full
implementation of first-class `calc()` won't land until mid-to-late 2020. And
the full implementation is a prerequisite for even *beginning* the deprecation
cycle for `/`-as-division, which means we probably wouldn't fully support
`/`-as-separator for another three to six months after that point. This is just
too much time to wait on giving users a good solution for writing `/`-separated
properties.

### `math()` Syntax

A possible middle ground between `calc()` and the current syntax would be using
a special `math()` expression as a way of signaling a syntactic context where
`/` is interpreted as division without needing to fully support all the edge
cases of `calc()`. For example, `$width / 2` would be instead written
`math($width / 2)`.

Unfortunately, this is highly likely to confuse users. They may think that
`math()` is necessary for all mathematical operations, when in fact it's only
necessary for division, which would lead to confusing and unnecessary `math()`
expressions popping up all over the place. They may also think it's a Sass
library function or a plain CSS feature, neither of which is true, and look in
the wrong place for documentation.

Worse, once we *did* add support for first-class `calc()`, there would then be
two different ways of wrapping mathematical expressions which had slightly but
meaningfully different semantics. This is a recipe for making users feel
confused and overwhelmed.

## Existing Behavior

To precisely describe when a deprecation warning should be emitted, we must
first describe the existing heuristic behavior.

A Sass number may be *potentially slash-separated*. If it is, it is associated
with two additional Sass numbers, the *original numerator* and the *original
denominator*. A number that is not potentially slash-separated is known as
*slash-free*.

A potentially slash-separated number is created when a `ProductExpression` with
a `/` operator is evaluated and both operands are *syntactically* either literal
`Number`s or `ProductExpression`s that can themselves create potentially
slash-separated numbers. In this case, both operands are guaranteed to be
evaluated as numbers. The first operand is the original numerator of the
potentially slash-separated number returned by the `/` operator, and the second
is the original denominator.

A potentially slash-separated number is converted to a slash-free number when:

* It is the value of a `ParenthesizedExpression`.

  > That is, it's in parentheses, such as in `(1 / 2)`. Note that if it's in a
  > list that's in parentheses, it's *not* converted to a slash-free number.

* It is stored in a Sass variable.

* It is passed into a user-defined function or mixin.

* It is returned by a function.

> Any expressions that normally produce a new number (such as other mathematical
> operations) always produce slash-free numbers, even when their arguments are
> slash-separated.
>
> When a potentially slash-separated number is "converted" to a slash-free
> number, a slash-free copy is made of the original. Sass values are always
> immutable.

When a potentially slash-separated number is converted to CSS, either when
converted to a string via interpolation or when included in a declaration's
value, it is written as the original numerator followed by `/` followed by the
original denominator. If either the original numerator or denominator are
themselves slash-separated, they're also written this way.

## Syntax

> Note that the existing productions being modified have not been defined
> explicitly before this document. The old definitions are listed in
> strikethrough mode to clarify the change.

This proposal modifies the existing `CommaListExpression` production to add
support for slash-separated lists. The new grammar for this production is:

<x><pre>
~~**CommaListExpression** ::= SpaceListExpression (',' SpaceListExpression)*~~
**CommaListExpression** ::= SlashListExpression (',' SlashListExpression)*
**SlashListExpression** ::= SpaceListExpression ('/' SpaceListExpression)*
</pre></x>

> Note that `/` may *not* be used in single-element lists the way `,` is. That
> is, `(foo,)` is valid, but `(foo/)` is not.
>
> This defines `/` to bind tighter than `,` but looser than space-separated
> lists. This was chosen because most common uses of `/` in CSS conceptually
> bind looser than space-separated values. The only exception is the [`font`
> shorthand syntax][], which is used much more rarely will still work (albeit
> with an unintuitive SassScript representation) with a loose-binding `/`.
>
> [`font` shorthand syntax]: https://developer.mozilla.org/en-US/docs/Web/CSS/font

It also modifies the existing `ProductExpression` production to remove `/` as an
operator. The new grammar for this production is:

<x><pre>
~~**ProductExpression** ::= (ProductExpression ('*' | '/' | '%'))? UnaryPlusExpression~~
**ProductExpression** ::= (ProductExpression ('*' | '%'))? UnaryPlusExpression
</pre></x>

When a `SlashListExpression` with one or more `/`s is evaluated, it produces a
list object whose contents are the values of its constituent
`SpaceListExpression`s and whose separator is "slash".

## Semantics

### Slash-Separated Lists

A new list separator, known as "slash", will be added. The string `"slash"` may
be passed to the `$separator` argument of `append()` and `join()`, and may be
returned by `list.separator()`. When converted to CSS, slash-separated lists
must have exactly one `/` between each adjacent pair of elements.

> Although CSS doesn't currently make use of this syntax, there's nothing
> stopping a list from being both bracketed and slash-separated.

### `math.div()` Function

The `div()` function in the `sass:math` module has the following signature:

```
math.div($number1, $number2)
```

It throws an error if either argument is not a number. If both are numbers, it
returns the same result that the `/` operator did prior to this proposal.

### `list.slash()` Function

The `slash()` function in the `sass:list` module has the following signature:

```
list.slash($elements...)
```

It throws an error if zero or one arguments are passed. It returns an
unbracketed slash-separated list containing the given elements.

### `rgb()` Function

This proposal modifies [the existing behavior][old rgb] of the `rgb($channels)`
overload to be the following:

[old rgb]: ../spec/functions.md#rgb-and-rgba

* If `$channels` is a [special variable string][], return a plain CSS function
  string with the name `"rgb"` and the argument `$channels`.

  [special variable string]: ../spec/functions.md#special-variable-string

* If `$channels` is an unbracketed slash-separated list:

  * If `$channels` doesn't have exactly two elements, throw an error. Otherwise,
    let `rgb` be the first element and `alpha` the second element.

  * If either `rgb` or `alpha` is a special variable string, return a plain CSS
    function string with the name `"rgb"` and the argument `$channels`.

  * If `rgb` is not an unbracketed space-separated list, throw an error.

  * If `rgb` has more than three elements, throw an error.

  * If `rgb` has fewer than three elements:

    * If any element of `rgb` is a [special variable string][], return a
      plain CSS function string with the name `"rgb"` and the argument
      `$channels`.

    * Otherwise, throw an error.

  * Let `red`, `green`, and `blue` be the three elements of `rgb`.

  * Call `rgb()` with `red`, `green`, `blue`, and `alpha` as arguments and
    return the result.

* Otherwise, proceed with the existing definition of the function.

> This ensures that calling (for example) `rgb(0 0 100% / 80%)` will continue to
> work when `/` is parsed as a slash separator. It's important to define this
> runtime behavior in phase 1 so that users manually constructing
> slash-separated lists can use them as expected.

### `hsl()` Function

This proposal modifies [the existing behavior][old hsl] of the `hsl($channels)`
overload to be the following:

[old hsl]: ../spec/functions.md#hsl-and-hsla

* If `$channels` is a [special variable string][], return a plain CSS function
  string with the name `"hsl"` and the argument `$channels`.

* If `$channels` is an unbracketed slash-separated list:

  * If `$channels` doesn't have exactly two elements, throw an error. Otherwise,
    let `hsl` be the first element and `alpha` the second element.

  * If either `hsl` or `alpha` is a special variable string, return a plain CSS
    function string with the name `"hsl"` and the argument `$channels`.

  * If `hsl` is not an unbracketed space-separated list, throw an error.

  * If `hsl` has more than three elements, throw an error.

  * If `hsl` has fewer than three elements:

    * If any element of `hsl` is a [special variable string][], return a
      plain CSS function string with the name `"hsl"` and the argument
      `$channels`.

    * Otherwise, throw an error.

  * Let `hue`, `saturation`, and `lightness` be the three elements of `hsl`.

  * Call `hsl()` with `hue`, `saturation`, `lightness`, and `alpha` as arguments
    and return the result.

* Otherwise, proceed with the existing definition of the function.

### Selector Functions

This proposal modifies [the "Parse a Selector From a SassScript Object"
procedure][] to throw an error whenever it encounters a slash-separated list.

[the "Parse a Selector From a SassScript Object" procedure]: ../spec/built-in-modules/selector.md#parse-a-selector-from-a-sassscript-object

### Slash-Free Numbers

This proposal adds one additional scenario in which [potentially slash-separated
numbers] are converted into [slash-free numbers]:

[potentially slash-separated numbers]: #existing-behavior
[slash-free numbers]: #existing-behavior

* When a number is passed to a built-in function or mixin.

> This change makes built-in functions/mixins consistent with user-defined ones,
> which *do* make their arguments slash-free. It also combines with [Phase
> 1](#phase-1) of the deprecation process to ensure that all uses of
> `/`-as-division will produce warnings.
>
> This could potentially be a breaking change. While most functions that could
> take potentially slash-separated numbers will either ignore the
> slash-separation or return the number and cause it to become slash-free that
> way, it's possible for a user to pass it to a function that puts it in a data
> structure, as in `list.join(1/2, ())` which returns a single-element list
> containing a potentially slash-separated number. However, this breakage is
> considered exceedingly unlikely and it's easy to work around using
> `list.slash()` so we aren't considering it a blocker.

## Deprecation Process

The deprecation process will be divided into three phases:

### Phase 1

This phase will add no breaking changes, and will be implemented as soon as
possible. Its purpose is to notify users that `/`-as-division will eventually be
removed and give them alternatives to migrate to that will continue to work when
`/`'s behavior is changed.

Phase 1 implements none of [the syntactic changes](#syntax) described above. It
implements all [the semantics](#semantics), with the exception that `math.div()`
allows non-number arguments. If either argument is not a number, it emits a
deprecation warning.

> If either argument is not a number, `math.div()` still returns the same result
> as the `/` operator, which in that case will be concatenating the two
> arguments into an unquoted string separated by `/`. This behavior is supported
> for the time being to make it easier to automatically migrate users to
> `math.div()` without causing runtime errors.

While phase 1 will continue to support `/` as a division operator, the use of
the operator in this way will produce a deprecation warning. Specifically, a
deprecation warning is emitted when a [potentially slash-separated
number](#existing-behavior) is converted to a slash-free number, *or* when a `/`
operation returns a [slash-free](#existing-behavior) number.

> In phase 1, we recommend authors write division and slash-separated lists like
> so:
>
> ```scss
> @use 'sass:list;
> @use 'sass:math';
>
> $ratio: math.div(12rem, 1px);
> $row: list.slash(span 3, 6);
>
> .grid .item1 {
>   // It's always safe to use `/` as a separator directly in a CSS property.
>   grid-row: span 2 / 7;
> }
> ```

### Phase 2

This phase will introduce breaking changes to the language. It implements both
[the syntactic changes](#syntax) and [the semantic changes](#semantics) exactly
described above (so `math.div()` will only accept numbers). In phase 2, the
`list.slash()` function will emit a deprecation warning whenever it's called.

> It's recommended that implementations increment their major version numbers
> with the release of phase 2, in accordance with [semantic versioning][].
>
> [semantic versioning]: https://semver.org/

> In phase 2 and 3, we recommend authors write division and slash-separated
> lists like so:
>
> ```scss
> @use 'sass:math';
>
> $ratio: math.div(12rem, 1px);
>
> // As of phase 2, `/` is always parsed as a slash-separated list.
> $row: span 3 / 6;
>
> .grid .item1 {
>   grid-row: span 2 / 7;
> }
> ```

### Phase 3

This phase will introduce a final breaking change, removing the now-unnecessary
`list.slash()` function.

> It's recommended that implementations increment their major version numbers
> again with the release of phase 3.

## Timeline

* Phase 1 was originally scheduled to be implemented by Dart Sass as soon as the
  proposal was accepted. However, it was delayed considerably in the hope that
  it would also be implemented by LibSass. Since [LibSass is now deprecated],
  we plan to release Phase 1 in Q2 2021.

  [LibSass is now deprecated]: https://sass-lang.com/blog/libsass-is-deprecated

* Phase 2 will be released in Dart Sass 2.0.0. There's no solid release date for
  this yet, and it may or may not be concurrent with the removal of support for
  `@import` depending on how quickly the module system is adopted and how urgent
  the need for a syntactic slash separator becomes.

* Phase 3 will be released in Dart Sass 3.0.0, whenever that ends up happening.
  Removing `list.slash()` is not considered a priority, so this will wait until
  we have additional, more-compelling breaking changes we want to release.
