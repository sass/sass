# Plain CSS If: Draft 1.1

*([Issue](https://github.com/sass/sass/issues/3886), [Changelog](plain-css-if.changes.md)*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Integrating Sass](#integrating-sass)
    * [Substituted Boolean Expressions](#substituted-boolean-expressions)
* [Definitions](#definitions)
  * [Special Variable String](#special-variable-string)
  * [Special Number](#special-number)
* [Syntax](#syntax)
  * [`InterpolatedIdentifier`](#interpolatedidentifier)
  * [`ArbitrarySubstitution`](#arbitrarysubstitution)
  * [`IfExpression`](#ifexpression)
* [Procedures](#procedures)
  * [Evaluating an `IfCondition`](#evaluating-an-ifcondition)
  * [Evaluating an `IfGroup`](#evaluating-an-ifgroup)
  * [Parsing Text as CSS](#parsing-text-as-css)
* [Semantics](#semantics)
  * [`IfExpression`](#ifexpression-1)
* [Functions](#functions)
  * [`if()`](#if)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)

## Background

> This section is non-normative.

Back in 2010, Sass added an `if()` function as part of its effort to enable more
complex logic for Sass libraries. All Sass functions in this era were global,
and CSS didn't seem interested in adding many new functions outside the
occasional color expansion, so this was judged to be fairly safe.

In 2019, with the release of [the new module system], most built-in Sass
functions were added to `sass:` modules in order to remove manual namespaces and
avoid potential conflicts with CSS. However, `if()` uniquely among Sass-specific
functions remained in the global scope. This was for a few reasons:

1. `if()`'s semantics aren't strictly the same as other functions. It only ever
   evaluates its second *or* third argument based on which one is selected by
   the first; the other one is ignored, which is detectable if it would have
   produced an error when evaluated. This historically made it difficult to
   implement as a module-based function, although the infrastructure laid down
   for [calculation functions] makes this much more feasible today.

2. `if()` is part of the core language infrastructure (like a ternary expression
   in other languages) and so requiring a module to be loaded to use it would
   have been more onerous than we wanted to impose on users.

3. It didn't fit cleanly into any of the core modules. `sass:meta` was the
   closest as sort of a grab-bag of functions that did things with odd
   semantics, but `if()` isn't reflective in any capacity.

4. CSS still was not very active in adding new functions at the time, and we
   judged it particularly unlikely that a function named "if" would be added
   since there seemed to be very little appetite for that sort of conditional
   logic.

[the new module system]: ../accepted/module-system.md
[calculation functions]: ../accepted/calc-functions.md

This fourth point would prove to be a mistake. In 2024, the CSSWG [began
discussing] an inline conditional function using the name `if()`, and in 2025 it
shipped in Chrome. This function uses a fairly different syntax than Sass's
`if`, with colon-separated conditions and values and semicolon-separated
conditions, such as

```css
if(
  media(width < 700px): 0 auto;
  else: 20px auto;
)
```

[began discussing]: https://github.com/w3c/csswg-drafts/issues/10064#issue-2182499866

## Summary

> This section is non-normative.

This proposal adds support for parsing the plain CSS `if()` syntax and adding
support for Sass conditions directly to it, using the syntax `sass(<SassScript
expression>)`. For example,

```scss
if(
  map.has-key($tokens, $token),
  map.get($tokens, $token),
  $fallback
)
```

would become

```scss
if(
  sass(map.has-key($tokens, $token)): map.get($tokens, $token);
  else: $fallback;
)
```

This will integrate gracefully with plain-CSS conditionals, so that you can
write for example:

```scss
if(
  sass($force-wide) or media(width >= 500px): 3px;
  else: 1px;
)
```

Here, the Sass conditional will be resolved at compile time, so this will either
return `3px` (if `$force-wide` is true) or `if(media(width >= 500px): 3px; else:
1px;)` (if `$force-wide` is false). SassScript values will otherwise not be
allowed as part of conditions except using interpolation, but conditional values
(on the right of the `:`) will support full SassScript.

The old Sass `if()` syntax will be deprecated and eventually removed. However,
because the full CSS syntax can be supported without removing the old Sass
syntax, we intend to wait until Dart Sass 3.0.0 to complete the removal.

### Design Decisions

#### Integrating Sass

Choosing to integrate Sass semantics into the new `if()` syntax was not trivial.
A much simpler approach that we strongly considered was to treat the plain-CSS
`if()` as a [special function] and completely forego the need to parse its
highly bespoke syntax. If we took this path, existing users of Sass's `if()`
would have been moved towards a module system solution like `meta.if()`.

[special function]: ../spec/syntax.md#specialfunctionexpression

We ultimately decided to go with the more complex solution because it's much
nicer from a user's perspective. Authors don't need to track multiple different
incompatible syntaxes for conditionals in the same document, nor do they need to
use a cumbersome namespace to write Sass inline conditionals which are intended
to be part of the core language. (Note that loading `if()` without a namespace
would not necessarily have been feasible due to the name conflict with the CSS
function.)

In addition, we believe we can largely mitigate the cost of keeping the syntax
up-to-date moving forward. The conditional syntax has a clear extension point in
the [`<if-test>`] production, and we can easily add general support for all
functions that may appear there in the future. Other extensions are likely to be
shared across all plain CSS functions, such as [argument grouping] and [spread
arguments].

[`<if-test>`]: https://drafts.csswg.org/css-values-5/#typedef-if-test
[argument grouping]: https://github.com/sass/sass/issues/3799
[spread arguments]: https://drafts.csswg.org/css-values-5/#early-resolution

#### Substituted Boolean Expressions

Both plain CSS and Sass have ways of injecting content into expression-level
syntactic structures that's resolved *before* the syntax is expected to be
parsed. For CSS, this is `var()` and other [arbitrary substitution functions]
(including `if()` itself). For Sass, it's interpolation. Either way, you can end
up with odd situations like this:

[arbitrary substitution functions]: https://drafts.csswg.org/css-values-5/#arbitrary-substitution-function

```css
/* plain CSS */
:root {
  --var: and;
  --value: if(
    media(width >= 500px) var(--var) style(--cards: small): 100px;
    else: 200px;
  );
}
```

```scss
// Sass
:root {
  $var: "and";
  --value: if(
    media(width >= 500px) #{$var} style(--cards: small): 100px;
    else: 200px;
  );
}
```

We're presented with the need to handle both of these cases gracefully while
also parsing the boolean expressions thoroughly enough to resolve the `sass()`
conditions in them. To make this work, we add an "escape hatch" to the parsing
logic: arbitrary substitution functions and interpolations are allowed in
sequence with plain CSS [`<if-test>`] productions, but *not* `sass()`
conditions.

There are a couple other ways we could have handled this:

* We could have made the entire conditional fall back to a simpler parse upon
  detecting a substitution (or even, if we chose, any kind of unexpected
  syntax). Done naïvely, this could make it easy for users to add invalid
  syntax, but we could define the parse to avoid that. The bigger issue is that
  it couldn't support cases like `sass($flag) or (media(width >= 500px)
  var(--var) style(--cards: small))` where the Sass condition *is* possible to
  unambiguously resolve.

* We could delay parsing the condition itself until evaluation time to handle
  interpolation specifically, as we do for `@media` queries and selectors. This
  would require some nuance to parse and evaluate the `sass()` expressions
  ahead-of-time, but it's not insurmountable. However, the existing uses of this
  late-parse behavior have proven to be a breeding ground for odd bugs and
  friction in the user experience; they're difficult to statically analyze,
  difficult to produce accurate source spans for, and difficult to implement. We
  would like to avoid adding any new such cases if at all possible.

We avoid parsing these functions as full SassScript, even in cases like nested
`if()` expressions, to match the general principle that SassScript is not
supported in `if()` conditions except within interpolation and the `sass()`
function.

## Definitions

### Special Variable String

Replace the definition of a [special variable string] with:

[special variable string]: ../spec/functions.md#special-variable-string

A *special variable string* is an unquoted string that CSS will recognize as an
[arbitrary substitution function]. For the purposes of Sass, this is any
unquoted string that begins with `var(`, `attr(`, or `if(`. This matching is
case-insensitive.

[arbitrary substitution function]: https://drafts.csswg.org/css-values-5/#arbitrary-substitution-function

> **Note:** The upgrade of `attr(` to a special variable function is technically
> outside the primary purpose of this proposal, but it's closely related in that
> `if()` represents the introduction of the category of "arbitrary substitution
> functions" to Sass. Previously, we assumed that `attr()` could only produce
> individual values, but that's not accurate (as in for example `rgb(attr(foo,
> 255, 255, 255))`).

### Special Number

Replace the definition of a [special number] with:

[special number]: ../spec/functions.md#special-number

A *special number* is any of:

* a [calculation],
* a [special variable string],
* an unquoted string that begins with `calc(`, `env(`, `clamp(`, `min(`, or
  `max(`. This matching is case-insensitive.

  > This final category is a historical artifact from when Sass compiled
  > calculations to strings rather than first-class calculation objects. It
  > should not be expanded with new entries, and may be deprecated in the
  > future.

[calculation]: ../spec/types/calculation.md

## Syntax

### `InterpolatedIdentifier`

Add the following production alongside [`InterpolatedIdentifier`]:

[`InterpolatedIdentifier`]: ../spec/syntax.md#interpolatedidentifier

<x><pre>
**InterpolatedCustomIdentifier** ::= '--' ([Name] | Interpolation)+
</pre></x>

[Name]: ../spec/syntax.md#name

No whitespace is allowed between components of an
`InterpolatedCustomIdentifier`.

### `ArbitrarySubstitution`

Add the following new grammar:

<x><pre>
**ArbitrarySubstitution**         ::= Interpolation | ArbitrarySubstitutionFunction
**ArbitrarySubstitutionFunction** ::= ('if(' | 'var(' | 'attr(')¹ [InterpolatedAnyValue] ')'
&#32;                               | InterpolatedCustomIdentifier² '(' [InterpolatedAnyValue] ')'
</pre></x>

1: This is matched case-insensitively.

2: No whitespace is allowed between this and the following `(`.

[InterpolatedAnyValue]: ../spec/syntax.md#interpolatedanyvalue

### `IfExpression`

Replace the grammar for `IfExpression` with the following:

<x><pre>
**IfExpression**          ::= 'if('¹ (IfBranch ';')* IfBranch ';'? ')'
**IfBranch**              ::= IfCondition ':' Expression
**IfCondition**           ::= IfConditionExpression | 'else'¹
**IfConditionExpression** ::= 'not'¹? IfGroup
&#32;                       | IfGroup ('and'¹ IfGroup)+
&#32;                       | IfGroup ('or'¹ IfGroup)+
&#32;                       | IfConditionArbitrary²
**IfConditionArbitrary**  ::= ArbitrarySubstitution IfGroup³
&#32;                       | ArbitrarySubstitution? IfGroup³ (
&#32;                             'and'¹ IfGroup³
&#32;                           | 'and'¹? ArbitrarySubstitution IfGroup³?
&#32;                         )+
&#32;                       | ArbitrarySubstitution? IfGroup³ (
&#32;                             'or'¹ IfGroup³
&#32;                           | 'or'¹? ArbitrarySubstitution IfGroup³?
&#32;                         )+
**IfGroup**               ::= Interpolation
&#32;                       | [InterpolatedIdentifier]⁴ '(' [InterpolatedAnyValue] ')'
&#32;                       | '(' IfConditionExpression ')'
&#32;                       | SassCondition
**SassCondition**         ::= 'sass(' Expression ')'
</pre></x>

[InterpolatedIdentifier]: ../spec/syntax.md#interpolatedidentifier

1: This is matched case-insensitively.

2: Parse other `IfConditionExpression` branches preferentially to
`IfConditionArbitrary`.

3: These `IfGroup`s may not contain `SassCondition`s except within
interpolation.

4: No whitespace is allowed between this and the following `(`. This may not be
case-sensitively equal to `sass`, nor may it be case-insensitively equal to
`not`, `and`, or `or`.

Whitespace *must* appear between `and`, `or`, `not`, and `Interpolation` and a
following `(`.

> We could consider `if(not(): ...)` to be an unknown CSS function call, but
> it's extremely likely to be a typo and (for the same reason) almost certainly
> won't be defined to be meaningful by CSS in the future. Throwing an error
> helps users catch that error early.

> Because every `ArbitrarySubstitution` is also a valid `IfGroup`, there are
> multiple possible `IfConditionArbitrary` parse trees of certain token streams.
> But since the condition is evaluated purely as interpolated text, which parse
> tree is constructed in particular is not relevant.

> Per the CSS spec, arbitrary substitution functions are *only* allowed within
> `IfCondition`s and `IfBranch`es. Because the colon and semicolon separators
> are part of `if()`'s [argument grammar], they must appear literally in the
> stylesheet. The upshot of this is that we don't have to worry about handling
> those edge cases when parsing.

[argument grammar]: https://drafts.csswg.org/css-values-5/#argument-grammar

## Procedures

### Evaluating an `IfCondition`

This procedure takes an `IfCondition` `condition` and returns either a boolean
representing a result that's known at build time or a string representing a
plain-CSS condition.

* If `condition` is case-insensitively equal to `else`, return true.

* If `condition` contains a single `IfGroup`:

  * Let `result` be the result of [evaluating the `IfGroup`].

  * If `condition` doesn't start (case-insensitively) with `'not'`, return `result`.

  * Otherwise, if `result` is a boolean, return `not result`.

  * Otherwise return the result of concatenating `'not '` and `result`.

  [evaluating the `IfGroup`]: #evaluating-an-ifgroup
  
* If `condition` contains multiple `IfGroup`s separated by (case-insensitive)
  `and`s:

  * Let `results` be an empty list.

  * For each `IfGroup` `group`:

    * Let `result` be the result of [evaluating the `group`].

    * If `result` is false, return false.

    * Otherwise, if `result` is a string, add it to `results`.

  * If `results` is empty, return true.

  * Otherwise, return `results` concatenated with `' and '` between each element.

  [evaluating the `group`]: #evaluating-an-ifgroup

* If `condition` contains multiple `IfGroup`s separated by (case-insensitive)
  `or`s:

  * Let `results` be an empty list.

  * For each `IfGroup` `group`:

    * Let `result` be the result of [evaluating the `group`].

    * If `result` is true, return true.

    * Otherwise, if `result` is a string, add it to `results`.

  * If `results` is empty, return false.

  * Otherwise, return `results` concatenated with `' or '` between each element.

* Otherwise, when `condition` is an `IfConditionArbitrary`, return the string
  result of evaluating any interpolations in `condition`.

  > All other tokens are included in the result as plain text.

### Evaluating an `IfGroup`

This procedure takes an `IfGroup` `group` and returns either a boolean
representing a result that's known at build time or a string representing a
plain-CSS condition.

* If `group` is a `SassCondition`, return a boolean indicating whether or not
  the result of evaluating its expression is truthy.

* Otherwise, if `group` contains a single `IfExpression`, return the result of
  [evaluating that expression as an `IfCondition`].

  [evaluating that expression as an `IfCondition`]: #evaluating-an-ifcondition

* Otherwise, return the result of evaluating any interpolation in `group`.

### Parsing Text as CSS

In [parsing text as CSS], add "The [`SassCondition`] production" to the list of
productions that should produce errors.

[parsing text as CSS]: ../spec/syntax.md#parsing-text-as-css
[`SassCondition`]: #ifexpression

## Semantics

### `IfExpression`

* Let `results` be an empty list.

* For each `IfBranch` `branch`:

  * Let `condition` be the result of [evaluating `branch`'s condition].

  * If `condition` is true:

    * Let `value` be the result of evaluating `branch`'s expression.

    * If `results` is empty, return `value`.

    * Otherwise, add `["else", value]` to `results` and stop processing
      additional `IfBranch`es.

  * Otherwise, if `condition` is false, ignore this branch.

  * Otherwise, let `value` be the result of evaluating `branch`'s expression and
    add `[condition, value]` to `results`.

  [evaluating `branch`'s condition]: #evaluating-an-ifcondition

* If `results` is empty, return null.

* Otherwise, return a string representation of a [CSS `if()` function] with the
  given conditions (treated as unquoted strings) and values.

  [CSS `if()` function]: https://drafts.csswg.org/css-values-5/#if-notation

  > As usual, implementations are free to choose a string representation as long
  > as it matches the CSS semantics of the function.

## Functions

### `if()`

Remove the [top-level `if()` function].

[top-level `if()` function]: ../spec/functions.md#if

## Deprecation Process

Because this proposal removes the old syntax for `if()`, it will be rolled out
in two phases to give users plenty of time to upgrade.

### Phase 1

In this phase, both the old and the new syntax for `if()` will coexist.
Specifically, an `IfExpression` will be parsed as a classic `IfExpression` if
possible, and will fall back to being parsed using the new syntax. This is
guaranteed to be safe because the new `if()` will always either contain a
top-level `;` which is never legal in the old syntax *or* a single top-level `:`
that's not preceded by any top-level commas. Since the old syntax takes three
comma-separated arguments, if it contains a top-level `:` it will either have a
comma before it or a `...` after it, neither of which is valid in the new
syntax.

In Phase 1, we will also retain the `if()` function as defined at the top level
(which is distinct from the expression we handle at parse-time). However,
evaluating either the top-level function or the expression will produce a
deprecation error named `if-function`.

We will provide a migration tool to automatically move users to the new `if()`
syntax.

### Phase 2

In this phase, we'll remove the old syntax and function entirely and Sass will
handle the new `if()` syntax as specified.
