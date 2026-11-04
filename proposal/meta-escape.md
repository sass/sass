# Meta Escape: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4268))*

## Table of Contents

* [Background](#background)
  * [Textual and Semantic Value](#textual-and-semantic-value)
  * [Historical Behavior](#historical-behavior)
    * [Pre-1.106.0](#pre-11060)
    * [1.106.0](#11060)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Throwing Errors](#throwing-errors)
* [Definitions](#definitions)
  * [Logical String](#logical-string)
* [Types](#types)
  * [String](#string)
* [Procedures](#procedures)
  * [Getting the Semantic Value of a String](#getting-the-semantic-value-of-a-string)
  * [Converting a String to an Identifier](#converting-a-string-to-an-identifier)
* [Meta-Programming Functions](#meta-programming-functions)
  * [`calc-name()`](#calc-name)
  * [`function-exists()`](#function-exists)
  * [`get-function()`](#get-function)
  * [`get-mixin()`](#get-mixin)
  * [`get-module()`](#get-module)
  * [`global-variable-exists()`](#global-variable-exists)
  * [`keywords()`](#keywords)
  * [`load()`](#load)
  * [`mixin-exists()`](#mixin-exists)
  * [`module-functions()`](#module-functions)
  * [`module-mixins()`](#module-mixins)
  * [`module-variables()`](#module-variables)
  * [`variable-exists()`](#variable-exists)
* [Arguments](#arguments)
* [Deprecation Process](#deprecation-process)
  * [Phase 1](#phase-1)
  * [Phase 2](#phase-2)
  * [Phase 3](#phase-3)

## Background

> This section is non-normative.

Sass has historically been a little loose with the way escape codes have been
handled in `sass:meta` APIs (as well as keyword rest arguments, which don't
necessarily involve `sass:meta` but are similar in that they expose Sass
constructs to SassScript). Many of these APIs deal with Sass structures that use
CSS-style identifiers, including variable, mixin, and function names, as well as
module namespaces.

We've been able to get away with this looseness because these almost universally
don't include characters that need to be escaped in identifiers. After all, why
would they include these? The point of these names is to be easily referenced,
and escape codes make them harder to read for no real benefit.

All the same, we want to ensure every corner of Sass's behavior is consistent
and sensible. In addition, even if users are rarely expected to *intentionally*
hit this behavior, there's a real possibility of them accidentally passing
non-identifier strings to various APIs, and we want to make sure we handle those
gracefully.

### Textual and Semantic Value

In Sass, we can talk about strings having two different values which may or may
not be the same in practice. The **textual value** is the exact sequence of
Unicode code points the string contains, whereas the **semantic value** is the
sequence of Unicode code points produced when the string is evaluated as CSS.

For quoted strings, these are identical. For unquoted strings, though, they may
differ if the string contains escape codes. For example, the unquoted string
`\$`'s textual value is `[U+005C BACKSLASH, U+0024 DOLLAR SIGN]` but its
semantic value is just `[U+0024 DOLLAR SIGN]`. Unquoted strings with different
textual values, such as `\$` and `\24`, may have the same semantic value.

It's also important to note that not all unquoted strings even *have* a
meaningful semantic value. Although Sass only supports unquoted string literals
that are valid CSS identifiers, interpolation and `string.unquote()` can create
unquoted strings that contain invalid identifier characters and thus have no
semantic value at all. For example, `string.unquote("$")` returns a value that
is not a valid CSS identifier. We call these **anti-identifiers**.

### Historical Behavior

#### Pre-1.106.0

Prior to Dart Sass 1.106.0, internal names of variables, mixins, functions, and
module namespaces were stored as the identifiers' textual values with escapes
normalized at parse time. This means that if the user wrote `\61` it would be
stored as `"a"`, but if they wrote `\24` it would be stored as `"\\24"`. The
`sass:meta` APIs that accessed or created these structures (again including
keyword rest arguments) always used a string's textual value rather than its
semantic value. For example, if a variable was defined as `$\$: null`:

* `meta.variable-exists(\$)` would return `true`.

* `meta.variable-exists(\24)` would return `true`, because the SassScript
  unquoted string literal `\24` is normalized to `\$` at parse-time.

* `meta.variable-exists("$")` would return `false`, because its textual value is
  `[U+0024 DOLLAR SIGN]` rather than `[U+005C BACKSLASH, U+0024 DOLLAR SIGN]`.

* `meta.variable-exists(string.unquote("$"))` would return `false` for the same
  reason. In general, no APIs distinguished between quoted an unquoted strings
  with the same textual value, even though the semantic value could be
  substantially different.

* `meta.variable-exists("\\$")` would return `true`.

* `meta.variable-exists("\\24")` and
  `meta.variable-exists(string.unquote("\\24"))` would return `false` because
  this is *not* the normalized form of an escaped space.

#### 1.106.0

Dart Sass 1.106.0 introduced a substantial internal refactor, now storing
identifier names as their semantic values rather than their normalized textual
values. This helped fix a number of parsing errors and eliminated the
possibility of undefined behavior if an anti-identifier ended up in an AST node.
However, the implementation of the `sass:meta` functions remained unchanged,
which unintentionally meant that their behavior with escaped identifiers was now
different. Again with the variable `$\$: null`:

* `meta.variable-exists(\$)` and `meta.variable-exists(\24)` now both return
  `false`, because the variable name is stored as its semantic value `[U+0024
  DOLLAR SIGN]` and they're checking for the textual value `[U+005C BACKSLASH,
  U+0024 DOLLAR SIGN]`.

* For the inverse reason, `meta.variable-exists("$")` and
  `meta.variable-exists(string.unquote("$"))` now return `true`.

Not only is this a breaking change, it's an overall regression in terms of how
intuitive the behavior is. The only potentially correct change is
`meta.variable-exists("$")` returning `true`.

## Summary

> This section is non-normative.

This proposal puts forth a new, principled set of behaviors for `sass:meta` APIs
that deal with the intersection of strings and identifiers. It is a breaking
change relative to both the pre-1.106.0 behavior and the 1.106.0 behavior, but
it's much more sensible than either.

In general:

* APIs that expose identifiers to Sass code, such as `meta.keywords()`, will
  consistently return them as unquoted strings in normalized escaped form. This
  matches the pre-1.106.0 behavior for `meta.keywords()`, but differs for other
  functions like `meta.module-variables()` in that *unquoted* strings are now
  returned.

* APIs that take Sass strings that are intended to be identifiers will operate
  on the *semantic value* of those strings rather than their textual value. They
  will throw an error if passed an anti-identifier.

### Design Decisions

#### Throwing Errors

In some cases, there are reasonably well-defined behaviors that could be defined
for anti-identifiers other than throwing errors. For example, the
`meta.*-exists()` functions could simply return `false`. However, there's also
no reason to pass an anti-identifier to these functions, and silently returning
`false` risks failing to notify the user if they're handling identifiers in a
way they don't expect. This risk is escalated given that escapes are already an
edge case many wouldn't think to test (even Sass's own tests weren't
sufficient!), and that the behavior is changing over time. As such, throwing an
error is the safer option.

## Definitions

### Logical String

A *logical string* is a finite sequence of Unicode code points. This terminology
is used specifically when it would otherwise be ambiguous with a [SassScript
string]. It's referred to as just a *string* in cases where there's no
ambiguity.

[SassScript string]: #string

## Types

### String

A *SassScript string* (referred to as just a *string* when doing so is
unambiguous with the more general concept) contains a [logical string] (known as
its *contents* or its *textual value* when necessary to disambiguate with its
[semantic value]). A string may or may not be *quoted*.

[logical string]: #logical-string
[semantic value]: #getting-the-semantic-value-of-a-string

## Procedures

### Getting the Semantic Value of a String

This procedure takes a SassScript string `string` and returns a [logical
string].

* If `string` is quoted, return its textual value.

* If `string`'s textual value is an [`<ident-token>`], return that token's
  value.

* Otherwise, throw an error.

[`<ident-token>`]: https://www.w3.org/TR/css-syntax-3/#ident-token-diagram

### Converting a String to an Identifier

This procedure takes a [logical string] `value` and returns an unquoted
SassScript string whose contents is an [`<ident-token>`] `ident` such that:

* `ident`'s value is equal to `value`.

* `ident` only contains escape codes for code points whose literal
  representation would cause it not to be a valid `<ident-token>`, code points
  in [private use areas], or U+FFFD REPLACEMENT CHARACTER.

* All code points in private use areas and U+FFFD REPLACEMENT CHARACTER are
  represented as escape codes.

* Escape codes use the lowercase hexadecimal representation with no leading `0`s
  for code points in private use areas, ASCII control characters and digits,
  U+007F DELETE, and U+FFFD REPLACEMENT CHARACTER; and for no other code points.

* All hexadecimal escape codes are followed by U+0020 SPACE.
  
[private use areas]: https://en.wikipedia.org/wiki/Private_Use_Areas

If there is no such identifier, throw an error.

> This is intended to uniquely determine a single identifier representation that
> can represent any given logical string except those which cannot be
> represented as identifiers (the empty string or strings containing U+0000
> NULL).

## Meta-Programming Functions

### `calc-name()`

Change the final bullet point to:

* Return `$calc`'s name [as an identifier].

[as an identifier]: #converting-a-string-to-an-identifier

### `function-exists()`

Replace the second bullet point with

* Let `name` be `$name`'s [semantic value].

Replace all further references to `$name` with `name`.

### `get-function()`

Replace the second bullet point with

* Let `name` be `$name`'s [semantic value].

Replace all further references to `$name` with `name`.

Replace the bullet point

* Return a plain CSS function string with the name `$name` and the arguments
  `$args`.

with

* Return a plain CSS function string whose name is `$name` [as an identifier]
  and whose arguments are `$args`.

### `get-mixin()`

Replace the second bullet point with

* Let `name` be `$name`'s [semantic value].

Replace all further references to `$name` with `name`.

### `get-module()`

Add the following bullet point between the second and third:

* Let `namespace` be `$module`'s [semantic value].

Replace all further references to `$module` with `namespace`.

### `global-variable-exists()`

Replace the second bullet point with

* Let `name` be `$name`'s [semantic value].

Replace all further references to `$name` with `name`.

### `keywords()`

```
keywords($args)
```

Add the following definition:

* If `$args` is not an argument list, throw an error.

* Return a map whose keys are the named arguments in `$args` [as identifiers]
  and whose values are those arguments' values.

[as identifiers]: #converting-a-string-to-an-identifier

### `load()`

Replace the third bullet point with:

* Let `config` be the empty configuration if `$with` is null.

* Otherwise, let `config` be a configuration whose variable names are the
  [semantic values] of `$with`'s keys and whose values are those keys' values.
  Throw an error if multiple keys have the same semantic value.

[semantic values]: #getting-the-semantic-value-of-a-string

### `mixin-exists()`

Replace the second bullet point with

* Let `name` be `$name`'s [semantic value].

Replace all further references to `$name` with `name`.

### `module-functions()`

Replace the second bullet point with:

* Return a map whose keys are the names of functions in `module` [as
  identifiers] and whose values are the corresponding functions.

### `module-mixins()`

Replace the second bullet point with:

* Return a map whose keys are the names of mixins in `module` [as identifiers]
  and whose values are the corresponding mixins.

### `module-variables()`

Replace the second bullet point with:

* Return a map whose keys are the names (without `$`) of variables in `module`
  [as identifiers] and whose values are the corresponding values.

### `variable-exists()`

Replace the second bullet point with

* Let `name` be `$name`'s [semantic value].

Replace all further references to `$name` with `name`.

## Arguments

When evaluating a map as a rest parameter, use its keys' [semantic values] as
parameter names.

When creating an argument list, make the keys for named arguments the argument
names [as identifiers].

## Deprecation Process

> In addition to this deprecation process, all the observable changes in 1.106.0
> described above will be changed back to their previous behavior.

### Phase 1

In the first phase of deprecation, most behaviors that will change will instead
emit a deprecation warning names `meta-escape`. This will be emitted when:

* A quoted string whose textual value is not an [`<ident-token>`] is passed to
  `function-exists()`, `global-variable-exists()`, `mixin-exists()`,
  `variable-exists()`, `get-function()`, `get-mixin()`, or `get-module()`.

* An anti-identifier is passed to `function-exists()`,
  `global-variable-exists()`, `mixin-exists()`, `variable-exists()`,
  `get-function()`, `get-mixin()`, or `get-module()`.

* An unquoted string whose textual value is not equal to its [semantic value]
  [as an identifier] is passed to `function-exists()`,
  `global-variable-exists()`, `mixin-exists()`, `variable-exists()`,
  `get-function()`, `get-mixin()`, or `get-module()`.

The changes to `load()` and to named rest arguments will be implemented as
described above, since all behaviors that are changing already produced errors
in Dart Sass 1.106.0.

The changes to `calc-name()`, `keywords()`, `module-functions()`,
`module-mixins()`, and `module-variables()` will *not* be implemented as part of
phase 1, but they will also not be deprecated, since there's no reliable way to
tell when the caller is relying on the aspects of these behaviors that are
changing.

### Phase 2

In the second phase, the changes to `calc-name()`, `keywords()`,
`module-functions()`, `module-mixins()`, and `module-variables()` will go into
effect. This will happen in the Dart Sass 2.0.0 release, which already includes
a number of breaking changes. We've committed to not including breaking changes
for any additional deprecations in Dart Sass 2.0.0, so it can't include any of
the breaking changes that will emit the `meta-escape` breaking change.

### Phase 3

In the third phase, we'll obsolete the `meta-escape` breaking change and
implement all the changes described above.
