# `@use`

The `@use` rule loads a [module][] from a URL, makes its members available to
the current stylesheet, and includes its CSS in the compilation output.

[module]: ../modules.md#module

## Table of Contents

* [Definitions](#definitions)
  * [A `@use` Rule's Module](#a-use-rules-module)
* [Syntax](#syntax)
* [Procedures](#procedures)
  * [Determining a `@use` Rule's Namespace](#determining-a-use-rules-namespace)
* [Semantics](#semantics)

## Definitions

### A `@use` Rule's Module

A `@use` rule's *module* is a [module][] associated with a `@use` rule. This
module is only associated once the rule has been [executed](#semantics).

[module]: ../modules.md#module

## Syntax

The grammar for the `@use` rule is as follows:

<x><pre>
**UseRule**         ::= '@use' QuotedString AsClause? WithClause?
**AsClause**        ::= 'as' ('\*' | [\<ident-token>][])
**WithClause**      ::= 'with' '('
&#32;                     KeywordArgument (',' KeywordArgument)\* ','?
&#32;                   ')'
**KeywordArgument** ::= '$' [\<ident-token>][] ':' Expression
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram

`@use` rules must be at the top level of the document, and must come before any
rules other than `@charset` or `@forward`. The `QuotedString`'s contents, known
as the rule's *URL*, must be a [valid URL string][] (for non-[special][] base
URL). No whitespace is allowed after `$` in `KeywordArgument`.

[valid URL string]: https://url.spec.whatwg.org/#valid-url-string
[special]: https://url.spec.whatwg.org/#special-scheme

> Because each `@use` rule affects the namespace of the entire [source file][]
> that contains it, whereas most other Sass constructs are purely imperative,
> keeping it at the top of the file helps reduce confusion.
>
> [source file]: ../syntax.md#source-file

> Variable declarations aren't rules, and so *are* valid before or between
> `@use` and `@forward` rules. This makes it possible to define intermediate
> variables when passing configuration to a `WithClause`.
>
> ```scss
> @use "sass:color";
>
> $base-color: #abc;
> @use "library" with (
>   $base-color: $base-color,
>   $secondary-color: color.scale($base-color, $lightness: -10%),
> );
> ```

A `@use` rule's *namespace* is determined using [this
algorithm](#determining-a-use-rules-namespace). If the algorithm for determining
a namespace fails for a `@use` rule, that rule is invalid. If it returns `null`,
that rule is called *global*. A namespace is used to identify the used
[module][]'s members within the current [source file][].

## Procedures

### Determining a `@use` Rule's Namespace

This algorithm takes a `@use` rule `rule`, and returns either an identifier or
`null`.

> This algorithm is context-independent, so a namespace for a `@use` rule can be
> determined without reference to anything outside the syntax of that rule.

* If `rule` has an `'as'` clause `as`:

  * If `as` has an identifier, return it.

  * Otherwise, return `null`. The rule is global.

* Let `path` be the `rule`'s URL's [path][URL path].

  [URL path]: https://url.spec.whatwg.org/#concept-url-path

* Let `basename` be the text after the final `/` in `path`, or the entire `path`
  if `path` doesn't contain `/`.

* Let `module-name` be the text before the first `.` in `basename`, or the entire
  `basename` if `basename` doesn't contain `.`.

* If `module-name` begins with `_`, remove the leading `_` and set `module-name`
  to the result.

* If `module-name` isn't a Sass identifier, throw an error.

* Return `module-name`.

## Semantics

To execute a `@use` rule `rule`:

* If `rule` has a namespace that's the same as another `@use` rule's namespace
  in [the current source file][], throw an error.

  [the current source file]: ../spec.md#current-source-file

* Let `rule-config` be the empty configuration.

* If `rule` has a `WithClause`:

  * For each `KeywordArgument` `argument` in this clause:

    * Let `value` be the result of evaluating `argument`'s expression.

    * Add a variable to `rule-config` with the same name as `argument`'s identifier
      and with `value` as its value.

* Let `module` be the result of [loading the module][] with `rule`'s URL string
  and `rule-config`.

  [loading the module]: ../modules.md#loading-a-module

* For every variable name `name` in `rule-config`:

  * Let `variable` be the variable in `module` named `name`. If no such variable
    exists, throw an error.

  * If `variable` wasn't declared with a `!default` flag, throw an error.

* Set [`rule`'s module](#a-use-rules-module) to `module`.

