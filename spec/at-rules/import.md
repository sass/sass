# Import

The `@import` rule is the legacy way of splitting styles across multiple files
in Sass. The [`@use` rule][] should generally be used instead, but `@import` is
still supported for backwards-compatibility.

[`@use` rule]: use.md

## Table of Contents

* [Syntax](#syntax)
* [Semantics](#semantics)

## Syntax

<x><pre>
**ImportRule**            ::= '@import' (ImportArgumentNoMedia ',')* ImportArgument
**ImportArgumentNoMedia** ::= ImportUrl ImportModifierNoMedia*
**ImportArgument**        ::= ImportUrl ImportModifier
**ImportModifierNoMedia** ::= InterpolatedIdentifier* (ImportFunction | ImportSupports)
**ImportModifier**        ::= ImportModifierNoMedia* InterpolatedIdentifier* ImportMedia?
**ImportMedia**           ::= [MediaFeatureInParens] (',' [MediaQueryList])*
&#32;                       | InterpolatedIdentifier (',' [MediaQueryList])*
**ImportSupports**        ::= 'supports(' SupportsDeclaration ')'
**ImportFunction**        ::= [InterpolatedIdentifier]¹ '(' InterpolatedDeclarationValue? ')'
**ImportUrl**             ::= QuotedString | [InterpolatedUrl][]
</pre></x>

[InterpolatedIdentifier]: ../syntax.md#InterpolatedIdentifier
[InterpolatedUrl]: ../syntax.md#InterpolatedUrl
[MediaFeatureInParens]: media.md#syntax
[MediaQueryList]: media.md#syntax

1: This identifier may not be `"supports"` or `"and"`. No whitespace is allowed
   between it and the following `(`.

> This somewhat involved grammar was chosen over the simpler
>
> ```
> ImportRule     ::= '@import" (ImportArgument ',')* ImportArgument
> ImportArgument ::= ImportUrl ImportModifier*
> ImportArgument ::= ImportUrl ImportModifier*
> ImportModifier ::= ImportFunction | ImportSupports | MediaQueryList
> ```
>
> because this simpler version produces a number of problematic ambiguities. For
> example:
>
> * `@import "..." a b(c)` could be parsed as either:
>   * `MediaQuery "a", ImportFunction "b(c)"`
>   * `MediaQuery "a b", MediaQuery "(c)"`
> * `@import "..." a and(b)` could be parsed as either:
>   * `MediaQuery "a", ImportFunction "and(b)"`
>   * `MediaQuery "a and(b)"`
>
> To resolve these, this grammar explicitly indicates that a `MediaQueryList`
> and its associated commas may only appear at the end of an `ImportRule`, and
> delineates the exact circumstances in which an `InterpolatedIdentifier` is or
> is not part of a `MediaQueryList`.
>
> Note that this parses `@import "..." layer (max-width: 600px)` differently
> than the CSS standard: in CSS, `layer` is a CSS layering keyword but Sass
> parses it as part of a media query in this instance. This doesn't pose a
> problem in practice because Sass's semantics never depend on how import
> modifiers are parsed.

## Semantics

To execute an `@import` rule `rule`:

* For each of `rule`'s `ImportArgumentNoMedia`s and `ImportArgument`s `argument`:

  * If any of the following are true, `argument` is considered "plain CSS":

    * `argument`'s URL string begins with `http://` or `https://`.
    * `argument`'s URL string ends with `.css`.
    * `argument`'s URL is an `InterpolatedUrl`.
    * `argument` has at least one `ImportModifierNoMedia`.
    * `argument` has a non-empty `ImportModifier`.

    > Note that this means that imports that explicitly end with `.css` are
    > treated as plain CSS `@import` rules, rather than importing stylesheets as
    > CSS.

  * If `argument` is "plain CSS":

    * Evaluate each of the following within `argument`'s
      `ImportModifierNoMedia`s or `ImportModifier`s, and concatenate the results
      into a single string with `" "` between each one:

      * For an `InterpolatedIdentifier` outside an `ImportMedia`, concatenate
        the result of evaluating it.

      * For an `ImportFunction`, concatenate:
        * The result of evaluating its `InterpolatedIdentifier`
        * `"("`
        * The result of evaluating its `InterpolatedDeclarationValue` (or `""`
          if it doesn't have one)
        * `")"`

      * For an `ImportSupports`, concatenate:
        * `"supports("`
        * The result of evaluating its `SupportsDeclaration` as a CSS string
        * `")"

      * For an `ImportMedia`, concatenate the result of evaluating it as a
        [`MediaQueryList`] as a CSS string.

        > `ImportMedia` is a subset of the valid syntax of `MediaQueryList`, so
        > this will always work.

    * Add an `@import` with the evaluated modifiers to [the current module]'s
      CSS AST.

  * Otherwise, let `file` be the result of [loading the file][] with
    `argument`'s URL string. If this returns null, throw an error.

  * If `file`'s canonical URL is the same as that of any other [current source
    file][], throw an error.

  * Let `imported` be the result of [executing][] `file` with the empty
    configuration and the [current import context][], except that if
    `rule` is nested within at-rules and/or style rules, that context is
    preserved when executing `file`.

    > Note that this execution can mutate `import`.

  * Let `css` be the result of [resolving `imported`'s extensions][], except
    that if `rule` is nested within at-rules and/or style rules, that context is
    added to CSS that comes from modules loaded by `imported`.

    > This creates an entirely separate CSS tree with an entirely separate
    > `@extend` context than normal `@use`s of these modules. This means their
    > CSS may be duplicated, and they may be extended differently.

  * Add `css` to the current module's CSS.

  * Add `imported`'s [extensions][] to the current module.

   * If the `@import` rule is nested within at-rules and/or style rules, add each
     member in `imported` to the local [scope][].

   * Otherwise, add each member in `imported` to the current import context and
     the current module.

    > Members defined directly in `imported` will have already been added to
    > `import` in the course of its execution. This only adds members that
    > `imported` forwards.
    >
    > Members from `imported` override members of the same name and type that
    > have already been added to `import` and `module`.

  [the current module]: ../spec.md#current-module
  [loading the file]: ../modules.md#loading-a-source-file
  [current source file]: ../spec.md#current-source-file
  [executing]: ../spec.md#executing-a-file
  [current import context]: ../spec.md#current-import-context
  [resolving `imported`'s extensions]: extend.md#resolving-a-modules-extensions
  [extensions]: extend.md#extension
  [scope]: ../variables.md#scope
