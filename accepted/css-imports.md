# CSS Imports: Draft 3

*([Issue](https://github.com/sass/sass/issues/556), [Changelog](css-imports.changes.md))*

This proposal covers a long-awaited Sass feature: the ability to import plain
CSS files from Sass. Although the original plan was to wait on this support
until the [module system][] was in place and support CSS imports only with
`@use`, it has become desirable to support them sooner in order to have
compatibility with the existing LibSass implementation. See
[Background](#background) for more details.

[module system]: module-system.md

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Definitions](#definitions)
  * [Basename](#basename)
  * [Dirname](#dirname)
  * [Canonical URL of a Stylesheet](#canonical-url-of-a-stylesheet)
* [Semantics](#semantics)
  * [Handling an Import Rule](#handling-an-import-rule)
  * [Loading an Import](#loading-an-import)
  * [Resolving a `file:` URL](#resolving-a-file-url)
  * [Resolving a `file:` URL for partials](#resolving-a-file-url-for-partials)
  * [Parsing text as CSS](#parsing-text-as-css)
  * [Loading an entrypoint path](#loading-an-entrypoint-path)
* [Deprecation process](#deprecation-process)

## Background

Historically, the reference implementations of Sass—first Ruby Sass, then Dart
Sass—only supported importing other Sass files. However, LibSass supported
importing CSS files as well, interpreting them as though they were SCSS.
Although this technically violated the [implementation guide][]'s prohibition on
unilaterally extending the language, these CSS imports were useful and were
widely adopted in the Node.js community.

[implementation guide]: https://sass-lang.com/implementation

This became particularly clear when, at the language team's urging, LibSass
added [deprecation warnings][libsass#2611] for CSS imports and users were left
without a suitable replacement. The language team came together to discuss the
problem, and decided to move towards allowing CSS imports but forbidding the use
of non-CSS features in the imported files. This proposal describes the specifics
of that idea.

[libsass#2611]: https://github.com/sass/libsass/issues/2611

LibSass's behavior at time of writing is to import files with the extension
`.css` at the same precedence level as those with the `.scss` and `.sass`
extensions, and to throw an error if an import is ambiguous between a `.css`
file and a `.scss` or `.sass` file.

## Summary

> This section is non-normative.

This proposal seeks to strike a balance between preserving compatibility with
LibSass's existing behavior, and moving towards a more principled scheme for
loading CSS. This is particularly important as we intend to allow `@use` to load
CSS files without Sass features, so we want the existing CSS loading support to
be as similar as possible.

Locating CSS files for import works similarly under this proposal as it does in
LibSass currently: a relative `.css` file takes precedence over files with any
extension on the load path, a `.css` file earlier on the load path takes
precedence over a file with any extension later on the load path, and `foo.css`
takes precedence over `index/foo.scss`.

The only difference in loading scheme occurs when an import is ambiguous between
a `.css` file and a `.scss` or `.sass` file at the same path. LibSass currently
produces an error here, but in order to maximize compatibility with existing
Dart Sass (and Ruby Sass) behavior, this proposal has the `.scss` or `.sass`
file taking precedence. This is not a breaking change to LibSass's behavior,
since it only applies in situations that would previously have produced an
error.

This proposal diverges significantly from LibSass in parsing the imported CSS
files, though: it forbids all use of SCSS features in the parsed files. Most
SCSS features produce errors (rather than compiling to plain, likely-invalid
CSS) in order to help users who accidentally wrote SCSS in their CSS realize
what's going wrong. However, features like `@import` that overlap with plain CSS
continue to be rendered as CSS.

In order to avoid a sudden backwards-incompatible change in LibSass, this also
includes a proposal for a set of deprecation warnings that can be added to
LibSass's existing behavior to steer users away from using Sass features in
their imported CSS without entirely breaking their build process.

## Definitions

### Basename

The **basename** of a URL is the final component of that URL's path.

### Dirname

The **dirname** of a URL is the prefix of that URL up to, but not including, the
beginning of its [basename](#basename).

### Canonical URL of a Stylesheet

The **canonical URL** of a stylesheet is a URL associated with that stylesheet
that represents the location from which it was loaded.

The canonical URL for stylesheets are set by the algorithms for
[loading an import](#loading-an-import) and
[loading an entrypoint path](#loading-an-entrypoint-path).

## Semantics

This proposal defines a new algorithm for
[handling an `@import` rule](#handling-an-import-rule). It is intended to
replace the existing algorithm. I've also taken this opportunity to more
explicitly specify the behavior of `@import` rules.

This proposal omits importers, which while important, are out of scope.
Importers should be considered to effectively replace the algorithm for
[loading an import](#loading-an-import), possibly with another algorithm that
calls the existing algorithm to handle filesystem imports.

> Other than support for importing plain CSS, this algorithm is designed to
> accurately capture the current behavior of Ruby Sass and Dart Sass (when only
> filesystem importers are involved). It closely matches the implementation of
> Dart Sass, whereas Ruby Sass treats imported URLs as plain strings and
> operates on filesystem paths rather than `file:` URLs.

This proposal also defines a new algorithm for
[loading an entrypoint path](#loading-an-entrypoint-path), which is also
intended to replace the existing algorithm. This defines how a Sass
implementation should compile a file passed on the command line or through a
programming language API.

### Handling an Import Rule

To evaluate an `@import` rule:

* For each of that rule's arguments:

  * If any of the following are true, the argument is considered "plain CSS":

    * The imported URL begins with `http://` or `https://`.
    * The imported URL ends with `.css`.
    * The imported URL is syntactically defined as a `url()`.
    * The argument has a media query and/or a supports query.

    > Note that this means that imports that explicitly end with `.css` will
    > continue to be treated as plain CSS `@import` rules, rather than importing
    > stylesheets as CSS.

  * If the argument is "plain CSS":

    * Evaluate any interpolation it contains.

    * Add an `@import` with the evaluated string, media query, and/or supports
      query to the CSS AST.

  * Otherwise, let `stylesheet` be the result of
    [loading the imported string](#loading-an-import).

    If this returns null, throw an error.

  * If an AST with the same [canonical URL][] as `stylesheet` is currently being
    evaluated, throw an error.

  * Evaluate `stylesheet` in the global scope.

  [canonical URL]: #canonical-url-of-a-stylesheet

### Loading an Import

This algorithm takes a string, `argument`, and returns a Sass stylesheet.

* Let `root` be the current stylesheet's [canonical URL][] if its scheme is
  `file`, otherwise null.

* Let `bases` be a list beginning with `root` if it's non-null, followed by the
  absolute `file:` URLs of all import paths.

* For each `base` in `bases`:

  * Let `url` be the result of [parsing `argument` as a URL][] with `base` as
    the base URL.

    If this returns a failure, throw that failure.

  * If `url`'s scheme is not `file`, return null.

  * Let `resolved` be the result of [resolving `url`](#resolving-a-file-url).

  * If `resolved` is null:

    * Let `index` be [`dirname(url)`](#dirname) + `"index/"` +
      [`basename(url)`](#basename).

    * Set `resolved` to the result of
      [resolving `index`](#resolving-a-file-url).

  * If `resolved` is still null, continue to the next loop.

  * Let `text` be the contents of the file at `resolved`.

  * Let `ast` be:

    * The result of parsing `text` as SCSS if `resolved` ends in `.scss`.
    * The result of parsing `text` as the indented syntax if `resolved` ends in
      `.sass`.
    * The result of [parsing `text` as CSS](#parsing-text-as-css) if `resolved` ends
      in `.css`.

    > The algorithm for [resolving a `file:` URL](#resolving-a-file-url)
    > guarantees that `resolved` will have one of these extensions.

  * Return `ast` with the [canonical URL][] `resolved`.

  [parsing `argument` as a URL]: https://url.spec.whatwg.org/#concept-url-parser

* Return null.

### Resolving a `file:` URL

This algorithm takes a URL, `url`, whose scheme must be `file` and returns
either another URL that's guaranteed to point to a file on disk or null.

* If `url` ends in `.scss` or `.sass`, return the result of
  [resolving `url` for partials][resolving for partials].

* Let `sass` be the result of
  [resolving `url` + `".sass"` for partials][resolving for partials].

* Let `scss` be the result of
  [resolving `url` + `".scss"` for partials][resolving for partials].

* If neither `sass` nor `scss` are null, throw an error.

* If exactly one of `sass` and `scss` is null, return the other one.

* Return the result of
  [resolving `url` + `".css"` for partials][resolving for partials].

[resolving for partials]: #resolving-a-file-url-for-partials

### Resolving a `file:` URL for partials

This algorithm takes a URL, `url`, whose scheme must be `file` and returns
either another URL that's guaranteed to point to a file on disk or null.

* If `url`'s [basename](#basename) begins with `"_"`:

  * If a file exists on disk at `url`, return `url`.

    Otherwise return null.

* Let `partial` be [`dirname(url)`](#dirname) + `"_"` +
  [`basename(url)`](#basename).

* If a file exists on disk at both `url` and `partial`, throw an error.

* If a file exists on disk at `url`, return `url`.

* If a file exists on disk at `partial`, return `partial`.

* Return null.

### Parsing text as CSS

This algorithm takes a string, `text`, and returns a Sass abstract syntax tree.

> This algorithm is designed with two goals in mind:
>
> 1. CSS imported from Sass should be as compatible with standard CSS as
>    possible. In some cases we err even more towards CSS compatibility than
>    SCSS does, because the CSS being imported is likely not written by someone
>    who knows to avoid things that Sass interprets specially (such as certain
>    `@import` URLs).
>
> 2. We should provide clear and eager feedback to users who accidentally try to
>    use Sass features in CSS imports. We don't allow these features, and we
>    want users to know that through error messages rather than digging through
>    generated CSS only to find that Sass features were passed through
>    unmodified. This is a particular concern because LibSass has historically
>    allowed the use of Sass features in CSS imports.

The algorithm for parsing text as CSS works like parsing text as SCSS, with some
modifications. The following productions should produce errors:

* Any at-rules that are defined in Sass and not in plain CSS. At the time of
  writing, this means:

  * `@at-root`
  * `@content`
  * `@debug`
  * `@each`
  * `@error`
  * `@extend`
  * `@for`
  * `@function`
  * `@if`
  * `@include`
  * `@mixin`
  * `@return`
  * `@warn`
  * `@while`

* An `@import` that contains interpolation in the `url()`, the media query, or
  the supports query.

* An `@import` that appears within a style rule or at-rule.

* An `@import` with more than one argument.

* A declaration followed by an open curly brace (that is, a nested declaration).

* A style rule appearing within another style rule.

* The parent selector `&`, either in a selector or a declaration value.

* Placeholder selectors.

* All built-in functions, *excluding* the following:

  * `rgb()`
  * `rgba()`
  * `hsl()`
  * `hsla()`
  * `grayscale()`
  * `invert()`
  * `alpha()`
  * `opacity()`

  > Note that user-defined functions are *not* forbidden, whether they're
  > defined using `@function` or through a host language API.

* Any function called with keyword arguments or variable-length arguments.

* Interpolation anywhere its contents would be evaluated. At the time of
  writing, this means:

  * At-rule values (including `@media` queries)
  * Declaration names
  * Declaration values
  * Style rule selectors

* All SassScript operations *except for*:

  * `/`
  * `not`
  * `or`
  * `and`

  > Note that although unary `-` is forbidden, the `-` that appears at the
  > beginning of a number literal is part of that literal and thus allowed.

* Parentheses in declaration values that aren't part of a CSS production.

* Map literals.

* The empty list literal `(,)`.

* Uses or declarations of Sass variables.

* `//`-style ("silent") comments.

In addition, some productions should be parsed differently than they would be in
SCSS:

* All functions that don't produce errors should be parsed as plain CSS
  functions, regardless of whether a Sass function with that name is defined.

* All `@import`s that don't produce errors should be parsed as static CSS
  imports.

* The tokens `not`, `or`, `and`, and `null` should be parsed as unquoted
  strings.

  > The `/` operation should be parsed as normal. Because variables,
  > parentheses, functions that return numbers, and all other arithmetic
  > expressions are disallowed, it will always compile to slash-separated values
  > rather than performing division.

### Loading an entrypoint path

This algorithm takes a string, `path`, that represents a file on the filesystem.
It returns a Sass stylesheet.

* Let `url` be the absolute `file:` URL corresponding to `path`.

* Let `text` be the contents of the file at `path`.

* Let `ast` be:

  * The result of parsing `text` as the indented syntax if `url` ends in
    `.sass`.
  * The result of [parsing `text` as CSS](#parsing-text-as-css) if `url` ends in
    `.css`.
  * The result of parsing `text` as SCSS otherwise.

  > The algorithm for [resolving a `file:` URL](#resolving-a-file-url)
  > guarantees that `resolved` will have one of these extensions.

* Return `ast` with the [canonical URL][] `url`.

## Deprecation process

Because Dart Sass has no existing behavior for CSS imports–they'll just fail to
find a file to import–it can just add the new behavior without need for a
deprecation process. But LibSass parses its CSS imports as SCSS, which means it
allows Sass features that this proposal forbids. In order to help users migrate
away from using these features without a sudden breakage, LibSass should
continue to parse CSS imports as SCSS, while emitting deprecation warnings for:

* All productions that would produce an error according to the algorithm for
  [parsing text as CSS](#parsing-text-as-css).

* All invocations of user-defined functions, whether they're defined using
  `@function` or through a host language API.

* The `not`, `or`, and `and` operations.

* The value `null`.

Where possible, LibSass should avoid emitting multiple deprecation warnings for
the same expression.

> For example, if a user writes `-$var`, producing a warning for the use of the
> variable already makes it clear that Sass features are in use. There's no
> additional value in emitting a warning for the unary minus operation.

These deprecation warnings should indicate that Sass features will cease to be
allowed in files named `.css` in the future, and suggest that users either use
plain CSS or rename their files to `.scss`.
