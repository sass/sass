# Import

Imports are the fundamental mechanism for sharing code in Sass. This spec
describes the algorithm for [handling an `@import`
rule](#handling-an-import-rule), with the exception that it doesn't cover in
detail the behavior of user- or implementation-defined importers. An importer
should be considered to effectively replace the algorithm for [loading an
import](#loading-an-import), possibly with another algorithm that calls the
algorithm listed below to handle filesystem imports.

This spec also defines the algorithm for [loading an entrypoint
path](#loading-an-entrypoint-path), which defines how a Sass implementation
should compile a file passed on the command line or through a programming
language API.

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

### Handling an Import Rule

To evaluate an `@import` rule:

* For each of that rule's arguments:

  * If any of the following are true, the argument is considered "plain CSS":

    * The imported URL begins with `http://` or `https://`.
    * The imported URL ends with `.css`.
    * The imported URL is syntactically defined as a `url()`.
    * The argument has a media query and/or a supports query.

    > Note that this means that imports that explicitly end with `.css` are
    > treated as plain CSS `@import` rules, rather than importing stylesheets as
    > CSS.

  * If the argument is "plain CSS":

    * Evaluate any interpolation it contains.

    * Add an `@import` with the evaluated string, media query, and/or supports
      query to the CSS AST.

  * Otherwise, let `stylesheet` be the result of
    [loading the imported string](#loading-an-import).

    If this returns null, throw an error.

  * If an AST with the same [canonical URL][] as `stylesheet` is currently being
    evaluated, throw an error.

    [canonical URL]: #canonical-url-of-a-stylesheet

  * Evaluate `stylesheet` in the global scope.

### Loading an import

This algorithm takes a string, `argument`, and returns a Sass stylesheet.

* Let `root` be the current stylesheet's [canonical URL][] if its scheme is
  `file`, otherwise null.

* Let `bases` be a list beginning with `root` if it's non-null, followed by the
  absolute `file:` URLs of all import paths.

* For each `base` in `bases`:

  * Let `url` be the result of [parsing `argument` as a URL][] with `base` as
    the base URL.

    If this returns a failure, throw that failure.

    [parsing `argument` as a URL]: https://url.spec.whatwg.org/#concept-url-parser

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
