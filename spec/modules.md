# Modules

## Table of Contents

* [Definitions](#definitions)
  * [Member](#member)
  * [CSS Tree](#css-tree)
  * [Configuration](#configuration)
  * [Module](#module)
  * [Module Graph](#module-graph)
  * [Import Context](#import-context)
  * [Built-In Module](#built-in-module)
  * [Importer](#importer)
  * [Filesystem Importer](#filesystem-importer)
  * [Package Importers](#package-importers)
  * [Node Package Importer](#node-package-importer)
  * [Global Importer List](#global-importer-list)
  * [Underscore-Insensitive](#underscore-insensitive)
  * [Basename](#basename)
  * [Dirname](#dirname)
* [Syntax](#syntax)
* [Procedures](#procedures)
  * [Loading a Module](#loading-a-module)
  * [Loading a Source File](#loading-a-source-file)
  * [Resolving a `file:` URL](#resolving-a-file-url)
  * [Resolving a `file:` URL for Extensions](#resolving-a-file-url-for-extensions)
  * [Resolving a `file:` URL for Partials](#resolving-a-file-url-for-partials)
  * [Resolving a Member](#resolving-a-member)
  * [Node Package Importer Semantics](#node-package-importer-semantics)
    * [Node Algorithm for Resolving a `pkg:` URL](#node-algorithm-for-resolving-a-pkg-url)
    * [Resolving a package name](#resolving-a-package-name)
    * [Resolving the root directory for a package](#resolving-the-root-directory-for-a-package)
    * [Resolving package exports](#resolving-package-exports)
    * [Resolving package root values](#resolving-package-root-values)
    * [Export Load Paths](#export-load-paths)

## Definitions

### Member

A *member* is a Sass construct that's defined either by the user or the
implementation and is identified by a Sass identifier. This currently includes
[variables](variables.md), mixins, and functions (but *not* placeholder
selectors). All members have definitions associated with them, whose specific
structure depends on the type of the given member.

Two members are considered identical if they have the same name, type, source
location, and were defined in or forwarded from the same original module.

> Each member type has its own namespace in Sass, so for example the mixin
> `name` doesn't conflict with the function `name` or the variable `$name`.

### CSS Tree

A *CSS tree* is an abstract CSS syntax tree. It has multiple top-level CSS
statements like at-rules or style rules. The ordering of these statements is
significant. A CSS tree cannot contain any Sass-specific constructs, with the
notable exception of placeholder selectors.

An *empty CSS tree* contains no statements.

### Configuration

A *configuration* is a map from [variable](variables.md) names to SassScript
values and an opaque ID. An *empty configuration* contains no entries.

A new *configuration* ID is unique unless otherwise specified.

### Module

A *module* is a collection of various properties:

* A set of [members](#member) that contains at most one member of any given type
  and [underscore-insensitive] name. All lookups in this set are
  underscore-insensitive unless explicitly specified otherwise.

  [underscore-insensitive]: #underscore-insensitive

  > For example, a module may not have two variables named `$name`, although it
  > may contain a function and a mixin with the same name or two functions with
  > different names.

  > The names (and mixin and function signatures) of a module's members are
  > static, and can be determined without executing its associated source file.
  > This means that any possible module for a given source file has the same
  > member names and signatures regardless of the context in which those modules
  > are loaded.

* A set of [extensions][].

  [extensions]: at-rules/extend.md#extension

* A [CSS tree](#css-tree).

  > This tree is empty for [built-in modules](#built-in-module) and user-defined
  > modules that only define variables, functions, and mixins without including
  > any plain CSS rules.

* A list of references to other modules, known as the module's *dependencies*,
  in the same order as their [`@use` rules][] and/or [`@forward` rules][] appear
  in the module's source file. If a dependency is referred to from multiple
  rules, its order is determined by the first such rule.

  > Modules without a source file never have dependencies. Each dependency is
  > guaranteed to correspond to at least one `@use` rule or `@forward` rule.

  [`@use` rules]: at-rules/use.md
  [`@forward` rules]: at-rules/forward.md

* An optional [source file][].

  > Note that [built-in modules](#built-in-module) *do not* have source files
  > associated with them.

  [source file]: syntax.md#source-file

* An absolute URL, known as the module's *canonical URL*. If the module has a
  source file, this must be the same as the source file's canonical URL.

Once a user-defined module has been returned by [Executing a File][], it is
immutable except for its variable values. [Built-in modules](#built-in-module)
are always immutable.

[Executing a File]: spec.md#executing-a-file

### Module Graph

The set of [modules](#module) loaded in the course of processing a stylesheet
can be construed as a [directed acyclic graph][] where the vertices are modules
and the edges are [`@use` rules][] and/or [`@forward` rules][]. We call this the
*module graph*.

[directed acyclic graph]: https://en.wikipedia.org/wiki/Directed_acyclic_graph

The module graph is not allowed to contain cycles because they make it
impossible to guarantee that all dependencies of a module are available before
that module is loaded. Although the names and APIs of a dependency's members can
be determined without [executing][] it, Sass allows code to be executed during
load, so those members may not behave correctly when invoked before the
dependency is executed.

[executing]: spec.md#executing-a-file

### Import Context

An *import context* is a set of [members](#member) that contains at most one
member of any given type and name. It's always mutable.

> Import contexts serve as glue between the old [`@import` rule][] and the
> module system. It serves as a shared global namespace for stylesheets loaded
> using `@import` rules, while also preventing global names from leaking into or
> out of stylesheets loaded using [`@use` rules][] and/or [`@forward` rules][].

[`@import` rule]: at-rules/import.md

### Built-In Module

A *built-in module* is a module defined either by the Sass specification or by
the host environment of the Sass compilation in some implementation-specific
way. Modules defined by the Sass specification all have the scheme `sass:` and
are all described in [the `built-in-modules` directory][]. Modules defined
outside the Sass compilation may not use the scheme `sass:`.

[the `built-in-modules` directory]: built-in-modules

Built-in modules may contain mixins, variables, or functions, but they may never
contain CSS or extensions.

### Importer

An *importer* is a function that takes a string that may be either a relative or
absolute URL and returns three values: a string (the text of a stylesheet), a
syntax ("indented", "scss", or "css"), and an absolute URL (that
stylesheet's canonical URL). It may also return null to indicate that the
importer doesn't recognize the URL in question or cannot find a corresponding
stylesheet. If the URL is recognized but invalid, it should throw an error
rather than returning null. What constitutes "recognized" or "invalid" is left
up to the importer.

The details of an importer's behavior is typically defined by the end user in an
implementation-specific way. However, all importers must adhere to the following
contract:

* When the URL returned by an importer is passed back to that importer, it must
  return the same result.

* The importer must return the same result for all URLs that refer to the same
  file, although what specifically constitutes "the same file" is left up to the
  importer.

> Importers are represented as a single function in the spec to simplify the
> writing of algorithms, but implementations are encouraged to have users
> instead define two separate functions: a `canonicalize()` function that
> converts an input string into a canonical URL, and a `load()` function that
> loads the contents of a canonical URL. This allows implementations to avoid
> the overhead of reloading the same file over and over.

### Filesystem Importer

A *filesystem importer* is an [importer](#importer) with an associated absolute
`file:` URL named `base`. When a filesystem importer is invoked with a string
named `string`:

* Let `url` be the result of [parsing `string` as a URL][parsing a URL] with
  `base` as the base URL. If this returns a failure, throw that failure.

* If `url`'s scheme is not `file`, return null.

* Let `resolved` be the result of [resolving `url`](#resolving-a-file-url).

* If `resolved` is null, return null.

* Let `text` be the contents of the file at `resolved`.

* Let `syntax` be:
  * "scss" if `url` ends in `.scss`.
  * "indented" if `url` ends in `.sass`.
  * "css" if `url` ends in `.css`.

  > The algorithm for [resolving a `file:` URL](#resolving-a-file-url)
  > guarantees that `url` will have one of these extensions.

* Return `text`, `syntax`, and `resolved`.

[parsing a URL]: https://url.spec.whatwg.org/#concept-url-parser

### Package Importers

This proposal defines the requirements for Package Importers written by users or
provided by implementations. It is a type of [Importer] and, in addition to the
standard requirements for importers, it must handle only non-canonical URLs that:

* have the scheme `pkg`, and
* whose path begins with a package name, and
* optionally followed by a path, with path segments separated with a forward
  slash.

The package name will often be the first path segment, but the importer may take
into account any conventions in the environment. For instance, Node supports
scoped package names, which start with `@` followed by 2 path segments. Note
that package names that contain non-alphanumeric characters may be less portable
across different package importers.

Package Importers must reject the following patterns:

* A URL whose path begins with `/`.
* A URL with non-empty/null username, password, host, port, query, or fragment.

[importer]: #importer

### Node Package Importer

The Node Package Importer is an implementation of a [Package Importer] using the
standards and conventions of the Node ecosystem. It has an associated absolute
`file:` URL named `entryPointURL`.

When the Node Package Importer is invoked with a string named `string`:

* If `string` is a relative URL, return null.

* Let `url` be the result of [parsing `string` as a URL][parsing a URL]. If this
  returns a failure, throw that failure.

* If `url`'s scheme is not `pkg:`, return null.

* If `url`'s path begins with a `/` or is empty, throw an error.

* If `url` contains a username, password, host, port, query, or fragment, throw
  an error.

* Let `sourceFile` be the canonical URL of the [current source file] that
  contained the load.

* If `sourceFile`'s scheme is `file:`, let `baseURL` be `sourceFile`.

* Otherwise, let `baseURL` be `entryPointURL`.

* Let `resolved` be the result of [resolving a `pkg:` URL as Node] with `url` and
  `baseURL`.

* If `resolved` is null, return null.

* Let `text` be the contents of the file at `resolved`.

* Let `syntax` be:

  * "scss" if `resolved` ends in `.scss`.

  * "indented" if `resolved` ends in `.sass`.

  * "css" if `resolved` ends in `.css`.

  > The algorithm for [resolving a `pkg:` URL as Node] guarantees that
  > `resolved` will have one of these extensions.

* Return `text`, `syntax`, and `resolved`.

[Package Importer]: #package-importers
[current source file]: ./spec.md#current-source-file
[resolving a `pkg:` URL as Node]: #node-algorithm-for-resolving-a-pkg-url

### Global Importer List

The *global importer list* is a list of importers that's set for the entire
duration of a Sass compilation.

### Underscore-Insensitive

If a name comparison is *underscore-insensitive*, it considers the characters
U+002D HYPHEN-MINUS and U+005F LOW LINE to be equal to one another.

> Long ago, Sass only supported underscores as separators for Sass identifiers.
> When support for hyphens was added to match CSS, they were made equivalent to
> underscores for backwards-compatibility. This insensitivity is generally only
> used for Sass member names, not any other identifiers.

### Basename

The *basename* of a URL is the final component of that URL's path.

### Dirname

The *dirname* of a URL is the prefix of that URL up to, but not including, the
beginning of its [basename](#basename).

## Syntax

The module system defines the following syntax for referring to names from other
modules:

<x><pre>
**PublicIdentifier**     ::= [\<ident-token>][] that doesn't begin with '-' or '_'
**NamespacedIdentifier** ::= [\<ident-token>][] | [\<ident-token>][] '.' PublicIdentifier
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram

No whitespace is allowed before or after the `'.'` in `NamespacedIdentifier`.

## Procedures

### Loading a Module

This algorithm takes a string `argument` and [configuration](#configuration)
`config` and returns a [module](#module):

* If `argument` is a valid URL with scheme `sass`:

  * If `config` is not empty, throw an error.

  * If a [built-in module](#built-in-module) exists with the exact given URL,
    return it.

  * Otherwise, throw an error.

* Let `file` be the result of [loading the file](#loading-a-source-file) at
  `argument`.

* If `file` is null, throw an error.

* If `file` has already been [executed] by the [Loading a Module] procedure:

  [executed]: spec.md#executing-a-file

  * If `config` is not empty and has a different ID than the configuration that
    was passed the first time `file` was executed by the [Loading a Module]
    procedure, throw an error.

    > An ID may be reused in a new configuration via [`@forward ... with`].

  * Otherwise, return the module that execution produced.

  [Loading a Module]: #loading-a-module

[`@forward ... with`]: at-rules/forward.md#semantics

* If `file` is currently being executed, throw an error.

  > This disallows circular `@use`s, which ensures that modules can't be used
  > until they're fully initialized.

* Otherwise, return the result of [executing][] `file` with `config` and a new
  [import context](#import-context).

  > For simplicity, the spec creates an import context for every module.
  > Implementations are encouraged to avoid eagerly allocating resources for
  > imports, though, to make use-cases only involving `@use` more efficient.

### Loading a Source File

This algorithm takes a string, `argument`, and returns either a [source file] or
null.

* If `argument` is a relative URL and the [current source file] has an
  [importer] `importer`:

  * If the current source file has a canonical URL `canonical`, let `url` be the
    result of [parsing `argument` as a URL][parsing a URL] with `canonical` as
    the base URL. Otherwise, let `url` be `argument`.

    > Allowing `url` to remain relative here in the absence of a canonical URL
    > is a bit of a hack, but it's useful in that many importers (including
    > [filesystem importer]s) act as "load paths", resolving URLs relative to
    > some implicit base URL rather than expecting all inputs to be absolute.

  * Let `result` be the result of passing `url` to `base`.

  * If `result` is not null:

    * Let `ast` be the result of [parsing] `result`'s text as `result`'s syntax.

    * Return a source file with `ast` as its abstract syntax tree, `result`'s
      URL as its canonical URL, and the current source file's importer as its
      importer.

* For each `importer` in the [global importer list](#global-importer-list):

  * Let `result` be the result of passing `argument` to `importer`.

  * If `result` is not null:

    * Let `ast` be the result of [parsing] `result`'s text as `result`'s syntax.

    * Return a source file with `ast` as its abstract syntax tree, `result`'s
      URL as its canonical URL, and `importer` as its importer.

* Return null.

[filesystem importer]: #filesystem-importer
[parsing]: syntax.md#parsing-text

### Resolving a `file:` URL

This algorithm takes a URL, `url`, whose scheme must be `file` and returns
either another URL that's guaranteed to point to a file on disk or null.

* Let `resolved` be the result of [resolving `url` for extensions][resolving for
  extensions].

* If `resolved` is not null, return it. Otherwise:

* Let `index` be `url` + `"/index"`

* Return the result of [resolving `index` for extensions][resolving for
  extensions].

[resolving for extensions]: #resolving-a-file-url-for-extensions

### Resolving a `file:` URL for Extensions

This algorithm takes a URL, `url`, whose scheme must be `file` and returns
either another URL that's guaranteed to point to a file on disk or null.

* If `url` ends in `.scss`, `.sass`, or `.css`:

  * If this algorithm is being run for an `@import`:

    * Let `suffix` be the trailing `.scss`, `.sass`, `.css` in `url`, and
      `prefix` the portion of `url` before `suffix`.

    * If the result of [resolving `prefix` + `".import"` + `suffix` for
      partials][resolving for partials] is not null, return it.

  * Otherwise, return the result of [resolving `url` for partials][resolving for
    partials].

  > `@import`s whose URLs explicitly end in `.css` will have been treated as
  > plain CSS `@import`s before this algorithm even runs, so `url` will only end
  > in `.css` for `@use` rules.

* If this algorithm is being run for an `@import`:

  * Let `sass` be the result of [resolving `url` + `".import.sass"` for
    partials][resolving for partials].

  * Let `scss` be the result of [resolving `url` + `".import.scss"` for
    partials][resolving for partials].

  * If neither `sass` nor `scss` are null, throw an error.

  * Otherwise, if exactly one of `sass` and `scss` is null, return the other
    one.

  * Otherwise, if the result of [resolving `url` + `".import.css"` for
    partials][resolving for partials] is not null, return it.

* Let `sass` be the result of [resolving `url` + `".sass"` for
  partials][resolving for partials].

* Let `scss` be the result of [resolving `url` + `".scss"` for
  partials][resolving for partials].

* If neither `sass` nor `scss` are null, throw an error.

* Otherwise, if exactly one of `sass` and `scss` is null, return the other
  one.

* Otherwise, return the result of [resolving `url` + `".css"` for
  partials][resolving for partials]. .

[resolving for partials]: #resolving-a-file-url-for-partials

### Resolving a `file:` URL for Partials

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

### Resolving a Member

This algorithm takes a [member](#member) name `name` and a member type `type`,
and returns a member of type `type` or null.

* If `name` is a plain `Identifier` or a `Variable` that's not a
  `NamespacedVariable`:

  * Let `scope` be the [current scope] or its innermost parent such that `scope`
    has a member of type `type` named `name`, or null if no such scope exists.

  * If `scope` is not null, return `scope`'s value of type `type` named `name`.

  [current scope]: spec.md#scope

* If `name` is a [`NamespacedIdentifier`](#syntax) of the form
  `namespace.raw-name` or a [`Variable`][] of the form `namespace.$raw-name`:

  [`Variable`]: variables.md#syntax

  * Let `use` be the [`@use` rule][] in the [current source file][] whose
    namespace is `namespace`. If there isn't exactly one such rule, throw an
    error.

    > Unlike other identifiers in Sass, module namespaces *do not* treat `-` and
    > `_` as equivalent.

  * If `use` hasn't been executed yet, throw an error.

  * Otherwise, let `module` be [`use`'s module][].

  * Return the member of `module` with type `type` and name `raw-name`. If there
    is no such member, throw an error.

  [`@use` rule]: at-rules/use.md
  [`use`'s module]: at-rules/use.md#a-use-rules-module

* If `type` is not "variable" and the current source file contains a top-level
  definition of a member of type `type` named `name`:

  > Local function and mixin definitions shadow those from global `@use` rules,
  > so that an upstream package adding a member is less likely to break its
  > downstream dependencies. We exclude variables from this because a top-level
  > variable definition will set the module's variable value rather than
  > defining a new variable local to this module.

  * If the [current import context][] contains a member `member` of type `type`
    named `name`, return it.

    > This includes member definitions within the current module.

  * Otherwise, return null.

    > It's an error to refer to a local member before it's defined, even if a
    > member with the same name is defined in a loaded module. The referent to a
    > member is guaranteed not to change due to definitions later in the file.

  [current import context]: spec.md#current-import-context

* Let `members` be the set of [unique][] members of type `type` named `name` in
  [modules of][] the global `@use` rules.

  [unique]: #member
  [modules of]: at-rules/use.md#a-use-rules-module

* If the current import context contains a member `member` of type `type` named
  `name`:

  * If `members` is not empty, throw an error.

  * Otherwise, return `member`.

* Otherwise, if `members` contains more than one member, throw an error.

  > This ensures that, if a new version of a library produces a conflicting
  > name, it causes an immediate error.

* Otherwise, if `modules` contains a single module, return the member of
  type `type` named `name` in that module.

* Otherwise, if the implementation defines a global member `member` of type
  `type` named `name`, return that member.

  > This includes the global functions and mixins defined as part of the Sass
  > spec, and may also include other members defined through the
  > implementation's host language API.

* Otherwise, return null.

### Node Package Importer Semantics

#### Node Algorithm for Resolving a `pkg:` URL

This algorithm takes a URL with scheme `pkg:` named `url`, and a URL `baseURL`.
It returns a canonical `file:` URL or null.

* Let `fullPath` be `url`'s path.

* Let `packageName` be the result of [resolving a package name] with `fullPath`,
  and `subpath` be `fullPath` without the `packageName`.

* Let `packageRoot` be the result of [resolving the root directory for a
  package] with `packageName` and `baseURL`.

* If a `package.json` file does not exist at `packageRoot`, throw an error.

* Let `packageManifest` be the result of parsing the `package.json` file at
  `packageRoot` as [JSON].

* Let `resolved` be the result of [resolving package exports] with
  `packageRoot`, `subpath`, and `packageManifest`.

* If `resolved` has the scheme `file:` and an extension of `sass`, `scss` or
  `css`, return it.

* Otherwise, if `resolved` is not null, throw an error.

* If `subpath` is empty, return the result of [resolving package root values].

* Let `resolved` be `subpath` resolved relative to `packageRoot`.

* Return the result of [resolving a `file:` URL] with `resolved`.

[Resolving package exports]: #resolving-package-exports
[resolving package root values]: #resolving-package-root-values
[resolving a package name]: #resolving-a-package-name
[JSON]: https://datatracker.ietf.org/doc/html/rfc8259
[resolving the root directory for a package]: #resolving-the-root-directory-for-a-package
[resolving a `file:` URL]: #resolving-a-file-url

#### Resolving a package name

This algorithm takes a string, `path`, and returns the portion that identifies
the Node package.

* If `path` starts with `@`, it is a scoped package. Return the first 2 [URL path
  segments], including the separating `/`.

* Otherwise, return the first URL path segment.

#### Resolving the root directory for a package

This algorithm takes a string, `packageName`, and an absolute URL `baseURL`, and
returns an absolute URL to the root directory for the most proximate installed
`packageName`.

* Return the result of `PACKAGE_RESOLVE(packageName, baseURL)` as defined in
  the [Node resolution algorithm specification].

[Node resolution algorithm specification]: https://nodejs.org/api/esm.html#resolution-algorithm-specification

#### Resolving package exports

This algorithm takes a package.json value `packageManifest`, a directory URL
`packageRoot` and a relative URL path `subpath`. It returns a file URL or null.

* Let `exports` be the value of `packageManifest.exports`.

* If `exports` is undefined, return null.

* Let `subpathVariants` be the result of [Export load paths] with `subpath`.

* Let `resolvedPaths` be a list of the results of calling
  `PACKAGE_EXPORTS_RESOLVE(packageRoot, subpathVariant, exports, ["sass",
  "style"])` as defined in the [Node resolution algorithm specification], with
  each `subpathVariants` as `subpathVariant`.

  > The PACKAGE_EXPORTS_RESOLVE algorithm always includes a `default` condition,
  > so one does not have to be passed here.

* If `resolvedPaths` contains more than one resolved URL, throw an error.

* If `resolvedPaths` contains exactly one resolved URL, return it.

* If `subpath` has an extension, return null.

* Let `subpathIndex` be `subpath` + `"/index"`.

* Let `subpathIndexVariants` be the result of [Export load paths] with `subpathIndex`.

* Let `resolvedIndexPaths` be a list of the results of calling
  `PACKAGE_EXPORTS_RESOLVE(packageRoot, subpathVariant, exports, ["sass",
  "style"])` as defined in the [Node resolution algorithm specification], with
  each `subpathIndexVariants` as `subpathVariant`.

* If `resolvedIndexPaths` contains more than one resolved URL, throw an error.

* If `resolvedIndexPaths` contains exactly one resolved URL, return it.

* Return null.

> Where possible in Node, implementations can use [resolve.exports] which
> exposes the Node resolution algorithm, allowing for per-path custom
> conditions, and without needing filesystem access.

[Export load paths]: #export-load-paths
[resolve.exports]: https://github.com/lukeed/resolve.exports

#### Resolving package root values

This algorithm takes a string `packagePath`, which is the root directory for a
package, and `packageManifest`, which is the contents of that package's
`package.json` file, and returns a file URL.

* Let `sassValue` be the value of `sass` in `packageManifest`.

* If `sassValue` is a relative path with an extension of `sass`, `scss` or
  `css`:

  * Return the canonicalized `file:` URL for `${packagePath}/${sassValue}`.

* Let `styleValue` be the value of `style` in `packageManifest`.

* If `styleValue` is a relative path with an extension of `sass`, `scss` or
  `css`:

  * Return the canonicalized `file:` URL for `${packagePath}/${styleValue}`.

* Otherwise return the result of [resolving a `file:` URL for extensions] with
  `packagePath + "/index"`.

[resolving a `file:` URL for extensions]: #resolving-a-file-url-for-extensions

[URL path segments]: https://url.spec.whatwg.org/#url-path-segment

#### Export Load Paths

This algorithm takes a relative URL path `subpath` and returns a list of
potential subpaths, resolving for partials and file extensions.

* Let `paths` be a list.

* If `subpath` ends in `.scss`, `.sass`, or `.css`:

  * Add `subpath` to `paths`.

* Otherwise, add `subpath`, `subpath` + `.scss`, `subpath` + `.sass`, and
  `subpath` + `.css` to `paths`.

* If `subpath`'s [basename] does not start with `_`, for each `item` in
  `paths`, prepend `"_"` to the basename, and add to `paths`.

* Return `paths`.

[basename]: #basename
