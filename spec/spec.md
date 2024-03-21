# Sass Specification

This directory contains the formal specification for the Sass language.

Sass is a *living specification*, which means that it's actively updated over
time without having distinctions between numbered versions. Different
implementations may support different subsets of the specification, although all
implementations are expected to work towards full support. The *reference
implementation* (currently [Dart Sass][]) will generally support as close to the
full spec as possible.

[Dart Sass]: https://sass-lang.com/dart-sass

This specification is incomplete, and is added to *lazily*. This means that
portions of the spec are only written when they're necessary as background for
new language proposals. The Sass team eventually hopes to specify every part of
the language this way.

## Table of Contents

* [Definitions](#definitions)
  * [Scope](#scope)
  * [Global Scope](#global-scope)
  * [Current Source File](#current-source-file)
  * [Current Configuration](#current-configuration)
  * [Current Import Context](#current-import-context)
  * [Current Module](#current-module)
* [Procedures](#procedures)
  * [Running in a New Scope](#running-in-a-new-scope)
* [Semantics](#semantics)
  * [Compiling a Path](#compiling-a-path)
  * [Compiling a String](#compiling-a-string)
  * [Executing a File](#executing-a-file)

## Definitions

### Scope

A *scope* is a mutable structure that contains:

* The scope's *variables*: a mapping from identifiers to SassScript values.
* The scope's *mixins*: a mapping from identifiers to mixins.
* The scope's *functions*: a mapping from identifiers to functions.
* The scope's *parent*: a reference to another scope, which may be unset.

One scope at a time is designated the *current scope*. By default, this is the
[global scope](#global-scope).

### Global Scope

The *global scope* is the scope shared among the top level of all Sass files. It
has no parent.

### Current Source File

The *current source file* is the [source file][] that was passed to the
innermost active invocation of [Executing a File](#executing-a-file).

[source file]: syntax.md#source-file

*All current source files* refer to all the source files passed to any active
invocation of Executing a File.

### Current Configuration

The *current configuration* is the [configuration][] that was passed to the
innermost active invocation of [Executing a File](#executing-a-file).

[configuration]: modules.md#configuration

### Current Import Context

The *current import context* is the [import context][] that was passed to the
innermost active invocation of [Executing a File](#executing-a-file).

[import context]: modules.md#import-context

### Current Module

The *current module* is the [module][] that was created by the innermost active
invocation of [Executing a File](#executing-a-file).

[module]: modules.md#module

> Because a module is only made immutable (other than its variables) when
> execution has finished, the current module is always mutable.

## Procedures

### Running in a New Scope

To run a set of steps *in a new scope*:

* Let `parent` be the [current scope].

  [current scope]: #scope

* Return the result of running the given steps with the current scope set to an
  empty scope with `parent` as its parent.

## Semantics

### Compiling a Path

> This an entrypoint to the specification; it's up to each implementation how it
> exposes this to the user.

This algorithm takes a local filesystem path `path`, an optional list of
[importers] `importers`, and an optional list of paths `load-paths`. It returns
a string.

* Let `text` be the result of decoding the binary contents of the file at
  `path`.

* Let `syntax` be:

  * "indented" if `path` ends in `.sass`.
  * "css" if `path` ends in `.css`.
  * "scss" otherwise.

* Let `url` be the absolute `file:` URL corresponding to `path`.

* Let `importer` be a [filesystem importer] with a null `base`.

* Return the result of [compiling](#compiling-a-string) `text` with `syntax`,
  `url`, `importer`, `importers`, and `load-paths`.

[importers]: modules.md#importer

### Compiling a String

> This an entrypoint to the specification; it's up to each implementation how it
> exposes this to the user.

This algorithm takes:

* a string `string`,
* a syntax `syntax` ("indented", "scss", or "css"),
* an optional URL `url`,
* an optional [importer] `importer`,
* an optional list of importers `importers`,
* and an optional list of paths `load-paths`.

[importer]: modules.md#importer

It runs as follows:

* Set the [global importer list] to `importers`.

* For each `path` in `load-paths`:

  * Let `base` be the absolute `file:` URL that refers to `path`.

  * Add a [filesystem importer] with base `base` to the global importer list.

* Let `ast` be the result of [parsing] `text` as `syntax`.

* If `url` is null:

  * If `importer` is not null, throw an error.

  * Set `url` to a unique value.

    > This ensures that all source files have a valid URL. When displaying this
    > value, implementations should help users understand the source of the string
    > if possible.

* If `importer` is null:

  * If `url` is a `file:` URL, set `importer` to be a [filesystem importer] with an
    arbitrary `base`.

    > This importer will only ever be passed absolute URLs, so its base won't
    > matter.

  * If `url` is not a `file:` URL, set `importer` to be a function that always
    returns null.

* Let `file` be the [source file][] with `ast`, canonical URL `url`, and
  importer `importer`.

* Let `module` be the result of [executing](#executing-a-file) `file`.

* Let `css` be the result of [resolving `module`'s extensions][].

  [resolving `module`'s extensions]: at-rules/extend.md#resolving-a-modules-extensions

* Return the result of converting `css` to a CSS string.

[filesystem importer]: modules.md#filesystem-importer
[parsing]: syntax.md#parsing-text
[global importer list]: modules.md#global-importer-list

### Executing a File

This algorithm takes a [source file][] `file`, a [configuration][] `config`, an
[import context][] `import`, and returns a [module][].

* Let `module` be an empty module with source file `file`.

* Let `uses` be an empty map from `@use` rules to modules.

* Execute each top-level statement as described in that statement's
  specification.

  > The semantics for executing each statement is defined in that statement's
  > individual specification.

* For each variable declaration `variable` with a `!global` flag in `file`,
  whether or not it was evaluated:

  * If `variable`'s name *doesn't* begin with `-` or `_` and `variable` is not
    yet in `module`, set `variable` to `null` in `module`.

    > This isn't necessary for implementations that follow the most recent
    > [variables spec][] and don't allow `!global` assignments to variables
    > that don't yet exist. However, at time of writing, all existing
    > implementations are in the process of deprecating the old `!global`
    > behavior, which allowed `!global` declarations to create new
    > variables.
    >
    > Setting all `!global` variables to `null` if they weren't otherwise set
    > guarantees the stability of static analysis by ensuring that the set of
    > variables a module exposes doesn't depend on how it was executed.

  [variables spec]: variables.md

* Return `module`. Its functions, mixins, and CSS are now immutable.
