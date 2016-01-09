# The Next-Generation Sass Module System

This repository houses a proposal for the `@use` directive and associated module
system, which is intended to be the headlining feature for Sass 4. This is a
*living proposal*: it's intended to evolve over time, and is hosted on GitHub to
encourage community collaboration and contributions. Any suggestions or issues
can be brought up and discussed on [the issue tracker][issues].

[issues]: https://github.com/sass/proposal.module-system

Although this document describes some imperative processes when describing the
semantics of the module system, these aren't meant to prescribe a specific
implementation. Individual implementations are free to implement this feature
however they want as long as the end result is the same. However, there are
specific design decisions that were made with implementation efficiency in
mind—these will be called out explicitly in block-quoted "implementation note"s.

*Note: at the time of writing, the initial draft of the proposal is not yet
complete*.

## Background

The new `@use` directive is intended to supercede Sass's `@import` directive as
the standard way of sharing styles across Sass files. `@import` is the simplest
form of re-use: it does little more than directly include the target file in the
source file. This has caused numerous problems in practice: including the same
file more than once slows down compilation and produces redundant output, users
must manually namespace everything in their libraries, and there's no
encapsulation to allow them to keep moving pieces hidden.

The new module system is intended to address these shortcomings (among others)
and bring Sass's modularity into line with the best practices as demonstrated by
other modern languages. As such, the semantics of `@use` are is heavily based on
other languages' module systems, with Python and Dart being particularly strong
influences.

## Goals

### High-Level

These are the philosophical design goals for the module system at large. While
they don't uniquely specify a system, they do represent the underlying
motivations behind many of the lower-level design decisions.

* **Locality**. The module system should make it possible to understand a Sass
  file by looking only at that file. An important aspect of this is that names
  in the file should be resolved based on the contents of the file rather than
  the global state of the compilation. This also applies to authoring: an author
  should be able to be confident that a name is safe to use as long as it
  doesn't conflict with any name visible in the file.

* **Encapsulation**. The module system should allow authors, particularly
  library authors, to choose what API they expose. They should be able to define
  entities for internal use without making those entities available for external
  users to access or modify. This also includes the ability to "forward" public
  APIs from another file.

* **Configuration**. Sass is unusual among languages in that it encourages the
  use of files whose entire purpose is to produce side effects—specifically, to
  emit CSS. There's also a broader class of libraries that may not emit CSS
  directly, but do define configuration variables that are used in computations,
  sometimes at the top level. The module system should allow the user to
  flexibly use modules with side-effects, and shouldn't force global
  configuration.

### Low-Level

These are goals that are based less on philosophy than on practicality. For the
most part, they're derived from user feedback that we've collected about
`@import` over the years.

* **Using CSS files**. People often have CSS files that they want to bring into
  their Sass compilation. Historically, `@import` has been unable to do this due
  to its overlap with the plain-CSS `@import` directive and the requirement that
  SCSS remain a CSS superset. With a new directive name, this becomes possible.

* **Import once**. Because `@import` is a literal textual inclusion, multiple
  `@import`s of the same Sass file within the scope of a compilation will
  compile and run that file multiple times. At best this hurts compilation time,
  and it can also contribute to bloated CSS output when the styles themselves
  are duplicated. The new module system should only compile a file once, at
  least for the default configuration.

### Non-Goals

These are potential goals that we have explicitly decided to avoid pursuing for
various reasons. Some of them may be on the table for future work, but none are
expected to land in Sass 4.

* **Dynamic imports**. Allowing the path to a module to be defined dynamically,
  whether by including variables or including it in a conditional block, moves
  away from being declarative. In addition to making stylesheets harder to read,
  this makes any sort of static analysis more difficult—and actually impossible
  in the general case. It also limits the possibility of future implementation
  optimizations.

* **Importing multiple files at once**. In addition to the long-standing reason
  that this hasn't been supported—that it opens authors up to sneaky and
  difficult-to-debug ordering bugs—this violates the principle of locality by
  obfuscating which files are imported and thus where names come from.

* **Extend-only imports**. The idea of importing a file so that the CSS it
  generates isn't emitted unless it's `@extend`ed is cool, but it's also a lot
  of extra work. This is the most likely feature to end up in a future release,
  but it's not central enough to the module system to include in Sass 4.

## Definitions

### Member

A *member* is anything that's defined, either by the user or the implementation,
that is identified by a Sass identifier. This currently includes variables,
mixins, functions, and placeholder selectors. Each member type has its own
namespace, so for example the variable `$name` doesn't conflict with the
placeholder selector `%name`.

All members have definitions associated with them, whose specific structure
depends on the type of the given member. Variables, mixins, and functions have
intuitive definitions, but placeholder selectors' definitions just indicate
which [module](#module) they come from.

### CSS Tree

A *CSS tree* is an abstract CSS syntax tree. It has multiple top-level CSS
declarations like `@`-rules or rulesets. The ordering of the roots is
significant.

A CSS tree cannot contain any Sass-specific constructs, with the notable
exception of placeholder selectors. These are allowed so that modules' CSS may
be `@extend`ed.

An empty CSS tree contains no top-level declarations.

### Configuration

A *configuration* is a set of variables with associated SassScript values. It's
used when executing a [source file](#source-file) to customize its execution. It
may be empty—that is, it may contain no variables.

Two configurations are considered identical if they contain the same variables,
and if each pair of variables with the same name has values that are `==` to one
another.

### Module

A *module* is an abstract collection of [members](#members) as well as a
[CSS tree](#css-tree), although that tree may be empty. Each module may have
only one member of a given type and name (for example, a module may not have two
variables named `$name`). To satisfy this requirement, placeholder selectors are
de-duplicated.

Each module is uniquely identified by the combination of a URI and a
[configuration](#configuration). A given module can be produced by executing the
[source file](#source-file) identified by the module's URI with the module's
configuration.

### Source File

A *source file* is an entity uniquely identified by a URI. It can be executed
with a [configuration](#configuration) to produce a [module](#module). The names
of this module's members are static, and can be determined without executing the
file. This means that all modules for a given source file have the same member
names regardless of the configurations used for those modules.

There are five types of source file:

* Sass files, SCSS files, and CSS files are identified by file paths.

* Core libraries are identified by special URIs in an as-yet-undecided format.

* Implementations may define implementation-specific or pluggable means of
  defining source files, which can use any URI.

Each one has different execution semantics that are beyond the scope of this
document. Note that some of these are not or may not actually be files on the
file system.

## Syntax

The new directive will be called `@use`. The grammar for this directive is as
follows:

```
UseDirective ::= '@use' QuotedString (AsClause | NoPrefix)?
AsClause     ::= 'as' Identifier
NoPrefix     ::= 'no-prefix'
```

*Note: this only encompasses the syntax whose semantics are currently described
in this document. As the document becomes more complete, the grammar will be
expanded accordingly.*

`@use` directives must be at the top level of the document, and must come before
any directives other than `@charset`. Because each `@use` directive affects the
namespace of the entire [source file](#source-file) that contains it, whereas
most other Sass constructs are purely imperative, keeping it at the top of the
file helps reduce confusion.

## Semantics

### Loading Modules

When encountering a `@use` directive, the first step is to load the associated
[module](#module). To do so:

* Look up the [source file](#source-file) with the given URI. The process for
  doing this is out of scope of this document.

* If no such file can be found, throw a fatal error.

* If the source file has already been executed with an empty
  [configuration](#configuration), use the module that execution produced. This
  fulfills the "import once" low-level goal.

* Otherwise, execute that file with an empty configuration, and use the
  resulting module.

> **Implementation note:**
>
> Although this specification only requires that modules be cached and reused
> when compiling a single entrypoint, modules are intentionally
> context-independent enough to store and re-use across multiple entrypoints, as
> long as no source files change. For example, if the user requests that all
> Sass files beneath `stylesheets/sass` be compiled, modules may be shared
> between those separate compilations.

Each module loaded this may have an associated **prefix**, which is a Sass
identifier that's used to identify the module's [member](#member)s within the
current file. No two `@use` directives in a given file may share a prefix,
although any number may have no prefix. The prefix for a given `@use`
directive's module is determined as follows:

* If the directive has an `as` clause, use that clause's identifier.

* If the directive has a `no-prefix` clause, then it has no prefix.

* If the module's URI doesn't match the regular expression
  `(.*/)?([^/]+)(\.[^/]*)?`, the `@use` directive is malformed.

* Call the text captured by the second group of the regular expression the
  *module name*.

* If the module name isn't a Sass identifier, the `@use` directive is malformed.

* Use the module name.

This proposal follows Python and diverges from Dart in that `@use` imports
modules with a prefix by default. This is for two reasons. First, it seems to be
the case that language ecosystems with similar module systems either prefix all
imports by convention, or prefix almost none. Because Sass is not
object-oriented and doesn't have the built-in namespacing that classes provide
many other languages, its APIs tend to be much broader at the top level and thus
at higher risk for name conflict. Prefixing by default tilts the balance towards
always prefixing, which mitigates this risk.

Second, a default prefix scheme drastically reduces the potential for
inconsistency in prefix choice. If the prefix is left entirely up to the user,
different people may choose to prefix `strings.scss` as `strings`, `string`,
`str`, or `strs`. This taxes the reusability of code and knowledge, and
mitigating it is a benefit.
