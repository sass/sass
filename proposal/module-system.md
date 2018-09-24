# The Next-Generation Sass Module System: Draft 3

*([Issues](https://github.com/sass/sass/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3A%22%40use%22), [Changelog](module-system.changes.md))*

This repository houses a proposal for the `@use` rule and associated module
system, which is intended to be the headlining feature for Sass 4. This is a
*living proposal*: it's intended to evolve over time, and is hosted on GitHub to
encourage community collaboration and contributions. Any suggestions or issues
can be brought up and discussed on [the issue tracker][issues].

[issues]: https://github.com/sass/language/issues?q=is%3Aissue+is%3Aopen+label%3A%22proposal%3A+module+system%22

Although this document describes some imperative processes when describing the
semantics of the module system, these aren't meant to prescribe a specific
implementation. Individual implementations are free to implement this feature
however they want as long as the end result is the same. However, there are
specific design decisions that were made with implementation efficiency in
mind—these will be called out explicitly in block-quoted implementation notes.

## Table of Contents

* [Background](#background)
* [Goals](#goals)
  * [High-Level](#high-level)
  * [Low-Level](#low-level)
  * [Non-Goals](#non-goals)
* [Frequently Asked Questions](#frequently-asked-questions)
* [Definitions](#definitions)
  * [Member](#member)
  * [Extension](#extension)
  * [CSS Tree](#css-tree)
  * [Configuration](#configuration)
  * [Module](#module)
  * [Module Graph](#module-graph)
  * [Source File](#source-file)
  * [Entrypoint](#entrypoint)
  * [Import Context](#import-context)
* [Syntax](#syntax)
  * [`@use`](#use)
  * [`@forward`](#forward)
  * [Member References](#member-references)
* [Procedures](#procedures)
  * [Determining Namespaces](#determining-namespaces)
  * [Loading Modules](#loading-modules)
  * [Resolving Extensions](#resolving-extensions)
  * [Canonicalizing URLs](#canonicalizing-urls)
* [Semantics](#semantics)
  * [Compilation Process](#compilation-process)
  * [Executing Files](#executing-files)
  * [Resolving Members](#resolving-members)
  * [Module Mixins](#module-mixins)
  * [Forwarding Modules](#forwarding-modules)
  * [Importing Files](#importing-files)
* [Built-In Modules](#built-in-modules)
  * [New Functions](#new-functions)
    * [`module-variables()`](#module-variables)
    * [`module-functions()`](#module-functions)
  * [New Features For Existing Functions](#new-features-for-existing-functions)

## Background

The new `@use` at-rule is intended to supercede Sass's `@import` rule as the
standard way of sharing styles across Sass files. `@import` is the simplest
possible form of re-use: it does little more than directly include the target
file in the source file. This has caused numerous problems in practice:
including the same file more than once slows down compilation and produces
redundant output; users must manually namespace everything in their libraries;
there's no encapsulation to allow them to keep moving pieces hidden; and it's
very difficult for either humans or tools to tell where a given variable, mixin,
or function comes from.

The new module system is intended to address these shortcomings (among others)
and bring Sass's modularity into line with the best practices as demonstrated by
other modern languages. As such, the semantics of `@use` are is heavily based on
other languages' module systems, with Python and Dart being particularly strong
influences.

## Goals

### High-Level

These are the philosophical design goals for the module system as a whole. While
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

* **Configuration**. Sass is unusual among languages in that its design leads to
  the use of files whose entire purpose is to produce side effects—specifically,
  to emit CSS. There's also a broader class of libraries that may not emit CSS
  directly, but do define configuration variables that are used in computations,
  including computation of other top-level variables' values. The module system
  should allow the user to flexibly use modules with side-effects, and shouldn't
  force global configuration.

### Low-Level

These are goals that are based less on philosophy than on practicality. For the
most part, they're derived from user feedback that we've collected about
`@import` over the years.

* **Import once**. Because `@import` is a literal textual inclusion, multiple
  `@import`s of the same Sass file within the scope of a compilation will
  compile and run that file multiple times. At best this hurts compilation time
  for little benefit, and it can also contribute to bloated CSS output when the
  styles themselves are duplicated. The new module system should only compile a
  file once, at least for the default configuration.

* **Backwards compatibility**. We want to make it as easy as possible for people
  to migrate to the new module system, and that means making it work in
  conjunction with existing stylesheets that use `@import`. Existing stylesheets
  that only use `@import` should have identical importing behavior to earlier
  versions of Sass, and stylesheets should be able to change parts to `@use`
  without changing the whole thing at once.

### Non-Goals

These are potential goals that we have explicitly decided to avoid pursuing as
part of this proposal for various reasons. Some of them may be on the table for
future work, but we don't consider them to be blocking the module system.

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
  but it's not central enough to include in the initial module system.

* **Context-independent modules**. It's tempting to try to make the loaded form
  of a module, including the CSS it generates and the resolved values of all its
  variables, totally independent of the entrypoint that cause it to be loaded.
  This would make it possible to share loaded modules across multiple
  compilations and potentially even serialize them to the filesystem for
  incremental compilation.

  However, it's not feasible in practice. In practice, modules that generate CSS
  almost always do so based on some configuration, which may be changed by
  different entrypoints rendering caching useless. What's more, multiple modules
  may depend on the same shared module, and one may modify its configuration
  before the other uses it. Forbidding this case in general would effectively
  amount to forbidding modules from generating CSS.

  Fortunately, implementations have a lot of leeway to cache information that
  the can statically determine to be context-independent, including source trees
  and potentially even constant-folded variable values and CSS trees. Full
  context independence isn't likely to provide much value in addition to that.

* **Increased strictness**. Large teams with many people often want stricter
  rules around how Sass libraries are written, to enforce best practices and
  quickly catch mistakes. It's tempting to use a new module system as a lever to
  push strictness further; for example, we could make it harder or even
  impossible to have partials directly generate CSS, or we could decline to move
  functions we'd prefer people avoid to the new built-in modules.

  As tempting as it is, though, we want to make all existing use-cases as easy
  as possible in the new system, *even if we think they should be avoided*. This
  module system is already a major departure from the existing behavior, and
  will require a substantial amount of work from Sass users to support. We want
  to make this transition as easy as possible, and part of that is avoiding
  adding any avoidable hoops users have to jump through to get their existing
  stylesheets working in the new module system.

  Once `@use` is thoroughly adopted in the ecosystem, we can start thinking
  about increased strictness in the form of lints or TypeScript-style
  `--strict-*` flags.

## Frequently Asked Questions

* **Why this privacy model?** We considered a number of models for declaring
  members to be private, including a JS-like model where only members that were
  explicitly exported from a module were visible and a C#-like model with an
  explicit `@private` keyword. These models involve a lot more boilerplate,
  though, and they work particularly poorly for placeholder selectors where
  privacy may be mixed within a single style rule. Name-based privacy also
  provides a degree of compatibility with conventions libraries are already
  using.

* **Can I make a member library-private?** There's no language-level notion of a
  "library", so library-privacy isn't built in either. However, members imported
  by one module aren't automatically visible to downstream modules. If a module
  isn't [`@forward`ed](#forward) through the entrypoint to a library, it won't
  be visible to downstream consumers and thus is effectively library-private.

  As a convention, we recommend that libraries include library-private modules
  that aren't intended to be imported directly by their users in a directory
  named `src`.

## Definitions

### Member

A *member* is a Sass construct that's defined either by the user or the
implementation and is identified by a Sass identifier. This currently includes
variables, mixins, and functions (but *not* placeholder selectors). Each member
type has its own namespace, so for example the mixin `name` doesn't conflict
with the function `name` or the variable `$name`. All members have definitions
associated with them, whose specific structure depends on the type of the given
member.

### Extension

An *extension* is an object that represents a single `@extend` rule. It contains
two selectors: the *extender* is the selector for the rule that contains the
`@extend`, and the *extendee* is the selector that comes after the `@extend`.
For example:

```scss
.extender {
  @extend .extendee;
}
```

An extension may be applied to a selector to produce a new selector. This
process is outside the scope of this document, and remains unchanged from
previous versions of Sass.

### CSS Tree

A *CSS tree* is an abstract CSS syntax tree. It has multiple top-level CSS
statements like at-rules or style rules. The ordering of these statements is
significant.

A CSS tree cannot contain any Sass-specific constructs, with the notable
exception of placeholder selectors. These are allowed so that modules' CSS may
be `@extend`ed.

An *empty CSS tree* contains no statements.

### Configuration

A *configuration* is a map from variable names to SassScript values. It's used
when [executing](#executing-files) a [source file](#source-file) to customize
its execution. An *empty configuration* contains no entries.

Two configurations are considered identical if they contain the same variable
names, and if each pair of variables with the same name has values that are `==`
to one another.

### Module

A *module* is a collection of [members](#members) and [extensions](#extensions),
as well as a [CSS tree](#css-tree) (although that tree may be empty). Each
module may have only one member of a given type and name (for example, a module
may not have two variables named `$name`).

Each module is uniquely identified by the combination of a
[canonical](#canonicalizing-urls) URL and a [configuration](#configuration). A
given module can be produced by [executing](#executing-files) the [source
file](#source-file) identified by the module's URL with the module's
configuration.

### Module Graph

Modules also track their `@use` and [`@forward`](#forwarding-modules) at-rules,
which point to other modules. In this sense, modules with empty configuration
can be construed as a [directed acyclic graph][] where the vertices are modules
and the edges are `@use` rules (without `mixin` clauses) and/or `@forward`
rules. We call this the *module graph*.

[directed acyclic graph]: https://en.wikipedia.org/wiki/Directed_acyclic_graph

The module graph is not allowed to contain cycles because they make it
impossible to guarantee that all dependencies of a module are available before
that module is loaded. Although the names and APIs of a module's members can be
determined without [executing](#executing-files) it, Sass allows code to be
evaluated while loading a module, so those members may not behave correctly when
invoked before the module is executed.

### Source File

A *source file* is an entity uniquely identified by a
[canonical](#canonicalizing-urls) URL. It can be [executed](#executing-files)
with a [configuration](#configuration) to produce a [module](#module). The names
(and mixin and function signatures) of this module's members are static, and can
be determined without executing the file. This means that all modules for a
given source file have the same member names regardless of the configurations
used for those modules.

There are five types of source file:

* Sass files, SCSS files, and CSS files are identified by `file:` URLs.

* [Built-in modules](#built-in-modules) are identified by URLs beginning with
  `sass:`.

* Implementations may define implementation-specific or pluggable means of
  defining source files, which can use any URL.

Each one has different execution semantics that are beyond the scope of this
document. Note that some of these may not actually be files on the file system.

### Entrypoint

The *entrypoint* of a compilation is the [source file](#source-file) that was
initially passed to the implementation. Similarly, the *entrypoint module* is
the [module](#module) loaded from that source file with an empty configuration.
The entrypoint module is the root of the [module graph](#module-graph).

### Import Context

An *import context* is a collection of members, indexed by their types and
names. It's used to ensure that the previous global-namespace behavior is
preserved when `@import`s are used.

An import context is mutable throughout its entire lifetime, unlike a module
which doesn't change once it's been fully created. This allows it to behave as a
shared namespace for a connected group of imports.

> Note that an import context never includes members made visible by `@use`,
> even if a file with `@use` rules is imported.

## Syntax

### `@use`

The new at-rule will be called `@use`. The grammar for this rule is as follows:

<x><pre>
**UseRule**     ::= '@use' QuotedString (AsClause? MixinClause? | NoPrefix?)
**AsClause**    ::= 'as' Identifier
**NoNamespace** ::= 'no-prefix'
**MixinClause** ::= 'mixin'
</pre></x>

`@use` rules must be at the top level of the document, and must come before any
rules other than `@charset` or `@forward`. The `QuotedString`'s contents, known
as the rule's *URL*, must be a [valid URL string][] (for non-[special][special
URL scheme] base URL).

[valid URL string]: https://url.spec.whatwg.org/#valid-url-string
[special URL scheme]: https://url.spec.whatwg.org/#special-scheme

> Because each `@use` rule affects the namespace of the entire [source
> file](#source-file) that contains it, whereas most other Sass constructs are
> purely imperative, keeping it at the top of the file helps reduce confusion.

A `@use` rule's *namespace* is determined using [this
algorithm](#determining-namespaces). If the algorithm for determining a
namespace fails for a `@use` rule, that rule is invalid. If it returns `null`,
that rule is called *global*. A namespace is used to identify the used
[module](#module)'s members within the current [source file](#source-file).

The mixin clause is not allowed for global `@use` rules because the mixin name
is derived from the rule's namespace.

> I'm not at all sure about the mixin syntax here. `@use "foo" mixin` doesn't
> read very well, and sounds less sentence-like than I'd prefer. But I'm having
> trouble determining what else would be better, and still remain orthogonal to
> all the other modifiers that can be applied.

### `@forward`

This proposal introduces an additional new at-rule, called `@forward`. The
grammar for this rule is as follows:

<x><pre>
**ForwardRule** ::= '@forward' (QuotedString | Identifier)
&#32;                 (ShowClause | HideClause)?
**ShowClause**  ::= 'show' Identifier (',' Identifier)*
**HideClause**  ::= 'hide' Identifier (',' Identifier)*
</pre></x>

`@forward` rules must be at the top level of the document, and must come before
any rules other than `@charset` or `@use`. If they have a `QuotedString`, its
contents, known as the rule's *URL*, must be a [valid URL string][] (for
non-[special][special URL scheme] base URL).

### Member References

This proposal updates the syntax for referring to members. For functions and
mixins, this update affects only calls, not definitions. Variables, on the other
hand, may use this syntax for either assignment or reference.

<x><pre>
**NamespacedIdentifier** ::= (Identifier '.')? Identifier
**Variable**             ::= '$' NamespacedIdentifier
**FunctionCall**         ::= NamespacedIdentifier ArgumentInvocation
**Include**              ::= '@include' NamespacedIdentifier ArgumentInvocation?
</pre></x>

> The dot-separated syntax (`namespace.name`) was chosen in preference to a
> hyphenated syntax (for example `namespace-name`) because it makes the
> difference between module-based namespaces and manually-separated identifiers
> very clear. It also matches the conventions of many other languages. We're
> [reasonably confident][Tab comment] that the syntax will not conflict with
> future CSS syntax additions.
>
> [Tab comment]: https://github.com/sass/proposal.module-system/issues/1#issuecomment-174755061

## Procedures

The following procedures are not directly tied to the semantics of any single
construct. Instead, they're used as components of multiple constructs'
semantics. They can be thought of as re-usable functions.

### Determining Namespaces

This describes how to determine the namespace for a `@use` rule. Given a rule
`rule`:

> This algorithm is context-independent, so a namespace for a `@use` rule can be
> determined without reference to anything outside the syntax of that rule.

* If `rule` has an `as` clause, return that clause's identifier.

* If `rule` has a `NoNamespace` clause, return `null`. The rule is global.

* Let `path` be the `rule`'s URL's [path][URL path].

  [URL path]: https://url.spec.whatwg.org/#concept-url-path

* Let `basename` be the text after the final `/` in `path`, or the entire `path`
  if `path` doesn't contain `/`.

* Let `module-name` be the text before the first `.` in `path`, or the entire
  `path` if `path` doesn't contain `.`.

* If `module-name` isn't a Sass identifier, throw an error.

* Return `module-name`.

### Loading Modules

This describes the general process for loading a module. It's used as part of
various other semantics described below. To load a module with a given URL `url`
and [configuration](#configuration) `config`:

* Let `file` be [source file](#source-file) for `url`. The process for
  locating this file is out of scope of this document.

* If `file` can't be found, throw an error.

* If `file` has already been [executed](#executing-files) with the given
  configuration, return the module that execution produced.

  > This fulfills the "import once" low-level goal.

* If `file` is currently being executed with `config`, throw an error.

  > This disallows circular `@use`s, which ensures that modules can't be used
  > until they're fully initialized.

* Otherwise, let `module` be the result of [executing](#executing-files) `file`
  with `config` and a new [import context](#import-context).

* If `file` contained a `@use` rule with a `mixin` clause and a `@forward` rule
  with an identifier that's the same as the `@use` rule's namespace, *and* if
  that `@use` rule's mixin was not included during the execution of the source
  file, throw an error.

* Otherwise, return `module`.

> For simplicity, this proposal creates an import context for every module.
> Implementations are encouraged to avoid eagerly allocating resources for
> imports, though, to make use-cases only involving `@use` more efficient.

### Resolving Extensions

The module system also scopes the resolution of the `@extend` rule. This helps
satisfy locality, making selector extension more predictable than its global
behavior under `@import`.

Extension is scoped to CSS in [modules](#module) *transitively used by* or
*transitively using* the module in which the `@extend` appears. This
transitivity is necessary because CSS is not considered a [member](#member) of a
module, and can't be controlled as explicitly as members can.

> Extending all transitively-used and -using modules is indented to reflect the
> idea that the `@extend` should affect all CSS that has the same semantic
> notion of a given selector.
>
> Another way to think about it is that `@extend` affects upstream and downstream
> CSS, but not sibling CSS.
>
> ```scss
> // upstream.scss
> .bad-error {
>   font-weight: bold;
>
>   // This works because "downstream" uses "upstream".
>   @extend .error;
> }
>
> .success {
>   color: green;
> }
>
> // sibling.scss
> .no-extend-error {
>   // This doesn't work, because "sibling" doesn't use "upstream" and "upstream"
>   // doesn't use "sibling".
>   @extend .error;
> }
>
> // downstream.scss
> @use "upstream";
> @use "sibling";
>
> .error {
>   color: red;
> }
>
> .huge-success {
>   font-weight: bold;
>
>   // This works because "downstream" uses "upstream".
>   @extend .success;
> }
>
> // output.css
> .bad-error {
>   font-weight: bold;
> }
>
> .error, .bad-error {
>   font-weight: bold;
> }
> ```

We define a general process for resolving extensions for a given module
`starting-module`. This process emits CSS for that module and everything it
transitively uses.

* Let `extended` be the subgraph of the module graph containing modules that are
  transitively reachable from `starting-module`.

* For each module `domestic` in `extended`, in reverse [topological][] order:

  * Create an empty map associated with `domestic` (call it `domestic`'s
    *extended selectors*). This map will contain selectors defined for rules in
    `domestic` and its transitively reachable modules, with extensions partially
    resolved. This map is indexed by the locations of the rules for those
    selectors. We say that this is the *original location* for a selector.

  * For each module `foreign` used or forwarded by `domestic`, in reverse
    [topological][] order:

    * For each `foreign-selector` in `foreign`'s extended selectors:

      * Let `domestic-selector` be `domestic`'s extended selector with the same
        original location as `foreign-selector`, if one exists, or else a
        synthetic selector that matches no elements.

      * Let `new-selector` be a new selector that matches all elements matched
        by either `foreign-selector` or `domestic-selector`.

      * If `foreign` module was used by `domestic` (as opposed to only being
        forwarded), apply the `domestic`'s extensions to `new-selector`, and
        replace it with the result.

      * Add `new-selector` to `domestic`'s extended selectors, indexed by the
        `foreign-selector`'s original location. Replace `domestic-selector` in
        `domsetic`'s extended selectors if necessary.

  * For each style rule `rule` in `domestic`:

    * For each module transitively reachable in the module graph from `domestic`
      in reverse [topological][] order, apply that module's extensions to
      `rule`'s selector.

    * Apply `domestic`'s extensions to `rule`'s selector.

    * Add the resulting selector to `domestic`'s extended selectors, indexed by
      the rule's location.

* For each module `domestic` in `extended` graph, in reverse [topological][]
  order:

  * Emit each top-level statement in `domestic`'s [CSS tree](#css-tree), with
    any selectors replaced by the corresponding selector in `starting-module`'s
    extended selectors.

[topological]: https://en.wikipedia.org/wiki/Topological_sorting

> **Implementation note**:
>
> As written, this algorithm is O(n²) in the number of modules because resolving
> extends for each local style rule requires iterating over all its transitive
> modules. However, it's intended to be straightforwardly implementable in O(n)
> time by cumulatively tracking the extends as the algorithm proceeds.

> To promote locality, there is intentionally no way for a module to affect the
> extensions of another module that doesn't transitively use it or isn't
> transitively used by it.

### Canonicalizing URLs

[Modules](#module) and [source files](#source-file) are uniquely identified by
URLs, which means we must be able to determine the canonical form of URLs
written by users. Given a non-canonical URL `url` and a canonicalized URL `base`
representing the context in which it's being resolved:

* If the `url`'s scheme is `sass`, return it as-is.

  > [Built-in module](#built-in-modules) URLs are compared textually, and have
  > no special canonicalization logic.

* If the `base`'s scheme is `file` and `url` is relative, return `base`
  without its final path component concatenated with `url`.

  > For example, if `base` is `file:///foo/bar/baz` and `url` is `bang/qux`,
  > return `file:///foo/bar/bang/qux`.

* If `url`'s scheme is `file`, resolve , then return a copy of `url` with any
  `..` or `.` components resolved an any duplicate separators removed from the
  path component.

* Otherwise, canonicalization proceeds in an implementation-defined manner. This
  allows individual implementations to support user-defined means of resolving
  URLs.

## Semantics

### Compilation Process

First, let's look at the large-scale process that occurs when compiling a Sass
[entrypoint](#entrypoint) to CSS.

* [Load](#loading-modules) the [module](#module) with the entrypoint URL and the
  empty configuration. Note that this transitively loads any referenced modules,
  producing a [module graph](#module-graph).

* [Resolve extensions](#resolving-extensions) for the entrypoint's module. The
  resulting CSS is the compilation's output.

### Executing Files

Many of the details of executing a [source file](#source-file) are out of scope
for this specification. However, certain constructs have relevant new semantics
that are covered below. This procedure should be understood as modifying and
expanding upon the existing execution process rather than being a comprehensive
replacement.

Given a source file `file`, a [configuration](#configuration) `config`, and an
[import context](#import-context) `import`:

* Let `module` be an empty module with the configuration `config` and same URL
  as `file`.

* Let `uses` be an empty map from `@use` rules to [modules](#modules).

* When a `@use` rule `rule` without a `MixinClause` is encountered:

  * If `rule` has a namespace that's the same as another `@use` rule's namespace
    in `file`, throw an error.

  * Let `module` be the result of [loading](#loading-modules) the module with
    `rule`'s URL and the empty [configuration](#configuration).

  * Associate `rule` with `module` in `uses`.

* When a `@forward` rule is encountered,
  [forward the module](#forwarding-modules) it refers to.

* When an `@import` rule is encountered,
  [import the file](#importing-files) it refers to.
  
* When an `@extend` rule is encountered, add its extension to `module`.

* When a style rule or a plain CSS at-rule is encountered:

  * Execute the rule as normal.

  * Remove any style rules containing a placeholder selector that begins with
    `-` or `_`.

  * Add the resulting CSS to `module`'s CSS.

* When a [member](#member) definition `member` is encountered:

  * If `member`'s name *doesn't* begin with `-` or `_`, add `member` to `module`.

  * If `import` exists, add `member` to `import`.

    > This happens regardless of whether or not it begins with `-` or `_`.

* When a member use is encountered, [resolve it](#resolving-members) using
  `file`, `uses`, and `import`.

* Once all top-level statements are executed, for every global variable
  declaration `var` in `file`:

  * If `module` has a variable with the same name as `var`, do nothing.

  * Otherwise, if `var`'s name begins with `-` or `_`, do nothing.

  * Otherwise, add `var` to the current module, with a `null` value.

    > This ensures that the module exposes the same set of members regardless of
    > its execution.

* Finally, return `module`. It is now immutable.

> Note that members that begin with `-` or `_` (which Sass considers equivalent)
> are considered private. Private members are not added to the module's member
> set, but they are visible from within the module itself. This follows Python's
> and Dart's privacy models, and bears some similarity to CSS's use of leading
> hyphens to indicate experimental vendor features.
>
> For backwards-compatibility, privacy does not apply across `@import` boundaries.
> If one file imports another, either may refer to the other's private members.
>
> ```scss
> // This function is private and may only be used within this module.
> @function -parse-gutters($short) {
>   // ...
> }
>
> // By contrast, this mixin is part of the module's public API.
> @mixin gutters($span) {
>   // But it can use private members within its own module.
>   $span: -parse-gutters($span);
> }
> ```

> This proposal follows Python and diverges from Dart in that `@use` imports
> modules with a namespace by default. There are two reasons for this. First, it
> seems to be the case that language ecosystems with similar module systems
> either namespace all imports by convention, or namespace almost none. Because
> Sass is not object-oriented and doesn't have the built-in namespacing that
> classes provide many other languages, its APIs tend to be much broader at the
> top level and thus at higher risk for name conflict. Namespacing by default
> tilts the balance towards always namespacing, which mitigates this risk.
>
> Second, a default namespace scheme drastically reduces the potential for
> inconsistency in namespace choice. If the namespace is left entirely up to the
> user, different people may choose to namespace `strings.scss` as `strings`,
> `string`, `str`, or `strs`. This taxes the reusability of code and knowledge,
> and mitigating it is a benefit.

> ```scss
> // This has the default namespace "susy".
> @use "susy";
>
> // This has the explicit namespace "bbn".
> @use "bourbon" as bbn;
>
> // This has no namespace.
> @use "compass" no-prefix;
>
> // Both packages define their own "gutters()" functions. But because the members
> // are namespaced, there's no conflict and the user can use both at once.
> #susy {@include susy.gutters()}
> #bourbon {@include bbn.gutters()}
>
> // Users can also import without a namespace at all, which lets them use the
> // original member names.
> #compass {@include gutters()}
> ```

### Resolving Members

The main function of the module system is to control how [member](#member) names
are resolved across files—that is, to find the definition corresponding to a
given name. Given a source file `file`, a map `uses` from `@use` rules to the
[modules](#module) loaded by those rules, a member to resolve named `name` of
type `type`, and an [import context](#import-context) `import`:

* If `name` is a [namespaced identifier](#member-references)
  `namespace.raw-name`:

  * Let `use` be the `@use` rule in `uses` whose namespace is `namespace`. If
    there is no such rule, throw an error.

  * Let `module` be the module in `uses` associated with `use`.

  * Let `member` be the member of `module` with type `type` with name
    `raw-name`. If there is no such member, throw an error.

  * If `use` has a `mixin` clause and the [module mixin](#module-mixins) with
    the name `namespace` hasn't yet been included, or has been included more
    than once, throw an error.

  * Otherwise, return `member`.

* If `type` is "mixin" and there exists a `@use` rule in `uses` whose namespace
  is `name`, and that `@use` rule has a `mixin` clause, return its [module
  mixin](#module-mixins).

* If `file` defines a member `member` of `type` named `name`:

  * If `member`'s definition has already been evaluated, return it.

  * Otherwise, throw an error.

    > This ensures that any change in name resolution caused by reordering a
    > file causes an immediate error rather than an unexpected behavioral
    > change.

* If a member of type `type` named `name` is defined in exactly one module in
  `uses` whose `@use` rule is global, return that member.

* Otherwise, if a member of type `type` named `name` is defined in more than one
  module in `uses` whose `@use` rule is global, throw an error.

  > This ensures that, if a new version of a package produces a conflicting
  > name, it causes an immediate error.

* If `import` exists and contains a member of type `type` named `name`, return
  it.

* Otherwise, throw an error.

### Module Mixins

[Modules](#module) can be encapsulated in mixins by using `@use`'s `mixin`
clause. This allows a module's CSS to only be conditionally included in a
document, or to be included in a nested context. It also allows the user of the
module to configure it by providing default values for variables that the module
uses.

When executing a `@use` rule with a `mixin` clause, the rule's module isn't
loaded as normal. Instead a special *module mixin*, with the same name as the
rule's namespace, is made available in the current [source file](#source-file).

The module mixin's arguments are derived from the module's members (which we can
determine without executing the module). For every variable in module that has a
`!default` flag, the module mixin has an argument with the same name and a
default value of `null`. The mixin does not allow positional arguments, nor does
it allow named arguments that are not derived from variables.

> It may become useful to provide the ability to customize the behavior of
> module mixins at the language level. It may be wise to define a convention for
> reserved argument names so that we can add arguments to all module mixins in
> the future. It's not clear what a good convention would be for this, though.

When this mixin is included:

* Let `config` be a configuration whose variable names are the module mixin's
  argument names. These variable's values are the values of the corresponding
  arguments.

* [Load](#loading-modules) the module with the `@use` rule's URL and this
  configuration.

* If the current source file contains a `@forward` rule with an identifier
  that's the same as the `@use` rule's namespace, [forward](#forwarding-modules)
  the loaded module with that `@forward` rule.

* [Resolve extensions](#resolving-extensions) for the loaded module, then emit
  the resulting CSS to the location of the `@include`.

> There are several important things to note here. First, every time a module
> mixin is used, its CSS is emitted, which means that the CSS may be emitted
> multiple times. This behavior makes sense in context, and is unlikely to
> surprise anyone, but it's good to note nonetheless as an exception to the
> import-once goal.
>
> Second, because module mixins' CSS is included directly in another module's,
> `@use` rules with `mixin` clauses do not create edges on the module graph.
> Those edges represent a *reference to* another module's CSS, whereas module
> mixins *directly include* that CSS. Keeping them out of the module graph also
> allows users to dynamically choose not to include the module at all and avoid
> using its CSS at all.
>
> Finally, module mixins don't affect name resolution at all, except in that a
> name that refers to a member of the module will fail to load until the mixin
> has been included. The scoping of these names is independent of the location
> of the module mixin's `@include` rule, so even if it's included in a
> deeply-nested selector hierarchy its members will be accessible at the root of
> the document.
>
> ```scss
> // This defines a mixin named "susy" that loads the module with custom
> // configuration.
> @use "susy" mixin;
>
> // Forward all the members from susy, with our customization included.
> @forward "susy";
>
> // These variables are set in the scope of susy's main module.
> @include susy(
>   $columns: 4,
>   $gutters: 0.25,
>   $math: fluid
> );
> ```

### Forwarding Modules

The [`@forward`](#forward) rule forwards another [module](#module)'s public API
as though it were part of the current module's.

> Note that `@forward` *does not* make any APIs available to the current module;
> that is purely the domain of `@use`. However, it *does* include the forwarded
> module's CSS tree.

First, we define a general procedure for forwarding a module `module` with a
`@forward` rule `rule`:

* For every member `member` in `module`:

  * If there's a member with the same name and type defined in the current
    [source file](#source-file), do nothing.

    > Giving local definitions precedence ensures that a module continues to
    > expose the same API if a forwarded module changes to include a conflicting
    > member.

  * Otherwise, if `rule` has a `show` clause that doesn't include `member`'s
    name, do nothing.

  * Otherwise, if `rule` has a `hide` clause that does include `member`'s name,
    do nothing.

  * Otherwise, if another `@forward` rule's module has a member with the same
    name and type as `member`, throw an error.

    > Failing here ensures that, in the absence of an obvious member that takes
    > precedence, conflicts are detected as soon as possible.

  * Otherwise, add `member` to the current module's collection of members.

Note that the procedure defined above is not directly executed when encountering
a `@forward` rule. To execute a `@forward` rule `rule`:

* If `rule` has an identifier `namespace`:

  * If there's no `@use` rule in the current source file with namespace
    `namespace` *and* with a `mixin` clause, throw an error.

  * Otherwise, do nothing. The module will be forwarded when its mixin is
    included.

* Otherwise, [load](#loading-modules) the module for `rule`'s URL with the empty
  configuration and forward it.

> This forwards all members by default to reduce the churn and potential for
> errors when a new member gets added to a forwarded module. It's likely that
> most packages will already break up their definitions into many smaller
> modules which will all be forwarded, which makes the API definition explicit
> enough without requiring additional explicitness here.
>
> ```scss
> // _susy.scss would forward its component files so users would see its full
> // API with a single @use, but the definitions don't have to live in a single
> // file.
>
> @forward "susy/grids";
> @forward "susy/box-sizing";
> @forward "susy/content";
>
> // You can show or hide members that are only meant to be used within the
> // package. You could also choose not to forward this module at all and only
> // use it from internal modules.
> @forward "susy/settings" hide susy-defaults;
> ```

### Importing Files

For a substantial amount of time, `@use` will coexist with the old `@import`
rule in order to ease the burden of migration. This means that we need to define
how the two rules interact.

When executing an `@import` rule `rule` with an import context `import`:

* Let `file` be the [source file](#source-file) with the given URL. If no such
  file can be found, throw an error.

* If `file` is currently being executed with `import` as its import context,
  throw an error.

* Let `module` be the result of [executing](#executing-files) `file` with an
  empty configuration and `import` as its import context, with the following
  differences:

  * If the `@import` rule is nested within at-rules and/or style rules, that
    context is preserved when executing `file`.

  * The generated CSS for style rules or at-rules in `file` is emitted to the
    current module's CSS.

  > Note that this execution can mutate `import`.

* Add the `module`'s [extensions](#extension) to the current module.

* For each member `member` in `module`:

  * If `member` has the same type and name as a member in `import`, do nothing.

    > Note that *all* members defined in `file` or in files it imports will
    > already be in `import`. Only members brought in by `@forward` are added to
    > `import` in this step.

  * Otherwise, add `member` to `import` and to the current module.

    > This makes forwarded members available in the importing module, but does
    > not allow them to overwrite existing members with the same names and
    > types.

> When a stylesheet contains only `@import`s without any `@use`s, the `@import`s
> are intended to work exactly as they did in previous Sass versions. Any
> difference should be considered a bug in this specification.

> This definition allows files that include `@use` to be imported. Doing so
> includes those modules' CSS as well as any members they define or forward.
> This makes it possible for users to continue using `@import` even when their
> dependencies switch to `@use`, which conversely makes it safer for packages to
> switch to `@use`.
>
> It also allows files that use `@import` to be used as modules. Doing so treats
> them as though all CSS and members were included in the module itself.

## Built-In Modules

The new module system provides an opportunity to bring more locality and
organization to the set of built-in functions that comprise Sass's core library.
These functions currently reside in the same global namespace as everything
else, which makes it difficult to add new functions without risking conflict
with either user code or future CSS functions (which has [happened in
practice][issue 631]).

[issue 631]: https://github.com/sass/sass/issues/631

We'll move all current built-in functions to built-in [modules](#module), except
for those functions that are intentionally compatible with plain CSS functions.
These modules are identified by URLs that begin with "sass:". This scheme was
chosen to avoid conflicting with plausible filenames while still being
relatively concise.

The built-in functions will be organized as follows:

| Current Name             | New Name  | Module        |   | Current Name             | New Name           | Module        |
| ------------------------ | ----------| ------------- |---| ------------------------ | ------------------ | ------------- |
| `rgb`                    |           | *global*      |   | `percentage`             |                    | sass:math     |
| `rgba`                   |           | *global*      |   | `round`                  |                    | sass:math     |
| `hsl`                    |           | *global*      |   | `ceil`                   |                    | sass:math     |
| `hsla`                   |           | *global*      |   | `floor`                  |                    | sass:math     |
| `if`                     |           | *global*      |   | `abs`                    |                    | sass:math     |
|                          |           |               |   | `min`                    |                    | sass:math     |
| `red`                    |           | sass:color    |   | `max`                    |                    | sass:math     |
| `blue`                   |           | sass:color    |   | `random`                 |                    | sass:math     |
| `green`                  |           | sass:color    |   | `unit`                   |                    | sass:math     |
| `mix`                    |           | sass:color    |   | `unitless`               |                    | sass:math     |
| `hue`                    |           | sass:color    |   | `comparable`             |                    | sass:math     |
| `saturation`             |           | sass:color    |   |                          |                    |               |
| `lightness`              |           | sass:color    |   | `length`                 |                    | sass:list     |
| `adjust-hue`             |           | sass:color    |   | `nth`                    |                    | sass:list     |
| `lighten`                |           | sass:color    |   | `set-nth`                |                    | sass:list     |
| `darken`                 |           | sass:color    |   | `join`                   |                    | sass:list     |
| `saturate`               |           | sass:color    |   | `append`                 |                    | sass:list     |
| `desaturate`             |           | sass:color    |   | `zip`                    |                    | sass:list     |
| `grayscale`              |           | sass:color    |   | `index`                  |                    | sass:list     |
| `complement`             |           | sass:color    |   | `list-separator`         | `separator`        | sass:list     |
| `invert`                 |           | sass:color    |   |                          |                    |               |
| `alpha`                  |           | sass:color    |   | `feature-exists`         |                    | sass:meta     |
| `opacify`                |           | sass:color    |   | `variable-exists`        |                    | sass:meta     |
| `transparentize`         |           | sass:color    |   | `global-variable-exists` |                    | sass:meta     |
| `adjust-color`           | `adjust`  | sass:color    |   | `function-exists`        |                    | sass:meta     |
| `scale-color`            | `scale`   | sass:color    |   | `mixin-exists`           |                    | sass:meta     |
| `change-color`           | `change`  | sass:color    |   | `inspect`                |                    | sass:meta     |
| `ie-hex-str`             |           | sass:color    |   | `get-function`           |                    | sass:meta     |
|                          |           |               |   | `type-of`                |                    | sass:meta     |
| `map-get`                | `get`     | sass:map      |   | `call`                   |                    | sass:meta     |
| `map-merge`              | `merge`   | sass:map      |   | `unique-id`              |                    | sass:meta     |
| `map-remove`             | `remove`  | sass:map      |   |                          | `module-variables` | sass:meta     |
| `map-keys`               | `keys`    | sass:map      |   |                          | `module-functions` | sass:meta     |
| `map-values`             | `values`  | sass:map      |   |                          |                    |               |
| `map-has-key`            | `has-key` | sass:map      |   | `unquote`                |                    | sass:string   |
| `keywords`               |           | sass:map      |   | `quote`                  |                    | sass:string   |
|                          |           |               |   | `str-length`             | `length`           | sass:string   |
| `selector-nest`          | `nest`    | sass:selector |   | `str-insert`             | `insert`           | sass:string   |
| `selector-append`        | `append`  | sass:selector |   | `str-index`              | `index`            | sass:string   |
| `selector-replace`       | `replace` | sass:selector |   | `str-slice`              | `slice`            | sass:string   |
| `selector-unify`         | `unify`   | sass:selector |   | `to-upper-case`          |                    | sass:string   |
| `is-superselector`       |           | sass:selector |   | `to-lower-case`          |                    | sass:string   |
| `simple-selectors`       |           | sass:selector |   |                          |                    |               |
| `selector-parse`         | `parse`   | sass:selector |   |                          |                    |               |

Regardless of what configuration is used to load them, built-in modules will
contain only the functions described above. They won't contain any other
[members](#member), CSS, or extensions. New members may be added in the future,
but CSS will not be added to existing modules.

> ```scss
> @use "sass:color";
> @use "sass:map";
> @use "sass:math";
>
> // Adapted from https://css-tricks.com/snippets/sass/luminance-color-function/.
> @function luminance($color) {
>   $colors: (
>     'red': color.red($color),
>     'green': color.green($color),
>     'blue': color.blue($color)
>   );
>
>   @each $name, $value in $colors {
>     $adjusted: 0;
>     $value: $value / 255;
>
>     @if $value < 0.03928 {
>       $value: $value / 12.92;
>     } @else {
>       $value: ($value + .055) / 1.055;
>       $value: math.pow($value, 2.4);
>     }
>
>     $colors: map.merge($colors, ($name: $value));
>   }
>
>   @return map.get($colors, 'red') * .2126 +
>       map.get($colors, 'green') * .7152 +
>       map.get($colors, 'blue') * .0722;
> }
> ```

### New Functions

The module system brings with it the need for additional introspection
abilities. To that end, several new built-in functions will be defined in
the `sass:meta` module.

Because a module's member names are knowable statically, these functions may be
safely called even before a module mixin is included. Note that (like the
existing `*-defined()` functions) their behavior depends on the lexical context
in which they're invoked.

#### `module-variables()`

The `module-variables()` function takes a `$module` parameter, which must be a
string that matches the namespace of a `@use` rule in the current source file.
It returns a map from variable names defined in the module loaded by that rule
(as quoted strings, without `$`) to the current values of those variables.

#### `module-functions()`

The `module-functions()` function takes a `$module` parameter, which must be a
string that matches the namespace of a `@use` rule in the current source file.
It returns a map from function names defined in the module loaded by that rule
(as quoted strings) to function values that can be used to invoke those
functions.

### New Features For Existing Functions

Several functions will get additional features in the new module-system world.

The `global-variable-exists()`, `function-exists()`, and `mixin-exists()`
functions will all take an optional `$module` parameter. This parameter must be
a string or `null`, and it must match the namespace of a `@use` rule in the
current module. If it's not `null`, the function returns whether the module
loaded by that rule has a member with the given name and type. If it's `null`,
it looks for members defined so far in the current module or import context,
members of any modules loaded by global `@use` rules, or global built-in
definitions.
