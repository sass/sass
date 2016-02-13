# The Next-Generation Sass Module System: Draft 2

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

## Table of Contents

* [Background](#background)
* [Goals](#goals)
  * [High-Level](#high-level)
  * [Low-Level](#low-level)
  * [Non-Goals](#non-goals)
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
  * [`@forward`](#forward)
  * [Member References](#member-references)
* [Procedures](#procedures)
  * [Loading Modules](#loading-modules)
  * [Resolving Extensions](#resolving-extensions)
  * [Canonicalizing URIs](#canonicalizing-uris)
* [Semantics](#semantics)
  * [Compilation Process](#compilation-process)
  * [Executing Files](#executing-files)
  * [Resolving Members](#resolving-members)
  * [Using Modules](#using-modules)
  * [Module Mixins](#module-mixins)
  * [Forwarding Modules](#forwarding-modules)
  * [Importing Files](#importing-files)
* [Built-In Modules](#built-in-modules)

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

* **Backwards compatibility**. We want to make it as easy as possible for people
  to migrate to the new module system, and that means making it work in
  conjunction with existing stylesheets that use `@import`. Existing stylesheets
  that only use `@import` should have identical importing behavior to earlier
  versions of Sass, and stylesheets should be able to change parts to `@use`
  without changing the whole thing at once.

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

A *member* is a Sass construct that's defined either by the user or the
implementation and is identified by a Sass identifier. This currently includes
variables, mixins, and functions (but *not* placeholder selectors). Each member
type has its own namespace, so for example the variable `$name` doesn't conflict
with the mixin `name`. All members have definitions associated with them, whose
specific structure depends on the type of the given member.

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
declarations like `@`-rules or rulesets. The ordering of the roots is
significant.

A CSS tree cannot contain any Sass-specific constructs, with the notable
exception of placeholder selectors. These are allowed so that modules' CSS may
be `@extend`ed.

An empty CSS tree contains no top-level declarations.

### Configuration

A *configuration* is a set of variables with associated SassScript values. It's
used when [executing](#executing-files) a [source file](#source-file) to
customize its execution. It may be empty—that is, it may contain no variables.

Two configurations are considered identical if they contain the same variables,
and if each pair of variables with the same name has values that are `==` to one
another.

### Module

A *module* is an abstract collection of [members](#members) and
[extensions](#extensions), as well as a [CSS tree](#css-tree) (although that
tree may be empty). Each module may have only one member of a given type and
name (for example, a module may not have two variables named `$name`).

Each module is uniquely identified by the combination of a
[canonical](#canonicalizing-uris) URI and a [configuration](#configuration). A
given module can be produced by [executing](#executing-files) the
[source file](#source-file) identified by the module's URI with the module's
configuration.

### Module Graph

Modules also track their `@use` and [`@forward`](#forwarding-modules)
directives, which point to other modules. In this sense, modules with empty
configuration can be construed as a [directed acyclic graph][] where the
vertices are modules and the edges are `@use` directives (without `mixin`
clauses) and/or `@forward` directives. We call this the *module graph*.

[directed acyclic graph]: https://en.wikipedia.org/wiki/Directed_acyclic_graph

The module graph is not allowed to contain cycles because they make it
impossible to guarantee that all dependencies of a module are fully executed
before that module is loaded. Although a module's members can be determined
without [executing](#executing-files) it, Sass allows code to be executed while
loading a module, which means those members may be executed.

### Source File

A *source file* is an entity uniquely identified by a
[canonical](#canonicalizing-uris) URI. It can be [executed](#executing-files)
with a [configuration](#configuration) to produce a [module](#module). The names
(and mixin and function signatures) of this module's members are static, and can
be determined without executing the file. This means that all modules for a
given source file have the same member names regardless of the configurations
used for those modules.

There are five types of source file:

* Sass files, SCSS files, and CSS files are identified by file paths.

* [Built-in modules](#built-in modules) are identified by URIs beginning with
  "sass:".

* Implementations may define implementation-specific or pluggable means of
  defining source files, which can use any URI.

Each one has different execution semantics that are beyond the scope of this
document. Note that some of these are not or may not actually be files on the
file system.

### Entrypoint

The *entrypoint* of a compilation is the [source file](#source-file) that was
initially passed to the implementation. Similarly, the *entrypoint module* is
the [module](#module) loaded from that source file with an empty configuration.
The entrypoint module is the root of the [module graph](#module-graph).

### Import Context

An *import context* is a collection of members, indexed by their names. It's
used to ensure that the previous global-namespace behavior is preserved when
`@import`s are used.

An import context is mutable throughout its entire lifetime, unlike a module
which doesn't change once it's been fully created. This allows it to behave as a
shared namespace for a connected group of imports.

## Syntax

The new directive will be called `@use`. The grammar for this directive is as
follows:

```
UseDirective ::= '@use' QuotedString (AsClause? MixinClause? | NoPrefix?)
AsClause     ::= 'as' Identifier
NoPrefix     ::= 'no-prefix'
MixinClause  ::= 'mixin'
```

*Note: this only encompasses the syntax whose semantics are currently described
in this document. As the document becomes more complete, the grammar will be
expanded accordingly.*

`@use` directives must be at the top level of the document, and must come before
any directives other than `@charset`. Because each `@use` directive affects the
namespace of the entire [source file](#source-file) that contains it, whereas
most other Sass constructs are purely imperative, keeping it at the top of the
file helps reduce confusion.

The mixin clause is not allowed for unprefixed modules because the mixin name
is derived from the module's prefix.

> **Design note:**
>
> I'm not at all sure about the mixin syntax here. `@use "foo" mixin` doesn't
> read very well, and sounds less sentence-like than I'd prefer. But I'm having
> trouble determining what else would be better, and still remain orthogonal to
> all the other modifiers that can be applied.

### `@forward`

This proposal introduces an additional new directive, called `@forward`. The
grammar for this directive is as follows:

```
ForwardDirective ::= '@forward' QuotedString (ShowClause | HideClause)?
ShowClause       ::= 'show' Identifier (',' Identifier)*
HideClause       ::= 'hide' Identifier (',' Identifier)*
```

`@forward` directives must be at the top level of the document, and must come
before any directives other than `@charset` or `@use`.

### Member References

This proposal updates the syntax for using members. For functions and mixins,
this update affects only calls, not definitions. Variables, on the other hand,
may use this syntax for either assignment or reference.

```
NamespacedIdentifier ::= (Identifier '.')? Identifier
Variable             ::= '$' NamespacedIdentifier
```

The dot-separated syntax (`namespace.name`) was chosen in preference to a
hyphenated syntax (for example `namespace-name`) because it makes the difference
between module-based namespaces and manually-separated identifiers very clear.
It also matches the conventions of many other languages. We're
[reasonably confident][Tab comment] that the syntax will not conflict with
future CSS syntax additions.

[Tab comment]: https://github.com/sass/proposal.module-system/issues/1#issuecomment-174755061

## Procedures

The following procedures are not directly tied to the semantics of any single
construct. Instead, they're used as components of multiple constructs'
semantics. They can be thought of as re-usable functions.

### Loading Modules

This describes the general process for loading a module. It's used as part of
various other semantics described below. To load a module with a given URI,
[configuration](#configuration):

* Look up the [source file](#source-file) with the given URI. The process for
  doing this is out of scope of this document.

* If no such file can be found, loading fails.

* If the source file has already been [executed](#executing-files) with the
  given configuration, use the module that execution produced. This fulfills the
  "import once" low-level goal.

* If the source file is currently being executed with the given configuration,
  loading fails. This disallows circular `@use`s, which ensures that modules
  can't be used until they're fully initialized.

* Otherwise, execute that file with the given configuration, and take the
  resulting module.

* If the source file contained a `@use` directive with a `mixin` clause and a
  `@forward` directive with the same [canonical](#canonicalizing-uris) URI, and
  if that `@use` directive's mixin was not included during the execution of the
  source file, loading fails.

* Otherwise, use the resulting module.

> **Implementation note:**
>
> Although this specification only requires that modules be cached and reused
> when compiling a single [entrypoint](#entrypoint), modules are intentionally
> context-independent enough to store and re-use across multiple entrypoints, as
> long as no source files change. For example, if the user requests that all
> Sass files beneath `stylesheets/sass` be compiled, modules may be shared
> between those separate compilations.

### Resolving Extensions

The module system also scopes the resolution of the `@extend` directive. This
helps satisfy locality, making selector extension more predictable than it is
using `@import`s.

Extension is scoped to CSS in [module](#module)s *transitively used* by the
module in which the `@extend` appears. This transitivity is necessary because
CSS is not considered a [member](#member) of a module, and can't be controlled
as explicitly as members can. Extending all transitively-used modules means that
the `@extend` affects exactly that CSS that is guaranteed to exist by the `@use`
directives.

We define a general process for resolving extensions for a given module (call it
the *starting module*). This process emits CSS for that module and everything it
transitively uses.

* Take the subgraph of the module graph containing modules that are transitively
  reachable from the starting module. Call this the *extended graph*.

* For each module in the extended graph (call it the *domestic module*) in
  reverse [topological][] order:

  * Create an empty map for the domestic module (call it the module's *extended
    selectors*). This map will contain selectors defined for rules in this
    module and its transitively reachable modules, with extensions partially
    resolved. This map is indexed by the locations of the rules for those
    selectors. We say that this is the *original location* for a selector.

  * For each module used or forwarded by the domestic module (call it the
    *foreign module*`) in reverse [topological][] order:

    * For each of the foreign module's extended selectors (call it the *foreign
      selector*):

      * If the domestic module has an extended selector that has the same
        original location as the foreign selector, take it. Otherwise, create a
        selector that matches no elements. Call this the *domestic selector*.

      * Create a new selector that matches the union of all elements matched by
        the foreign selector selector and the domestic selector. Call this the
        *new selector*.

      * If the foreign module was used by the domestic module (as opposed to
        only being forwarded), apply the domestic module's extensions to the new
        selector, and replace it with the result.

      * Add the new selector to the domestic module's extended selectors,
        indexed by the foreign selector's original location. Replace the
        domestic selector if necessary.

  * For each CSS rule in the domestic module:

    * Apply the domestic module's extensions to the rule's selector.

    * Add the resulting selector to the domestic module's extended selectors,
      indexed by the rule's location.

* For each module in the extended graph (call it the *domestic module*) in
  reverse [topological][] order:

  * Emit each top-level CSS construct in the domestic module, with any selectors
    replaced by the corresponding selector in the starting module's extended
    selectors.

There is intentionally no way for a module to affect the extensions of another
module that doesn't transitively use it. This promotes locality, and matches the
behavior of mixins and functions in that monkey-patching is disallowed.

[topological]: https://en.wikipedia.org/wiki/Topological_sorting

### Canonicalizing URIs

[Module](#module)s and [source file](#source-file)s are uniquely identified by
URIs, which means we must be able to determine the canonical form of URIs
written by users. Given a non-canonical URI and a canonicalized base URI
representing the context in which it's being resolved:

* If the non-canonical URI's scheme is `sass`, return it as-is.
  [Built-in module](#built-in-modules) URIs are compared textually, and have no
  special canonicalization logic.

* If the base URI's scheme is `file` and the non-canonical URI is relative,
  prepend the base URI without its final path component to the non-canonical
  URI. For example, if the base URI is `file:///foo/bar/baz` and the
  non-canonical URI was `bang/qux`, it is now `file:///foo/bar/bang/qux`.

* If the non-canonical URI's scheme is `file`, resolve any `..` or `.`
  components and remove any duplicate separators in the path component, then
  return the URI with the new path component.

* Otherwise, canonicalization proceeds in an implementation-defined manner. This
  allows individual implementations to support user-defined means of resolving
  URIs.

## Semantics

### Compilation Process

First, let's look at the large-scale process that occurs when compiling a Sass
[entrypoint](#entrypoint) to CSS.

* [Load](#loading-modules) the [module](#module) with the entrypoint URI and the
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

Given a [source file](#source-file), a [configuration](#configuration), and
optionally an [import context](#import-context):

* Create an empty module with the given configuration and the current file's
  URI. Call this the *current module*.

* When a `@use` directive is encountered, [use the module](#using-modules) it
  refers to.

* When a `@forward` directive is encountered,
  [forward the module](#forwarding-modules) it refers to.

* When an `@import` directive is encountered,
  [import the file](#importing-files) it refers to.
  
* When an `@extend` directive is encountered, add its extension to the current
  module.

* When a CSS rule or a plain CSS directive is encountered:

  * Execute the rule or directive as normal.

  * Remove any rules containing a placeholder selector that begins with `-` or
    `_`.

  * Add the resulting CSS to the current module's CSS.

* When a [member](#member) definition is encountered, if its member's name
  doesn't begin with `-` or `_`, add it to the current module. In addition, if
  there's a current import context, add the member to the import context
  (regardless of whether or not the member is private).

* When a member use is encountered, [resolve it](#resolving-members) using the
  set of used modules and the current import context.

* Once all top-level statements are executed, return the current module.

Note that members that begin with `-` or `_` (which Sass considers equivalent)
are considered private. Private members are not added to the module's member
set, but they are visible from within the module itself. This follows Python's
and Dart's privacy models, and bears some similarity to CSS's use of leading
hyphens to indicate experimental vendor features.

For backwards-compatibility, privacy does not apply across `@import` boundaries.
If one file imports another, either may refer to the other's private members.

```scss
// This function is private and may only be used within this module.
@function -parse-gutters($short) {
  // ...
}

// By contrast, this mixin is part of the module's public API.
@mixin gutters($span) {
  // But it can use private members within its own module.
  $span: -parse-gutters($span);
}
```

### Resolving Members

The main function of the module system is to control how [member](#member) names
are resolved across files—that is, to find the definition corresponding to a
given name. Given a set of [module](#module)s loaded via `@use` and a member
type and name to resolve:

* If the name is a [namespaced identifier](#member-references):

  * Take the module whose prefix is the initial identifier. If there is no such
    module, resolution fails.

  * Strip the prefix and period to get the *unprefixed name*.

  * If the module doesn't have a member of the given type with the unprefixed
    name, resolution fails.

  * If the module's `@use` directive has a `mixin` clause and the
    [module mixin](#module-mixins) hasn't yet been included, or has been
    included more than once, resolution fails.

  * Otherwise, use the module's definition.

* If the type is "mixin" and the name is exactly a module's prefix, and that
  module's `@use` directive has a `mixin` clause, use its
  [module mixin](#module-mixins).

* If a member of the given type with the given name has already been defined in
  the current source file or exists in the current
  [import context](#import-context), use its definition.

* If such a member is defined later on in the file, resolution fails. This
  ensures that any change in name resolution caused by reordering a file causes
  an immediate error rather than an unexpected behavioral change.

* If such a member is defined in exactly one unprefixed module, use that
  module's definition.

* Otherwise, if such a member is defined in more than one unprefixed module,
  resolution fails. This ensures that, if a new version of a package produces a
  conflicting name, it causes an immediate error.

* Otherwise, if such a member isn't defined in any unprefixed module, resolution
  fails.

### Using Modules

When encountering a `@use` directive without a `mixin` clause, the first step is
to [load](#loading-modules) the [module](#module) with the given URI and the
empty configuration. Once that's done, the next step is to determine its
**prefix**.

Each module loaded this may have an associated prefix, which is a Sass
identifier that's used to identify the module's [member](#member)s within the
current file. No two `@use` directives in a given file may share a prefix,
although any number may have no prefix. The prefix for a given `@use`
directive's module is determined as follows:

* If the directive has an `as` clause, use that clause's identifier.

* If the directive has a `no-prefix` clause, then it has no prefix.

* If the module's URI (as written) doesn't match the regular expression
  `(.*/)?([^/]+)(\.[^/]*)?`, the `@use` directive is malformed.

* Call the text captured by the second group of the regular expression the
  *module name*.

* If the module name isn't a Sass identifier, the `@use` directive is malformed.

* If the module name followed by a hyphen is a initial substring of previous
  `@use` directive's prefix, or if another `@use` directive's prefix followed by
  a hyphen is an initial substring of the module name, the `@use` directive is
  malformed.

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

```scss
// This has the default prefix "susy".
@use "susy";

// This has the explicit prefix "bbn".
@use "bourbon" as bbn;

// This has no prefix.
@use "compass" no-prefix;

// Both packages define their own "gutters()" functions. But because the members
// are prefixed, there's no conflict and the user can use both at once.
#susy {@include susy.gutters()}
#bourbon {@include bbn.gutters()}

// Users can also import without a prefix at all, which lets them use the
// original member names.
#compass {@include gutters()}
```

### Module Mixins

[Modules](#module) can be encapsulated in mixins by using `@use`'s `mixin`
clause. This allows a module's CSS to only be conditionally included in a
document, or to be included in a nested context. It also allows the user of the
module to configure it by providing default values for variables that the module
uses.

When executing a `@use` directive with a `mixin` clause, the directive's module
isn't [loaded as normal](#using-modules). Instead a special *module mixin*, with
the same name as the directive's prefix, is introduced into the current source
file's namespace.

The module mixin's arguments are derived from the module's members (which we can
determine without executing the module). For every variable in module that has a
`!default` flag, the module mixin has an argument with the same name and a
default value of `null`. These arguments are in the order the variables are
defined, although users should be strongly encouraged to only pass them by name.

When this mixin is included:

* Create a configuration whose variable names are the module mixin's argument
  names. These variable's values are the values of the corresponding arguments.

* [Load](#loading-modules) the module with the `@use` directive's URI and this
  configuration.

* If the current source file contains a `@forward` directive with the same
  [canonical](#canonicalizing-uris) URI as the `@use` directive,
  [forward](#forwarding-modules) the loaded module with that `@forward`
  directive.

* [Resolve extensions](#resolving-extensions) for the loaded module, then emit
  the resulting CSS to the location of the `@include`.

There are several important things to note here. First, every time a module
mixin is used, its CSS is emitted, which means that the CSS may be emitted
multiple times. This behavior makes sense in context, and is unlikely to
surprise anyone, but it's good to note nonetheless as an exception to the
import-once goal.

Second, because module mixins' CSS is included directly in another module's,
`@use` directives with `mixin` clauses do not create edges on the module graph.
Those edges represent a *reference to* another module's CSS, whereas module
mixins *directly include* that CSS. Keeping them out of the module graph also
allows users to dynamically choose not to include the module at all and avoid
using its CSS at all.

Finally, module mixins don't affect name resolution at all, except in that a
name that refers to a member of the module will fail to load until the mixin has
been included. The scoping of these names is independent of the location of the
module mixin's `@include` directive, so even if it's included in a deeply-nested
selector hierarchy its members will be accessible at the root of the document.

```scss
// This defines a mixin named "susy" that loads the module with custom
// configuration.
@use "susy" mixin;

// Forward all the members from susy, with our customization included.
@forward "susy";

// These variables are set in the scope of susy's main module.
@include susy(
  $columns: 4,
  $gutters: 0.25,
  $math: fluid
);
```

### Forwarding Modules

The [`@forward`](#forward) directive forwards another [module](#module)'s public
API as though it were part of the current module's. Note that it *does not* make
that API available to the current module; that is purely the domain of `@use`.

First, we define a general procedure for forward a module (call it the
*forwarded module*) with a `@forward` directive:

* For every member (call it the *forwarded member*) in the forwarded module:

  * If there's a member with the same name and type defined later on in the
    current [source file](#source-file), do nothing. Giving local definitions
    precedence ensures that a module continues to expose the same API if a
    forwarded module changes to include a conflicting member.

  * If the `@forward` directive has a `show` clause that doesn't include the
    forwarded member's name, do nothing.

  * If the `@forward` directive has a `hide` clause that does include the
    forwarded member's name, do nothing.

  * If another `@forward` directive's module has a member with the same name and
    type, the directive is malformed. Failing here ensures that, in the absence
    of an obvious member that takes precedence, conflicts are detected as soon
    as possible.

  * Otherwise, add the member to the current module's collection of members.

Note that the procedure defined above is not directly executed when encountering
a `@forward` directive. To execute a `@forward` directive:

* If the current source file contains a `@use` directive with the same
  [canonical](#canonicalizing-uris) URI as the `@forward` directive and a
  `mixin` clause:

  * If there are multiple `@use` directives with that canonical URI, the
    `@forward` directive is malformed. This is true regardless of whether the
    additional `@use` directives have `mixin` declarations.

  * Otherwise, do nothing. The module will be forwarded when the module is
    included.

* Otherwise, [load](#loading-modules) the module for the directive's URI with
  the empty configuration.

* Forward the loaded module.

This forwards all members by default to reduce the churn and potential for
errors when a new member gets added to a forwarded module. It's likely that most
packages will already break up their definitions into many smaller modules which
will all be forwarded, which makes the API definition explicit enough without
requiring additional explicitness here.

> **Design note:**
>
> There should definitely be a way to forward members from a configured module,
> but I'm not sure whether this is the best way to do it. It weirds me out that
> an identical `@forward` declaration can mean different things based on `@use`
> directives around it. But I haven't come up with a better alternative.

```scss
// _susy.scss would forward its component files so users would see its full API
// with a single @use, but the definitions don't have to live in a single file.

@forward "susy/grids";
@forward "susy/box-sizing";
@forward "susy/content";

// You can show or hide members that are only meant to be used within the
// package. You could also choose not to forward this module at all and only
// use it from internal modules.
@forward "susy/settings" hide susy-defaults;
```

### Importing Files

For the duration of the Sass 4.x release cycle, `@use` will coexist with the old
`@import` directive in order to ease the burden of migration. This means that we
need to define how the two directives interact.

When executing an `@import` directive:

* If there is no current [import context](#import-context), create one that
  contains all of the current [module](#module)'s members that have been defined
  so far. Note that this does not include members visible because of `@use`, nor
  does it include members from [forwarded](#forwarding-modules) modules.

* Look up the [source file](#source-file) with the given URI.

* If no such file can be found, importing fails.

* If the source file is currently being executed with the current import
  context, loading fails.

* [Execute](#executing-files) that file with an empty configuration and the
  current import context. Note that this execution can mutate the current import
  context.

* Emit the resulting module's CSS to the location of the `@import`.

* Add the resulting module's extensions to the current module.

* Add any members of the resulting module that don't conflict with the current
  import context to that context, and to the current module. This makes
  forwarded members available in the importing module, but does not them to
  overwrite existing members with the same names and types.

When a stylesheet contains only `@import`s without any `@use`s, the `@import`s
are intended to work exactly as they did in previous Sass versions. Any
difference should be considered a bug in this specification.

This definition allows files that include `@use` to be imported. Doing so
includes those modules' CSS as well as any members they define or forward. This
makes it possible for users to continue using `@import` even when their
dependencies switch to `@use`, which conversely makes it safer for packages to
switch to `@use`.

It also allows files that use `@import` to be used as modules. Doing so treats
them as though all CSS and members were included in the module itself.

## Built-In Modules

The new module system provides an opportunity to bring more locality and
organization to the set of built-in functions that comprise Sass's core library.
These functions currently reside in the same global namespace as everything
else, which makes it difficult to add new functions without risking conflict
with either user code or future CSS functions (which has
[happened in practice][issue-631]).

[issue 631]: https://github.com/sass/sass/issues/631

We'll move all current built-in functions to built-in [module](#module)s, except
for those functions that are intentionally compatible with plain CSS functions.
These modules are identified by URIs that begin with "sass:". This scheme was
chosen to avoid conflicting with plausible filenames while still being
relatively concise.

The built-in functions will be organized as follows:

| Current Name             | New Name    | Module        |   | Current Name             | New Name    | Module        |
| ------------------------ | ------------| ------------- |---| ------------------------ | ------------| ------------- |
| `rgb`                    |             | *global*      |   | `percentage`             |             | sass:math     |
| `rgba`                   |             | *global*      |   | `round`                  |             | sass:math     |
| `hsl`                    |             | *global*      |   | `ceil`                   |             | sass:math     |
| `hsla`                   |             | *global*      |   | `floor`                  |             | sass:math     |
| `if`                     |             | *global*      |   | `abs`                    |             | sass:math     |
|                          |             |               |   | `min`                    |             | sass:math     |
| `red`                    |             | sass:color    |   | `max`                    |             | sass:math     |
| `blue`                   |             | sass:color    |   | `random`                 |             | sass:math     |
| `green`                  |             | sass:color    |   | `unit`                   |             | sass:math     |
| `mix`                    |             | sass:color    |   | `unitless`               |             | sass:math     |
| `hue`                    |             | sass:color    |   | `comparable`             |             | sass:math     |
| `saturation`             |             | sass:color    |   |                          |             |               |
| `lightness`              |             | sass:color    |   | `length`                 |             | sass:list     |
| `adjust-hue`             |             | sass:color    |   | `nth`                    |             | sass:list     |
| `lighten`                |             | sass:color    |   | `set-nth`                |             | sass:list     |
| `darken`                 |             | sass:color    |   | `join`                   |             | sass:list     |
| `saturate`               |             | sass:color    |   | `append`                 |             | sass:list     |
| `desaturate`             |             | sass:color    |   | `zip`                    |             | sass:list     |
| `grayscale`              |             | sass:color    |   | `index`                  |             | sass:list     |
| `complement`             |             | sass:color    |   | `list-separator`         | `separator` | sass:list     |
| `invert`                 |             | sass:color    |   |                          |             |               |
| `alpha`                  |             | sass:color    |   | `feature-exists`         |             | sass:meta     |
| `opacify`                |             | sass:color    |   | `variable-exists`        |             | sass:meta     |
| `transparentize`         |             | sass:color    |   | `global-variable-exists` |             | sass:meta     |
| `adjust-color`           | `adjust`    | sass:color    |   | `function-exists`        |             | sass:meta     |
| `scale-color`            | `scale`     | sass:color    |   | `mixin-exists`           |             | sass:meta     |
| `change-color`           | `change`    | sass:color    |   | `inspect`                |             | sass:meta     |
| `ie-hex-str`             |             | sass:color    |   | `type-of`                |             | sass:meta     |
|                          |             |               |   | `call`                   |             | sass:meta     |
| `map-get`                | `get`       | sass:map      |   | `unique-id`              |             | sass:meta     |
| `map-merge`              | `merge`     | sass:map      |   |                          |             |               |
| `map-remove`             | `remove`    | sass:map      |   | `unquote`                |             | sass:string   |
| `map-keys`               | `keys`      | sass:map      |   | `quote`                  |             | sass:string   |
| `map-values`             | `values`    | sass:map      |   | `str-length`             | `length`    | sass:string   |
| `map-has-key`            | `has-key`   | sass:map      |   | `str-insert`             | `insert`    | sass:string   |
| `keywords`               |             | sass:map      |   | `str-index`              | `index`     | sass:string   |
|                          |             |               |   | `str-slice`              | `slice`     | sass:string   |
| `selector-nest`          | `nest`      | sass:selector |   | `to-upper-case`          |             | sass:string   |
| `selector-append`        | `append`    | sass:selector |   | `to-lower-case`          |             | sass:string   |
| `selector-replace`       | `replace`   | sass:selector |   |                          |             |               |
| `selector-unify`         | `unify`     | sass:selector |   |                          |             |               |
| `is-superselector`       |             | sass:selector |   |                          |             |               |
| `simple-selectors`       |             | sass:selector |   |                          |             |               |
| `selector-parse`         | `parse`     | sass:selector |   |                          |             |               |

> **Design note:**
>
> For now, I've left in all existing functions. However, given that we'll be
> asking users to make such a big transition anyway, it may be worth considering
> whether we want to get rid of some. I'm thinking in particular of individual
> color functions that are redundant with `adjust-color()`.

Regardless of what configuration is used to load them, built-in modules will
contain only the functions described above. They won't contain any other
[member](#member)s, CSS, or extensions. New members may be added in the future,
but CSS will not be added to existing modules.

```scss
@use "sass:color";
@use "sass:map";
@use "sass:math";

// Adapted from https://css-tricks.com/snippets/sass/luminance-color-function/.
@function luminance($color) {
  $colors: (
    'red': color.red($color),
    'green': color.green($color),
    'blue': color.blue($color)
  );

  @each $name, $value in $colors {
    $adjusted: 0;
    $value: $value / 255;

    @if $value < 0.03928 {
      $value: $value / 12.92;
    } @else {
      $value: ($value + .055) / 1.055;
      $value: math.pow($value, 2.4);
    }

    $colors: map.merge($colors, ($name: $value));
  }

  @return map.get($colors, 'red') * .2126 +
      map.get($colors, 'green') * .7152 +
      map.get($colors, 'blue') * .0722;
}
```
