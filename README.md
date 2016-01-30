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

## Table of Contents

* [Background](#background)
* [Goals](#goals)
  * [High-Level](#high-level)
  * [Low-Level](#low-level)
  * [Non-Goals](#non-goals)
* [Definitions](#definitions)
  * [Member](#member)
  * [CSS Tree](#css-tree)
  * [Configuration](#configuration)
  * [Module](#module)
  * [Module Graph](#module-graph)
  * [Source File](#source-file)
  * [Entrypoint](#entrypoint)
* [Syntax](#syntax)
  * [`@forward`](#forward)
* [Semantics](#semantics)
  * [Loading Modules](#loading-modules)
  * [Compilation Process](#compilation-process)
  * [Using Modules](#using-modules)
  * [Resolving Members](#resolving-members)
  * [Resolving Extends](#resolving-extends)
  * [Forwarding Modules](#forwarding-modules)
  * [Module Mixins](#module-mixins)
  * [Private Members](#private-members)

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

A *member* is anything that's defined either by the user or the implementation 
and is identified by a Sass identifier. This currently includes variables,
mixins, functions, and placeholder selectors. Each member type has its own
namespace, so for example the variable `$name` doesn't conflict with the
placeholder selector `%name`.

All members have definitions associated with them, whose specific structure
depends on the type of the given member. Variables, mixins, and functions have
intuitive definitions, but placeholder selectors' definitions just indicate
which [module](#module) they come from.

There's some question of whether placeholders ought to be considered members,
and consequently [namespaced](#resolving-members) like other members. On one
hand, they're frequently used in parallel with mixins as the API exposed by a
library, which suggests that they should be namespaced like the mixins they
parallel. On the other hand, this usage is somewhat discouraged since it doesn't
treat them like selectors, and not namespacing them would potentially free up
characters like `.` or `:` to be used as namespace separators.

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
without executing it, Sass allows code to be executed while loading a module,
which means those members may be executed.

### Source File

A *source file* is an entity uniquely identified by a URI. It can be executed
with a [configuration](#configuration) to produce a [module](#module). The names
(and mixin and function signatures) of this module's members are static, and can
be determined without executing the file. This means that all modules for a
given source file have the same member names regardless of the configurations
used for those modules.

There are five types of source file:

* Sass files, SCSS files, and CSS files are identified by file paths.

* Core libraries are identified by special URIs in an as-yet-undecided format.

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

## Semantics

### Loading Modules

This describes the general process for loading a module. It's used as part of
various other semantics described below. To load a module with a given URI and
configuration:

* Look up the [source file](#source-file) with the given URI. The process for
  doing this is out of scope of this document.

* If no such file can be found, loading fails.

* If the source file has already been executed with the given
  [configuration](#configuration), use the module that execution produced. This
  fulfills the "import once" low-level goal.

* If the source file is currently being executed with the given
  [configuration](#configuration), loading fails. This disallows circular
  `@use`s, which ensures that modules can't be used until they're fully
  initialized.

* Otherwise, execute that file with the given configuration, and use the
  resulting module.

> **Implementation note:**
>
> Although this specification only requires that modules be cached and reused
> when compiling a single [entrypoint](#entrypoint), modules are intentionally
> context-independent enough to store and re-use across multiple entrypoints, as
> long as no source files change. For example, if the user requests that all
> Sass files beneath `stylesheets/sass` be compiled, modules may be shared
> between those separate compilations.

### Compilation Process

First, let's look at the large-scale process that occurs when compiling a Sass
[entrypoint](#entrypoint) to CSS.

* [Load](#loading-modules) the [module](#module) with the entrypoint URI and the
  empty configuration. Note that this transitively loads any referenced modules,
  producing a [module graph](#module-graph).

* [Resolve extends](#resolving-extends) for the entrypoint's module. The
  resulting CSS is the compilation's output.

[topological]: https://en.wikipedia.org/wiki/Topological_sorting

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

* If the module's URI doesn't match the regular expression
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

### Resolving Members

The main function of the module system is to control how [member](#member) names
are resolved across files—that is, to find the definition corresponding to a
given name. Given a set of [module](#module)s loaded via `@use` and a member
type and name to resolve:

* If the name begins with a module's prefix followed by a hyphen:

  * Strip the prefix and hyphen to get the *unprefixed name*.

  * If the module doesn't have a member of the given type with the unprefixed
    name, resolution fails.

  * If the module's `@use` directive has a `mixin` clause and the
    [module mixin](#module-mixins) hasn't yet been invoked, resolution fails.

  * Otherwise, use the module's definition.

* If the type is "mixin" and the name is exactly a module's prefix, and that
  module's `@use` directive has a `mixin` clause, use its
  [module mixin](#module-mixins).

* If a member of the given type with the given name has already been defined in
  the current source file, use its definition.

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

The hyphenated syntax (`namespace-name`) was chosen in preference to other
syntaxes (for example `namespace.name`, `namespace::name`, or `namespace|name`)
because it's likely to be compatible with existing code that uses manual
namespaces, and because it doesn't overlap with plain CSS syntax. This is
especially relevant for namespaced placeholder selectors, because most other
reasonable characters are already meaningful in selector contexts.

The downside to hyphens are that they look like normal identifiers, which makes
it less locally clear what's a namespace and what's a normal member name. It
also allows module prefixes to shadow other members, and introduces the
possibility of conflicting prefixes between modules.

### Resolving Extends

The module system also scopes the resolution of the `@extend` directive. This
helps satisfy locality, making selector extension more predictable than it is
using `@import`s.

Extension is scoped to CSS in [module](#module)s *transitively used* by the
module in which the `@extend` appears. This transitivity is necessary because
CSS is not considered a [member](#member) of a module, and can't be controlled
as explicitly as members can. Extending all transitively-used modules means that
the `@extend` affects exactly that CSS that is guaranteed to exist by the `@use`
directives.

We define a general process for resolving extends for a given module (call it
the *starting module*). This process emits CSS for that module and everything it
transitively uses.

* Take the subgraph of the module graph containing modules that are transitively
  reachable from the starting module. Call this the *extended graph*.

* For each module in the extended graph (call it the *domestic module*) in
  reverse [topological][] order:

  * Create an empty map for the domestic module (call it the module's *extended
    selectors*). This map will contain selectors defined for rules in this
    module and its transitively reachable modules, with extends partially
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

      * Apply any extends defined in the domestic module to the new selector,
        and replace it with the result.

      * Add the new selector to the domestic module's extended selectors,
        indexed by the foreign selector's original location. Replace the
        domestic selector if necessary.

  * For each CSS rule in the domestic module:

    * Apply any extends defined in the domestic module to the rule's selector.

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

### Forwarding Modules

The [`@forward`](#forward) directive forwards another [module](#module)'s public
API as though it were part of the current module's. Note that it *does not* make
that API available to the current module; that is purely the domain of `@use`.
To execute a `@forward` directive:

* [Load](#loading-modules) the module for the directive's URI with the empty
  configuration.

* For every member (call it the *forwarded member*) in the loaded module:

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

This forwards all members by default to reduce the churn and potential for
errors when a new member gets added to a forwarded module. It's likely that most
packages will already break up their definitions into many smaller modules which
will all be forwarded, which makes the API definition explicit enough without
requiring additional explicitness here.

### Module Mixins

[Modules](#module) can be encapsulated in mixins by using `@use`'s `mixin`
clause. This allows a module's CSS to only be conditionally included in a
document, or to be included in a nested context. It also allows the user of the
module to configure it by providing default values for variables that the module
uses.

When executing a `@use` directive with a `mixin` clause, the directive's module
isn't [loaded as normal](#using-modules). Instead a special *module mixin*, with
the same name as the directive's prefix, is introduced into the current module's
namespace.

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

* [Resolve extends](#resolving-extends) for the loaded module, then emit the
  resulting CSS to the location of the `@include`.

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

### Private Members

For the most part, when a source file is executed to produce a module, any
variables, functions, mixins, and placeholder selectors defined in the source
file become members of the corresponding module. However, an author may also
declare members private, which makes them accessible only within the module.

Privacy is determined by the naming of the member: members that begin with `-`
or `_` (which Sass considers equivalent) are private. Private members are not
added to the module's member set, but they are visible from within the module
itself. This follows Python's and Dart's privacy models, and bears some
similarity to CSS's use of leading hyphens to indicate experimental vendor
features.
