# The Next-Generation Sass Module System: Draft 6

*([Issues](https://github.com/sass/sass/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3A%22%40use%22), [Changelog](module-system.changes.md))*

This repository houses a proposal for the `@use` rule and associated module
system. This is a *living proposal*: it's intended to evolve over time, and is
hosted on GitHub to encourage community collaboration and contributions. Any
suggestions or issues can be brought up and discussed on [the issue
tracker][issues].

[issues]: https://github.com/sass/sass/issues?q=is%3Aissue+is%3Aopen+label%3A%22proposal%3A+module+system%22

Although this document describes some imperative processes when describing the
semantics of the module system, these aren't meant to prescribe a specific
implementation. Individual implementations are free to implement this feature
however they want as long as the end result is the same. However, there are
specific design decisions that were made with implementation efficiency in
mind—these will be called out explicitly in non-normative block-quoted asides.

## Table of Contents

* [Background](#background)
* [Goals](#goals)
  * [High-Level](#high-level)
  * [Low-Level](#low-level)
  * [Non-Goals](#non-goals)
* [Summary](#summary)
  * [`@use`](#use)
    * [Controlling Namespaces](#controlling-namespaces)
    * [Configuring Libraries](#configuring-libraries)
  * [`@forward`](#forward)
    * [Visibility Controls](#visibility-controls)
    * [Extra Prefixing](#extra-prefixing)
  * [`@import` Compatibility](#import-compatibility)
  * [Built-In Modules](#built-in-modules)
    * [`meta.load-css()`](#metaload-css)
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
  * [`@use`](#use-1)
  * [`@forward`](#forward-1)
  * [Member References](#member-references)
* [Procedures](#procedures)
  * [Determining Namespaces](#determining-namespaces)
  * [Loading Modules](#loading-modules)
  * [Resolving Extensions](#resolving-extensions)
  * [Resolving a `file:` URL](#resolving-a-file-url)
* [Semantics](#semantics)
  * [Compilation Process](#compilation-process)
  * [Executing Files](#executing-files)
  * [Resolving Members](#resolving-members)
  * [Forwarding Modules](#forwarding-modules)
  * [Importing Files](#importing-files)
* [Built-In Modules](#built-in-modules-1)
  * [New Functions](#new-functions)
    * [`module-variables()`](#module-variables)
    * [`module-functions()`](#module-functions)
    * [`load-css()`](#load-css)
  * [New Features For Existing Functions](#new-features-for-existing-functions)
* [Timeline](#timeline)

## Background

> This section is non-normative.

The new `@use` at-rule is intended to supercede Sass's `@import` rule as the
standard way of sharing styles across Sass files. `@import` is the simplest
possible form of re-use: it does little more than directly include the target
file in the source file. This has caused numerous problems in practice:
including the same file more than once slows down compilation and produces
redundant output; users must manually namespace everything in their libraries;
there's no encapsulation to allow them to keep implementation details hidden;
and it's very difficult for either humans or tools to tell where a given
variable, mixin, or function comes from.

The new module system is intended to address these shortcomings (among others)
and bring Sass's modularity into line with the best practices as demonstrated by
other modern languages. As such, the semantics of `@use` are heavily based on
other languages' module systems, with Python and Dart being particularly strong
influences.

## Goals

> This section is non-normative.

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
  users to access or modify. The organization of a library's implementation into
  files should be flexible enough to change without changing the user-visible
  API.

* **Configuration**. Sass is unusual among languages in that its design leads to
  the use of files whose entire purpose is to produce side effects—specifically,
  to emit CSS. There's also a broader class of libraries that may not emit CSS
  directly, but do define configuration variables that are used in computations,
  including computation of other top-level variables' values. The module system
  should allow the user to flexibly use and configure modules with side-effects.

### Low-Level

These are goals that are based less on philosophy than on practicality. For the
most part, they're derived from user feedback that we've collected about
`@import` over the years.

* **Import once**. Because `@import` is a literal textual inclusion, multiple
  `@import`s of the same Sass file within the scope of a compilation will
  compile and run that file multiple times. At best this hurts compilation time
  for little benefit, and it can also contribute to bloated CSS output when the
  styles themselves are duplicated. The new module system should only compile a
  file once.

* **Backwards compatibility**. We want to make it as easy as possible for people
  to migrate to the new module system, and that means making it work in
  conjunction with existing stylesheets that use `@import`. Existing stylesheets
  that only use `@import` should have identical importing behavior to earlier
  versions of Sass, and stylesheets should be able to change parts to `@use`
  without changing the whole thing at once.

* **Static analysis**. We want to make it possible for tools that consume Sass
  files to understand where every variable, mixin, and function reference
  points. In service of this, we want to ensure that every module has a "static
  shape"—the set of variables, mixins, and functions it exposes, as well as
  mixin and function signatures—that's entirely independent of how that module
  might be executed.

### Non-Goals

These are potential goals that we have explicitly decided to avoid pursuing as
part of this proposal for various reasons. Some of them may be on the table for
future work, but we don't consider them to be blocking the module system.

* **Dynamic imports**. Allowing the path to a module to be defined dynamically,
  whether by including variables or including it in a conditional block, moves
  away from being declarative. In addition to making stylesheets harder to read,
  this makes any sort of static analysis more difficult (and actually impossible
  in the general case). It also limits the possibility of future implementation
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

  However, it's not feasible in practice. Modules that generate CSS almost
  always do so based on some configuration, which may be changed by different
  entrypoints rendering caching useless. What's more, multiple modules may
  depend on the same shared module, and one may modify its configuration before
  the other uses it. Forbidding this case in general would effectively amount to
  forbidding modules from generating CSS based on variables.

  Fortunately, implementations have a lot of leeway to cache information that
  the can statically determine to be context-independent, including source trees
  and potentially even constant-folded variable values and CSS trees. Full
  context independence isn't likely to provide much value in addition to that.

* **Increased strictness**. Large teams with many people often want stricter
  rules around how Sass stylesheets are written, to enforce best practices and
  quickly catch mistakes. It's tempting to use a new module system as a lever to
  push strictness further; for example, we could make it harder to have partials
  directly generate CSS, or we could decline to move functions we'd prefer
  people avoid to the new built-in modules.

  As tempting as it is, though, we want to make all existing use-cases as easy
  as possible in the new system, *even if we think they should be avoided*. This
  module system is already a major departure from the existing behavior, and
  will require a substantial amount of work from Sass users to support. We want
  to make this transition as easy as possible, and part of that is avoiding
  adding any unnecessary hoops users have to jump through to get their existing
  stylesheets working in the new module system.

  Once `@use` is thoroughly adopted in the ecosystem, we can start thinking
  about increased strictness in the form of lints or TypeScript-style
  `--strict-*` flags.

* **Code splitting**. The ability to split monolithic CSS into separate chunks
  that can be served lazily is important for maintaining quick load times for
  very large applications. However, it's orthogonal to the problems that this
  module system is trying to solve. This system is primarily concerned with
  scoping Sass APIs (mixins, functions, and placeholders) rather than declaring
  dependencies between chunks of generated CSS.

  We believe that this module system can work in concert with external
  code-splitting systems. For example, the module system can be used to load
  libraries that are used to style individual components, each of which is
  compiled to its own CSS file. These CSS files could then declare dependencies
  on one another using special comments or custom at-rules and be stitched
  together by a code-splitting post-processor.

## Summary

> This section is non-normative.

This proposal adds two at-rules, `@use` and `@forward`, which may only appear at
the top level of stylesheets before any rules (other than `@charset`). Together,
they're intended to completely replace `@import`, which will eventually be
deprecated and even more eventually removed from the language.

### `@use`

`@use` makes CSS, variables, mixins, and functions from another stylesheet
accessible in the current stylesheet. By default, variables, mixins, and
functions are available in a namespace based on the basename of the URL.

```scss
@use "bootstrap";

.element {
  @include bootstrap.float-left;
  border: 1px solid bootstrap.theme-color("dark");
  margin-bottom: bootstrap.$spacer;
}
```

In addition to namespacing, there are a few important differences between `@use`
and `@import`:

* `@use` only executes a stylesheet and includes its CSS once, no matter how
  many times that stylesheet is used.
* `@use` only makes names available in the current stylesheet, as opposed to
  globally.
* Members whose names begin with `-` or `_` are private to the current
  stylesheet with `@use`.
* If a stylesheet includes `@extend`, that extension is only applied to
  stylesheets it imports, not stylesheets that import it.

Note that placeholder selectors are *not* namespaced, but they *do* respect
privacy.

#### Controlling Namespaces

Although a `@use` rule's default namespace is determined by the basename of its
URL, it can also be set explicitly using `as`.

```scss
@use "bootstrap" as b;

.element {
  @include b.float-left;
}
```

The special construct `as *` can also be used to include everything in the
top-level namespace. Note that if multiple modules expose members with the same
name and are used with `as *`, Sass will produce an error.

```scss
@use "bootstrap" as *;

.element {
  @include float-left;
}
```

#### Configuring Libraries

With `@import`, libraries are often configured by setting global variables that
override `!default` variables defined by those libraries. Because variables are
no longer global with `@use`, it supports a more explicit way of configuring
libraries: the `with` clause.

```scss
// bootstrap.scss
$paragraph-margin-bottom: 1rem !default;

p {
  margin-top: 0;
  margin-bottom: $paragraph-margin-bottom;
}
```

```scss
@use "bootstrap" with (
  $paragraph-margin-bottom: 1.2rem
);
```

This sets bootstrap's `$paragraph-margin-bottom` variable to `1.2rem` before
evaluating it. The `with` clause only allows variables defined in (or forwarded
by) the module being imported, and only if they're defined with `!default`, so
users are protected against typos.

### `@forward`

The `@forward` rule includes another module's variables, mixins, and functions
as part of the API exposed by the current module, without making them visible to
code within the current module. It allows library authors to be able to split up
their library among many different source files without sacrificing locality
within those files. Unlike `@use`, forward doesn't add any namespaces to names.

```scss
// bootstrap.scss
@forward "functions";
@forward "variables";
@forward "mixins";
```

#### Visibility Controls

A `@forward` rule can choose to show only specific names:

```scss
@forward "functions" show color-yiq;
```

It can also hide names that are intended to be library-private:

```scss
@forward "functions" hide assert-ascending;
```

#### Extra Prefixing

If you forward a child module through an all-in-one module, you may want to add
some manual namespacing to that module. You can do what with the `as` clause,
which adds a prefix to every member name that's forwarded:

```scss
// material/_index.scss
@forward "theme" as theme-*;
```

This way users can use the all-in-one module with well-scoped names for theme
variables:

```scss
@use "material" with ($theme-primary: blue);
```

or they can use the child module with simpler names:

```scss
@use "material/theme" with ($primary: blue);
```

### `@import` Compatibility

The Sass ecosystem won't switch to `@use` overnight, so in the meantime it needs
to interoperate well with `@import`. This is supported in both directions:

* When a file that contains `@import`s is `@use`d, everything in its global
  namespace is treated as a single module. This module's members are then
  referred to using its namespace as normal.

* When a file that contains `@use`s is `@import`ed, everything in its public API
  is added to the importing stylesheet's global scope. This allows a library to
  control what specific names it exports, even for users who `@import` it rather
  than `@use` it.

In order to allow libraries to maintain their existing `@import`-oriented API,
with explicit namespacing where necessary, this proposal also adds support for
files that are only visible to `@import`, not to `@use`. They're written
`"file.import.scss"`, and imported when the user writes `@import "file"`.

### Built-In Modules

The new module system will also add seven built-in modules: `math`, `color`,
`string`, `list`, `map`, `selector`, and `meta`. These will hold all the
existing built-in Sass functions. Because these modules will (typically) be
imported with a namespace, it will be much easier to use Sass functions without
running into conflicts with plain CSS functions.

This in turn will make it much safer for Sass to add new functions. We expect to
add a number of convenience functions to these modules in the future.

#### `meta.load-css()`

This proposal also adds a new built-in mixin, `meta.load-css($url, $with: ())`.
This mixin dynamically loads the module with the given URL and includes its CSS
(although its functions, variables, and mixins are not made available). This is
a replacement for nested imports, and it helps address some use-cases of dynamic
imports without many of the problems that would arise if new members could be
loaded dynamically.

## Frequently Asked Questions

> This section is non-normative.

* **Why this privacy model?** We considered a number of models for declaring
  members to be private, including a JS-like model where only members that were
  explicitly exported from a module were visible and a C#-like model with an
  explicit `@private` keyword. These models involve a lot more boilerplate,
  though, and they work particularly poorly for placeholder selectors where
  privacy may be mixed within a single style rule. Name-based privacy also
  provides a degree of compatibility with conventions libraries are already
  using.

* **Can I make a member library-private?** There's no language-level notion of a
  "library", so library-privacy isn't built in either. However, members used by
  one module aren't automatically visible to downstream modules. If a module
  isn't [`@forward`ed](#forwarding-modules) through a library's main stylesheet,
  it won't be visible to downstream consumers and thus is effectively
  library-private.

  As a convention, we recommend that libraries write library-private stylesheets
  that aren't intended to be used directly by their users in a directory named
  `src`.

* **How do I make my library configurable?** If you have a large library made up
  of many source files that all share some core `!default`-based configuration,
  we recommend that you define that configuration in a file that gets forwarded
  from your library's entrypoint and used by your library's files. For example:

  ```scss
  // bootstrap.scss
  @forward "variables";
  @use "reboot";
  ```

  ```scss
  // _variables.scss
  $paragraph-margin-bottom: 1rem !default;
  ```

  ```scss
  // _reboot.scss
  @use "variables" as *;

  p {
    margin-top: 0;
    margin-bottom: $paragraph-margin-bottom;
  }
  ```

  ```scss
  // User's stylesheet
  @use "bootstrap" with (
    $paragraph-margin-bottom: 1.2rem
  );
  ```

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

### Module

A *module* is a collection of [members](#member) and [extensions](#extension),
as well as a [CSS tree](#css-tree) (although that tree may be empty).
User-defined modules have an associated [source file](#source-file) as well.
Each module may have only one member of a given type and name (for example, a
module may not have two variables named `$name`).

A given module can be produced by [executing](#executing-files) the [source
file](#source-file) identified by the module's canonical URL with a
[configuration](#configuration).

### Module Graph

Modules also track their `@use` and `@forward` at-rules, which point to other
modules. In this sense, modules can be construed as a [directed acyclic graph][]
where the vertices are modules and the edges are `@use` rules and/or `@forward`
rules. We call this the *module graph*.

[directed acyclic graph]: https://en.wikipedia.org/wiki/Directed_acyclic_graph

The module graph is not allowed to contain cycles because they make it
impossible to guarantee that all dependencies of a module are available before
that module is loaded. Although the names and APIs of a module's members can be
determined without [executing](#executing-files) it, Sass allows code to be
evaluated while loading a module, so those members may not behave correctly when
invoked before the module is executed.

### Source File

A *source file* is a Sass abstract syntax tree along with its canonical URL.
Each canonical URL is associated with zero or one source files.

A source file can be [executed](#executing-files) with a
[configuration](#configuration) to produce a [module](#module).

> The names (and mixin and function signatures) of this module's members are
> static, and can be determined without executing the file. This means that all
> modules for a given source file have the same member names regardless of the
> context in which those modules are loaded.

> Note that [built-in modules](#built-in-modules) *do not* have source files
> associated with them.

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
whose CSS and function/mixin definitions don't change once it's been fully
created. This allows it to behave as a shared namespace for a connected group of
imports.

> Note that an import context never includes members made visible by `@use`,
> even if a file with `@use` rules is imported.

## Syntax

### `@use`

The new at-rule will be called `@use`. The grammar for this rule is as follows:

<x><pre>
**UseRule**         ::= '@use' QuotedString AsClause? WithClause?
**AsClause**        ::= 'as' ('\*' | Identifier)
**WithClause**      ::= 'with' '('
&#32;                     KeywordArgument (',' KeywordArgument)\* ','?
&#32;                   ')'
**KeywordArgument** ::= '$' Identifier ':' Expression
</pre></x>

`@use` rules must be at the top level of the document, and must come before any
rules other than `@charset` or `@forward`. The `QuotedString`'s contents, known
as the rule's *URL*, must be a [valid URL string][] (for non-[special][special
URL scheme] base URL). No whitespace is allowed after `$` in `KeywordArgument`.

[valid URL string]: https://url.spec.whatwg.org/#valid-url-string
[special URL scheme]: https://url.spec.whatwg.org/#special-scheme

> Because each `@use` rule affects the namespace of the entire [source
> file](#source-file) that contains it, whereas most other Sass constructs are
> purely imperative, keeping it at the top of the file helps reduce confusion.
>
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
algorithm](#determining-namespaces). If the algorithm for determining a
namespace fails for a `@use` rule, that rule is invalid. If it returns `null`,
that rule is called *global*. A namespace is used to identify the used
[module](#module)'s members within the current [source file](#source-file).

### `@forward`

This proposal introduces an additional new at-rule, called `@forward`. The
grammar for this rule is as follows:

<x><pre>
**ForwardRule** ::= '@forward' QuotedString AsClause? (ShowClause | HideClause)?
**AsClause**    ::= 'as' Identifier '*'
**ShowClause**  ::= 'show' MemberName (',' MemberName)*
**HideClause**  ::= 'hide' MemberName (',' MemberName)*
**MemberName**  ::= '$'? Identifier
</pre></x>

`@forward` rules must be at the top level of the document, and must come before
any rules other than `@charset` or `@use`. If they have a `QuotedString`, its
contents, known as the rule's *URL*, must be a [valid URL string][] (for
non-[special][special URL scheme] base URL). No whitespace is allowed after `$`
in `MemberName`, or before `*` in `AsClause`.

### Member References

This proposal updates the syntax for referring to members. For functions and
mixins, this update affects only calls, not definitions. Variables, on the other
hand, may use this syntax for either assignment or reference.

<x><pre>
**PublicIdentifier**     ::= [\<ident-token>][] that doesn't begin with '-' or '_'
**Variable**             ::= '$' Identifier | Identifier '.$' PublicIdentifier
**NamespacedIdentifier** ::= Identifier | Identifier '.' PublicIdentifier
**FunctionCall**         ::= NamespacedIdentifier ArgumentInvocation
**Include**              ::= '@include' NamespacedIdentifier ArgumentInvocation?
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram

No whitespace is allowed before or after the `'.'` in `NamespacedIdentifier`,
before or after the `'.$'` in `VariableIdentifier`, after the `$` in
`VariableIdentifier`, or between the `NamespacedIdentifier` and the
`ArgumentInvocation` in `FunctionCall` or `Include`.

> The dot-separated syntax (`namespace.name`) was chosen in preference to a
> hyphenated syntax (for example `namespace-name`) because it makes the
> difference between module-based namespaces and manually-separated identifiers
> very clear. It also matches the conventions of many other languages. We're
> [reasonably confident][Tab comment] that the syntax will not conflict with
> future CSS syntax additions.
>
> [Tab comment]: https://github.com/sass/sass/issues/2618#issuecomment-174755061

## Procedures

The following procedures are not directly tied to the semantics of any single
construct. Instead, they're used as components of multiple constructs'
semantics. They can be thought of as re-usable functions.

### Determining Namespaces

This algorithm takes a `@use` rule `rule`, and returns either a string or an
identifier.

> This algorithm is context-independent, so a namespace for a `@use` rule can be
> determined without reference to anything outside the syntax of that rule.

* If `rule` has an `'as'` clause `as`:

  * If `as` has an identifier, return it.

  * Otherwise, return `null`. The rule is global.

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

* If `url`'s scheme is `sass`:

  * If `config` is not empty, throw an error.

  * If a [built-in module](#built-in-modules) exists with the exact given URL,
    return it.

  * Otherwise, throw an error.

* Let `file` be the [source file](#source-file) result of [loading][loading an
  import] `url`.

  [loading an import]: https://github.com/sass/sass/blob/2c08156248496c75678826420aafb09e2ace9041/spec/at-rules/import.md#loading-an-import

* If `file` is null, throw an error.

* If `file` has already been [executed](#executing-files):

  * If `config` is not empty, throw an error.

  * Otherwise, return the module that execution produced.

  > This fulfills the "import once" low-level goal.

* If `file` is currently being executed, throw an error.

  > This disallows circular `@use`s, which ensures that modules can't be used
  > until they're fully initialized.

* Otherwise, return the result of [executing](#executing-files) `file` with
  `config` and a new [import context](#import-context).

> For simplicity, this proposal creates an import context for every module.
> Implementations are encouraged to avoid eagerly allocating resources for
> imports, though, to make use-cases only involving `@use` more efficient.

### Resolving Extensions

The module system also scopes the resolution of the `@extend` rule. This helps
satisfy locality, making selector extension more predictable than its global
behavior under `@import`.

Extension is scoped to CSS in [modules](#module) *transitively used or forwarded
by* the module in which the `@extend` appears. This transitivity is necessary
because CSS is not considered a [member](#member) of a module, and can't be
controlled as explicitly as members can.

> We considered having extension also affect modules that were *downstream* of
> the `@extend`, on the theory that they had a similar semantic notion of the
> selector in question. However, because this didn't affect other modules
> imported by the downstream stylesheet, it created a problem for the downstream
> author. It should generally be safe to take a bunch of style rules from one
> module and split them into multiple modules that are all imported by that
> module, but doing so could cause those styles to stop being affected by
> upstream extensions.
>
> Extending downstream stylesheets also meant that the semantics of a downstream
> author's styles are affected by the specific extensions used in an upstream
> stylesheet. For example,
>
> ```scss
> .foo { /* ... */ }
> .bar { @extend .foo }
> ```
>
> isn't identical (from a downstream user's perspective) to
> 
> ```scss
> .foo, .bar { /* ... */ }
> ```
>
> That could be a drawback or a benefit, but it's more likely that upstream
> authors think of themselves as distributing a chunk of styles rather than an
> API consisting of things they've extended.

We define a general process for resolving extensions for a given module
`starting-module`. This process returns a [CSS tree](#css-tree) that includes
CSS for *all* modules transitively used or forwarded by `starting-module`.

* Let `new-selectors` be an empty map from style rules to selectors. For the
  purposes of this map, style rules are compared using *reference equality*,
  meaning that style rules at different points in the CSS tree are always
  considered different even if their contents are the same.

* Let `new-extensions` be an empty map from modules to sets of extensions.

* Let `extended` be the subgraph of the [module graph](#module-graph) containing
  modules that are transitively reachable from `starting-module`.

* For each module `domestic` in `extended`, in reverse [topological][] order:

  * Let `downstream` be the set of modules that use or forward `domestic`.

    > We considered having extension *not* affect forwarded modules that weren't
    > also used. This would have matched the visibility of module members, but
    > it would also be the only place where `@forward` and `@use` behave
    > differently with regards to CSS, which creates confusion and
    > implementation complexity. There's also no clear use case for it, so we
    > went with the simpler route of making forwarded CSS visible to `@extend`.

  * For each style rule `rule` in `domestic`'s CSS:

    * Let `selector` be the result of applying `domestic`'s extensions to
      `rule`'s selector.

    * Let `selector-lists` be an empty set of selector lists.

    * For each module `foreign` in `downstream`:

      * Let `extended-selector` be the result of applying
        `new-extensions[foreign]` to `selector`.

        > `new-extensions[foreign]` is guaranteed to be populated at this point
        > because `extended` is traversed in reverse topological order, which
        > means that `foreign`'s own extensions will already have been resolved
        > by the time we start working on modules upstream of it.

      * Add `selector` to `selector-lists`.

    * Set `new-selectors[rule]` to a selector that matches the union of all
      elements matched by selectors in `selector-lists`. This selector must obey
      [the specificity laws of extend][] relative to the selectors from which it
      was generated. For the purposes of the first law of extend, "the original
      extendee" is considered only to refer to selectors that appear in
      `domestic`'s CSS, *not* selectors that were added by other modules'
      extensions.

      > Implementations are expected to trim redundant selectors from
      > `selector-lists` as much as possible. For the purposes of the first law
      > of extend, "the original extendee" is *only* the selectors in `rule`'s
      > selector. The new complex selectors in `selector` generated from
      > `domestic`'s extensions don't count as "original", and may be optimized
      > away.

    * For every extension `extension` whose extender appears in `rule`'s
      selector:

      * For every complex selector `complex` in `new-selectors[rule]`:

        * Add a copy of `extension` with its extender replaced by `complex` to
          `new-extensions[domestic]`.

  [the first law of extend]: ../spec/at-rules/extend.md#specificity
  [the specificity laws of extend]: ../spec/at-rules/extend.md#specificity

* Let `css` be an empty CSS tree.

* Define a recursive procedure, "traversing", which takes a module `domestic`:

  * If `domestic` has already been traversed, do nothing.

  * Otherwise, traverse every module `@use`d or `@forward`ed by `domestic`, in
    the order their `@use` or `@forward` rules appear in `domestic`'s source.

    > Because this traverses modules depth-first, it emits CSS in reverse
    > topological order.
    
  * Let `initial-imports` be the longest initial subsequence of top-level
    statements in `domestic`'s CSS that contains only comments and `@import`
    rules *and* that ends with an `@import` rule.

  * Insert a copy of `initial-imports` in `css` after the last `@import` rule, or
    at the beginning of `css` if it doesn't contain any `@import` rules.

  * For each top-level statement `statement` in `domestic`'s CSS tree after
    `initial-imports`:

    * If `statement` is an `@import` rule, insert a copy of `statement` in `css`
      after the last `@import` rule, or at the beginning of `css` if it doesn't
      contain any `@import` rules.

    * Otherwise, add a copy of `statement` to the end of `css`, with any style
      rules' selectors replaced with the corresponding selectors in
      `new-selectors`.

* Return `css`.

[queue]: https://en.wikipedia.org/wiki/Queue_(abstract_data_type)
[topological]: https://en.wikipedia.org/wiki/Topological_sorting

### Resolving a `file:` URL

This algorithm is intended to replace [the existing algorithm][] for resolving a
`file:` URL to add support for `@import`-only files, and to allow imports that
include a literal `.css` extension. This algorithm takes a URL, `url`, whose
scheme must be `file` and returns either another URL that's guaranteed to point
to a file on disk or null.

[the existing algorithm]: https://github.com/sass/sass/blob/2c08156248496c75678826420aafb09e2ace9041/spec/at-rules/import.md#resolving-a-file-url

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

* Otherwise, let `sass` be the result of [resolving `url` + `".sass"` for
  partials][resolving for partials].

* Let `scss` be the result of [resolving `url` + `".scss"` for
  partials][resolving for partials].

* If neither `sass` nor `scss` are null, throw an error.

* Otherwise, if exactly one of `sass` and `scss` is null, return the other
  one.

* Otherwise, return the result of [resolving `url` + `".css"` for
  partials][resolving for partials]. .

[resolving for partials]: https://github.com/sass/sass/blob/2c08156248496c75678826420aafb09e2ace9041/spec/at-rules/import.md#resolving-a-file-url-for-partials

> This allows a library to define two parallel entrypoints, one
> (`_file.import.scss`) that's visible to `@import` and one (`_file.scss`)
> that's visible to `@use`. This will allow it to maintain
> backwards-compatibility even as it switches to supporting a `@use`-based API.
>
> The major design question here is whether the file for `@use` or `@import`
> should be the special case. The main benefit to `_file.use.scss` would be that
> users don't need to use a version of Sass that supports `@use` to get the
> import-only stylesheet, but in practice it's likely that most library authors
> will want to use `@use` or other new Sass features internally anyway.
>
> On the other hand, there are several benefits to `_file.import.scss`:
>
> * It makes the recommended entrypoint is the more obvious one.
>
> * It inherently limits the lifetime of language support for the extra
>   entrypoint: once imports are removed from the language, import-only files
>   will naturally die as well.
>

> When resolving for `@use`, this algorithm treats a `.css` file is treated with
> the same priority as a `.scss` and `.sass` file.
>
> The only reason a `.css` file was ever treated as secondary was that CSS
> imports were added later on, and backwards-compatibility needed to be
> maintained for `@import`. `@use` allows us to make CSS more consistent with
> the other extensions, at a very low risk of migration friction.

## Semantics

### Compilation Process

First, let's look at the large-scale process that occurs when compiling a Sass
[entrypoint](#entrypoint) with the canonical URL `url` to CSS.

* Let `module` be the result of [loading](#loading-modules) `url` with the empty
  configuration.

  > Note that this transitively loads any referenced modules, producing a
  > [module graph](#module-graph).

* Let `css` be the result of [resolving extensions](#resolving-extensions) for
  `module`.

* Convert `css` to a CSS string. This is the result of the compilation.

### Executing Files

Many of the details of executing a [source file](#source-file) are out of scope
for this specification. However, certain constructs have relevant new semantics
that are covered below. This procedure should be understood as modifying and
expanding upon the existing execution process rather than being a comprehensive
replacement.

Given a source file `file`, a [configuration](#configuration) `config`, and an
[import context](#import-context) `import`:

* If this file isn't being executed for a `@forward` rule:

  * For every variable name `name` in `config`:

    * If neither `file` nor any source file for a module transitively forwarded
      or imported by `file` contains a variable declaration named `name` with a
      `!default` flag at the root of the stylesheet, throw an error.

      > Although forwarded modules are not fully loaded at this point, it's
      > still possible to statically determine where those modules are located
      > and whether they contain variables with default declarations.
      >
      > Implementations may choose to verify this lazily, after `file` has been
      > executed.

* Let `module` be an empty module with the same URL as `file`.

* Let `uses` be an empty map from `@use` rules to [modules](#module).

* When a `@use` rule `rule` is encountered:

  * If `rule` has a namespace that's the same as another `@use` rule's namespace
    in `file`, throw an error.

  * Let `rule-config` be the empty configuration.

  * If `rule` has a `WithClause`:

    * For each `KeywordArgument` `argument` in this clause:

      * Let `value` be the result of evaluating `argument`'s expression.

        > If the expression refers to a module that's used below `rule`, that's
        > an error.

      * Add a variable to `rule-config` with the same name as `argument`'s identifier
        and with `value` as its value.

  * Let `module` be the result of [loading](#loading-modules) the module with
    `rule`'s URL and `rule-config`.

  * Associate `rule` with `module` in `uses`.

* When a `@forward` rule `rule` is encountered:

  * If `rule` has an `AsClause` with identifier `prefix`:

    * Let `rule-config` be an empty configuration.

    * For each variable `variable` in `config`:

      * If `variable`'s name begins with `prefix`:

        * Let `suffix` be the portion of `variable`'s name after `prefix`.

        * Add a variable to `rule-config` with the name `suffix` and with the
          same value as `variable`.

  * Otherwise, let `rule-config` be `config`.

  * Let `forwarded` be the result of [loading](#loading-modules) the module with
    `rule`'s URL and `rule-config`.

  * [Forward `forwarded`](#forwarding-modules) with `file` through `module`.

* When an `@import` rule `rule` is encountered:

  * Let `file` be the result of [loading][loading an import] `rule`'s URL.

  * If `file` is `null`, throw an error.

  * [Import `file`](#importing-files) into `import` and `module`.
  
* When an `@extend` rule is encountered, add its extension to `module`.

  > Note that this adds the extension to the module being evaluated, not the
  > module in which the `@extend` lexically appears. This means that `@extend`s
  > are effectively dynamically scoped, not lexically scoped. This design allows
  > extensions generated by mixins to affect rules also generated by mixins.

* When a style rule or a plain CSS at-rule is encountered:

  * Let `css` be the result of executing the rule as normal.

  * Remove any [complex selectors][] containing a placeholder selector that
    begins with `-` or `_` from `css`.
    
  * Remove any style rules that now have no selector from `css`.

  * Append `css` to `module`'s CSS.

  [complex selectors]: https://drafts.csswg.org/selectors-4/#complex

* When a variable declaration `declaration` is encountered:

  > This algorithm is intended to replace [the existing algorithm][old
  > assigning-to-a-variable] for assigning to a variable.

  [old assigning-to-a-variable]: ../spec/variables.md#executing-a-variable-declaration

  * Let `name` be `declaration`'s [`Variable`](#member-references)'s name.

  * If `name` is a [namespaced identifier](#member-references) *and*
    `declaration` has a `!global` flag, throw an error.

  * Otherwise, if `declaration` is outside of any block of statements, *or*
    `declaration` has a `!global` flag, *or* `name` is a namespaced identifier:

    * Let `resolved` be the result of [resolving a variable named
      `name`](#resolving-members) using `file`, `uses`, and `import`.

    * If `declaration` has a `!default` flag, `resolved` isn't null, *and*
     `resolved`'s value isn't `null`, do nothing.

    * Otherwise, if `resolved` is a variable in another module:

      * Evaluate `declaration`'s value and set `resolved`'s value to the result.

    * Otherwise:

      * If `declaration` is outside of any block of statements, it has a
        `!default` flag, *and* `config` contains a variable named `name` whose
        value is not `null`:

        * Let `value` be the value of `config`'s variable named `name`.

      * Otherwise, let `value` be the result of evaluating `declaration`'s
        value.

      * If `name` *doesn't* begin with `-` or `_`, add a variable with name
        `name` and value `value` to `module`.

        > This overrides the previous definition, if one exists.

      * Add a variable with name `name` and value `value` to `import`.

        > This also overrides the previous definition.

  * Otherwise, if `declaration` is within one or more blocks associated with
    `@if`, `@each`, `@for`, and/or `@while` rules *and no other blocks*:

    * Let `resolved` be the result of [resolving a variable named
      `name`](#resolving-members) using `file`, `uses`, and `import`.

    * If `resolved` is not `null`:

      * If `declaration` has a `!default` flag and `resolved`'s value isn't
        `null`, do nothing.

      * Otherwise, let `value` be the result of evaluating `declaration`'s
        value.

      * If `name` *doesn't* begin with `-` or `_`, add a variable with name
        `name` and value `value` to `module`.

        > This overrides the previous definition, if one exists.

      * Add a variable with name `name` and value `value` to `import`.

        > This also overrides the previous definition.

    > This makes it possible to write
    >
    > ```scss
    > $variable: value1;
    > @if $condition {
    >   $variable: value2;
    > }
    > ```
    >
    > without needing to use `!global`.

  * Otherwise, if no block containing `declaration` has a [scope][] with a
    variable named `name`, set the innermost block's scope's variable `name` to
    `value`.

  * Otherwise, let `scope` be the scope of the innermost block such that `scope`
    already has a variable named `name`. Set `scope`'s variable `name` to `value`.

  [scope]: ../spec/variables.md#scope

* When a top-level mixin or function declaration `declaration` is encountered:

  > Mixins and functions defined within rules are never part of a module's API.

  * If `declaration`'s name *doesn't* begin with `-` or `_`, add `declaration` to
    `module`.

    > This overrides the previous definition, if one exists.

  * Add `declaration` to `import`.

    > This happens regardless of whether or not it begins with `-` or `_`.

* When a member use `member` is encountered:

  * Let `scope` be the [scope][] of the innermost block containing `member` such
    that `scope` has a member of `member`'s name and type, or `null` if no such
    scope exists.

  * If `scope` is not `null`, return `scope`'s member of `member`'s name and
    type.

  * Otherwise, return the result of [resolving `member`](#resolving-members)
    using `file`, `uses`, and `import`. If this returns null, throw an error.

* Finally:

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
      > guarantees [static analysis][] by ensuring that the set of variables a
      > module exposes doesn't depend on how it was executed.

  * Return `module`. Its functions, mixins, and CSS are now immutable.

  [variables spec]: ../spec/variables.md
  [static analysis]: #low-level

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
> @use "compass" as *;
>
> // Both libraries define their own "gutters()" functions. But because the
> // members are namespaced, there's no conflict and the user can use both at
> // once.
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

> Note that this procedure only covers non-local member resolution. Local
> members that are scoped to individual blocks are covered in [Executing
> Files](#executing-files).

* If `name` is a [namespaced identifier](#member-references)
  `namespace.raw-name`:

  * Let `use` be the `@use` rule in `uses` whose namespace is `namespace`. If
    there is no such rule, throw an error.

    > Unlike other identifiers in Sass, module namespaces *do not* treat `-` and
    > `_` as equivalent. This equivalence only exists for
    > backwards-compatibility, and since modules are an entirely new construct
    > it's not considered necessary.

  * If `use` hasn't been evaluated yet, throw an error.

  * Otherwise, let `module` be the module in `uses` associated with `use`.

  * Return the member of `module` with type `type` and name `raw-name`. If there
    is no such member, throw an error.

* If `type` is not "variable" and `file` contains a top-level definition of a
  member of type `type` named `name`:

  > A top-level variable definition will set the module's variable value rather
  > than defining a new variable local to this module.

  * If `import` contains a member `member` of type `type` named `name`, return
    it.

    > This includes member definitions within the current module.

  * Otherwise, return `null`.

    > This ensures that it's an error to refer to a local member before it's
    > defined, even if a member with the same name is defined in a loaded
    > module. It also allows us to guarantee that the referent to a member
    > doesn't change due to definitions later in the file.

* Let `member-uses` be the set of modules in `uses` whose `@use` rules are
  global, and which contain members of type `type` named `name`.

* Otherwise, if `import` contains a member `member` of type `type` named `name`:

  * If `member-uses` is not empty, throw an error.

  * Otherwise, return `member`.

* Otherwise, if `member-uses` contains more than one module, throw an error.

  > This ensures that, if a new version of a library produces a conflicting
  > name, it causes an immediate error.

* Otherwise, if `member-uses` contains a single module, return the member of
  type `type` named `name` in that module.

* Otherwise, if the implementation defines a global member `member` of type
  `type` named `name`, return that member.

  > This includes the global functions and mixins defined as part of the Sass
  > spec, and may also include other members defined through the
  > implementation's host language API.

* Otherwise, return null.

### Forwarding Modules

The [`@forward`](#forward-1) rule forwards another [module](#module)'s public
API as though it were part of the current module's.

> Note that `@forward` *does not* make any APIs available to the current module;
> that is purely the domain of `@use`. It *does* include the forwarded module's
> CSS tree, but it's not visible to `@extend` without also using the module.

This algorithm takes an immutable module `forwarded`, a [source
file](#source-file) `file`, and a mutable module `module`.
  
* For every member `member` in `forwarded`:

  * Let `name` be `member`'s name.
  
  * If `rule` has an `AsClause` `as`, prepend `as`'s identifier to `name` (after
    the `$` if `member` is a variable).

  * If there's a member defined at the top level of `file` named `name` with the
    same type as `member`, do nothing.

    > Giving local definitions precedence ensures that a module continues to
    > expose the same API if a forwarded module changes to include a conflicting
    > member.

  * Otherwise, if `rule` has a `show` clause that doesn't include `name`
    (including `$` for variables), do nothing.

    > It's not possible to show/hide a mixin without showing/hiding the
    > equivalent function, or to do the reverse. This is unlikely to be a
    > problem in practice, though, and adding support for it isn't worth the
    > extra syntactic complexity it would require.

  * Otherwise, if `rule` has a `hide` clause that does include `name` (including
    `$` for variables), do nothing.

  * Otherwise, if another `@forward` rule's module has a member named `name`
    with the same type as `member`, throw an error.

    > Failing here ensures that, in the absence of an obvious member that takes
    > precedence, conflicts are detected as soon as possible.

  * Otherwise, add `member` to `module` with the name `name`.

    > It's possible for the same member to be added to a given module multiple
    > times if it's forwarded with different prefixes. All of these names refer
    > to the same logical member, so for example if a variable gets set that
    > change will appear for all of its names.
    >
    > It's also possible for a module's members to have multiple prefixes added,
    > if they're forwarded with prefixes multiple times.

> This forwards all members by default to reduce the churn and potential for
> errors when a new member gets added to a forwarded module. It's likely that
> most libraries will already break up their definitions into many smaller
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
> // library. You could also choose not to forward this module at all and only
> // use it from internal modules.
> @forward "susy/settings" hide susy-defaults;
> ```

### Importing Files

For a substantial amount of time, `@use` will coexist with the old `@import`
rule in order to ease the burden of migration. This means that we need to define
how the two rules interact.

This algorithm takes a [source file](#source-file) `file`, an [import
context](#import-context) `import`, and a mutable [module](#module) `module`.

* If `file` is currently being executed, throw an error.

* Let `imported` be the result of [executing](#executing-files) `file` with the
  empty configuration and `import` as its import context, except that if the
  `@import` rule is nested within at-rules and/or style rules, that context is
  preserved when executing `file`.

  > Note that this execution can mutate `import`.

* Let `css` be the result of [resolving extensions](#resolving-extensions) for
  `imported`, except that if the `@import` rule is nested within at-rules and/or
  style rules, that context is added to CSS that comes from modules loaded by
  `imported`.

  > This creates an entirely separate CSS tree with an entirely separate
  > `@extend` context than normal `@use`s of these modules. This means their CSS
  > may be duplicated, and they may be extended differently.

* Add `css` to `module`'s CSS.

* Add `imported`'s [extensions](#extension) to `module`.

* If the `@import` rule is nested within at-rules and/or style rules, add each
  member in `imported` to the local [scope][].

* Otherwise, add each member in `imported` to `import` and `module`.

  > Members defined directly in `imported` will have already been added to
  > `import` in the course of its execution. This only adds members that
  > `imported` forwards.
  >
  > Members from `imported` override members of the same name and type that have
  > already been added to `import` and `module`.

> When a stylesheet contains only `@import`s without any `@use`s, the `@import`s
> are intended to work exactly as they did in previous Sass versions. Any
> difference should be considered a bug in this specification.

> This definition allows files that include `@use` to be imported. Doing so
> includes those modules' CSS as well as any members they define or forward.
> This makes it possible for users to continue using `@import` even when their
> dependencies switch to `@use`, which conversely makes it safer for libraries
> to switch to `@use`.
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
| `mix`                    |           | sass:color    |   | `unitless`               | `is-unitless`      | sass:math     |
| `hue`                    |           | sass:color    |   | `comparable`             | `compatible`       | sass:math     |
| `saturation`             |           | sass:color    |   |                          |                    |               |
| `lightness`              |           | sass:color    |   | `length`                 |                    | sass:list     |
| `complement`             |           | sass:color    |   | `nth`                    |                    | sass:list     |
| `invert`                 |           | sass:color    |   | `set-nth`                |                    | sass:list     |
| `alpha`                  |           | sass:color    |   | `join`                   |                    | sass:list     |
| `adjust-color`           | `adjust`  | sass:color    |   | `append`                 |                    | sass:list     |
| `scale-color`            | `scale`   | sass:color    |   | `zip`                    |                    | sass:list     |
| `change-color`           | `change`  | sass:color    |   | `index`                  |                    | sass:list     |
| `ie-hex-str`             |           | sass:color    |   | `list-separator`         | `separator`        | sass:list     |
|                          |           |               |   |                          |                    |               |
| `map-get`                | `get`     | sass:map      |   | `feature-exists`         |                    | sass:meta     |
| `map-merge`              | `merge`   | sass:map      |   | `variable-exists`        |                    | sass:meta     |
| `map-remove`             | `remove`  | sass:map      |   | `global-variable-exists` |                    | sass:meta     |
| `map-keys`               | `keys`    | sass:map      |   | `function-exists`        |                    | sass:meta     |
| `map-values`             | `values`  | sass:map      |   | `mixin-exists`           |                    | sass:meta     |
| `map-has-key`            | `has-key` | sass:map      |   | `inspect`                |                    | sass:meta     |
|                          |           |               |   | `get-function`           |                    | sass:meta     |
| `unquote`                |           | sass:string   |   | `type-of`                |                    | sass:meta     |
| `quote`                  |           | sass:string   |   | `call`                   |                    | sass:meta     |
| `str-length`             | `length`  | sass:string   |   | `content-exists`         |                    | sass:meta     |
| `str-insert`             | `insert`  | sass:string   |   | `keywords`               |                    | sass:meta
| `str-index`              | `index`   | sass:string   |   |                          | `module-variables` | sass:meta     |
| `str-slice`              | `slice`   | sass:string   |   |                          | `module-functions` | sass:meta     |
| `to-upper-case`          |           | sass:string   |   |                          |                    |               |
| `to-lower-case`          |           | sass:string   |   | `selector-nest`          | `nest`             | sass:selector |
| `unique-id`              |           | sass:string   |   | `selector-append`        | `append`           | sass:selector |
|                          |           |               |   | `selector-replace`       | `replace`          | sass:selector |
|                          |           |               |   | `selector-unify`         | `unify`            | sass:selector |
|                          |           |               |   | `is-superselector`       |                    | sass:selector |
|                          |           |               |   | `simple-selectors`       |                    | sass:selector |
|                          |           |               |   | `selector-parse`         | `parse`            | sass:selector |
|                          |           |               |   | `selector-extend`        | `extend`           | sass:selector |

In addition, one built-in mixin will be added:

| Name       | Module    |
| ---------- | --------- |
| `load-css` | sass:meta |

The existing built-in functions `adjust-hue()`, `lighten()`, `darken()`,
`saturate()`, `desaturate()`, `opacify()`, `fade-in()`, `transparentize()`, and
`fade-out()` will not be added to any module. Instead, functions with the same
names will be added to the `sass:color` module that will always emit errors
suggesting that the user use `color.adjust()` instead.

> These functions are shorthands for `color.adjust()`. However, `color.adjust()`
> generally produces less useful results than `color.scale()`, so having
> shorthands for it tends to mislead users. The automated module migrator will
> migrate uses of these functions to literal `color.adjust()` calls, and the
> documentation will encourage users to use `color.scale()` instead.
>
> Once the module system is firmly in place, we may add new `color.lighten()`
> *et al* functions that are shorthands for `color.scale()` instead.

The `grayscale()`, `invert()`, `alpha()`, and `opacity()` functions in
`sass:color` will only accept color arguments, unlike their global counterparts.

> These global functions need to accept non-color arguments for compatibility
> with CSS functions of the same names. Since module namespacing eliminates the
> ambiguity between built-in Sass functions and plain CSS functions, this
> compatibility is no longer necessary.

Built-in modules will contain only the functions described above. They won't
contain any other [members](#member), CSS, or extensions. New members may be
added in the future, but CSS will not be added to existing modules.

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

#### `module-variables()`

The `module-variables()` function takes a `$module` parameter, which must be a
string that matches the namespace of a `@use` rule in the current source file.
It returns a map from variable names (with all `_`s converted to `-`s) defined
in the module loaded by that rule (as quoted strings, without `$`) to the
current values of those variables.

> Variable names are normalized to use hyphens so that callers can safely work
> with underscore-separated libraries using this function the same as they can
> when referring to variables directly.

Note that (like the existing `*-defined()` functions), this function's behavior
depends on the lexical context in which it's invoked.

#### `module-functions()`

The `module-functions()` function takes a `$module` parameter, which must be a
string that matches the namespace of a `@use` rule in the current source file.
It returns a map from function names (with all `_`s converted to `-`s) defined
in the module loaded by that rule (as quoted strings) to function values that
can be used to invoke those functions.

> Function names are normalized to use hyphens so that callers can safely work
> with underscore-separated libraries using this function the same as they can
> when calling functions directly.

Note that (like the existing `*-defined()` functions), this function's behavior
depends on the lexical context in which it's invoked.

#### `load-css()`

The `load-css()` mixin takes a `$url` parameter, which must be a string, and an
optional `$with` parameter, which must be either a map with string keys or null.
When this mixin is invoked:

* Let `config` be a configuration whose variable names and values are given by
  `$with` if `$with` is passed and non-null, or the empty configuration
  otherwise.

* Let `module` be the result of [loading](#loading-modules) `$url` with
  `config`. The URL is loaded as though it appeared in a `@use` rule in the
  stylesheet where `@include load-css()` was written.

  > This means that `load-css()` doesn't see import-only stylesheets, and that
  > URLs are resolved relative to the file that contains the `@include` call
  > even if it's invoked from another mixin.

* Let `css` be the result of [resolving extensions](#resolving-extensions) for
  `module`.

  > This means that, if a module loaded by `load-css()` shares some dependencies
  > with the entrypoint module, those dependencies' CSS will be included twice.

* Treat `css` as though it were the contents of the mixin.

> The `load-css()` function is primarily intended to satisfy the use-cases that
> are currently handled using nested imports. It clearly also goes some way
> towards dynamic imports, which is listed as a non-goal. It's considered
> acceptable because it doesn't dynamically alter the names available to
> modules.

> There are a couple important things to note here. First, *every time*
> `load-css()` is included, its module's CSS is emitted, which means that the
> CSS may be emitted multiple times. This behavior makes sense in context, and
> is unlikely to surprise anyone, but it's good to note nonetheless as an
> exception to the import-once goal.
>
> Second, `load-css()` doesn't affect name resolution at all. Although it loads
> the module in an abstract sense, the user is only able to access the module's
> CSS, not any functions, mixins, or variables that it defines.
>
> ```scss
> // The CSS from the print module will be nested within the media rule.
> @media print {
>   @include load-css("print");
> }
>
> // These variables are set in the scope of susy's main module.
> @include load-css("susy", $with: (
>   "columns": 4,
>   "gutters": 0.25,
>   "math": fluid
> ));
> ```

### New Features For Existing Functions

Several functions will get additional features in the new module-system world.

The `global-variable-exists()`, `function-exists()`, `mixin-exists()`, and
`get-function()` functions will all take an optional `$module` parameter. This
parameter must be a string or `null`, and it must match the namespace of a
`@use` rule in the current module. If it's not `null`, the function returns
whether the module loaded by that rule has a member with the given name and
type, or in the case of `get-function()`, it returns the function with the given
name from that module.

If the `$module` parameter is `null`, or when the `variable-exists()` function
is called, these functions will look for members defined so far in the current
module or import context, members of any modules loaded by global `@use` rules,
or global built-in definitions. If multiple global `@use` rules define a member
of the given name and type, these functions will throw an error.

> We considered having the functions return `true` in the case of a conflicting
> member, but eventually decided that such a case was likely unexpected and
> throwing an error would help the user notice more quickly.

The `get-function()` function will throw an error if the `$module` parameter is
non-`null` *and* the `$css` parameter is truthy.

## Timeline

Our target dates for implementing and launching the module system are as
follows:

* **1 March 2019**: Support for `@use` without configuration or core libraries
  landed in a Dart Sass branch, with specs in a sass-spec branch.

* **1 August 2019**: Full support for this spec landed in a Dart Sass branch, with
  specs in a sass-spec branch.

* **1 September 2019**: Alpha release for Dart Sass module system support.

* **1 October 2019**: Stable release of Dart Sass module system support.

Although it would be desirable to have both Dart Sass and LibSass launch support
for the module system simultaneously, this hasn't proven to be logistically
feasible. As of August 2019, LibSass has not yet begun implementing the module
system, and there are no concrete plans for it to do so.

The Sass team wants to allow for a large amount of time when `@use` and
`@import` can coexist, to help the ecosystem smoothly migrate to the new system.
However, doing away with `@import` entirely is the ultimate goal for simplicity,
performance, and CSS compatibility. As such, we plan to gradually turn down
support for `@import` on the following timeline:

* One year after both implementations launch support for the module system *or*
  two years after Dart Sass launches support for the module system, whichever
  comes sooner (**1 October 2021** at latest): Deprecate `@import` as well as global
  core library function calls that could be made through modules.

* One year after this deprecation goes into effect (**1 October 2022** at
  latest): Drop support for `@import` and most global functions entirely. This
  will involve a major version release for all implementations.

This means that there will be at least two full years when `@import` and `@use`
are both usable at once, and likely closer to three years in practice.
