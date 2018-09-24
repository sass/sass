## Draft 3

* Limit extensions to affecting only modules transitively used by the module in
  which the `@extend` appears ([#6][]).

  [#6]: https://github.com/sass/language/issues/6

* Update the `module-variables()` and `module-functions()` functions to return
  maps from names to values, rather than just lists of names ([#12][]).
  
  [#12]: https://github.com/sass/language/issues/12

* Remove the `module-mixins()` function until Sass supports first-class mixins
  ([#12][]).

* Change the syntax for a `@use` rule without a namespace to `@use "..." as *`
  ([#10][]).

  [#10]: https://github.com/sass/language/issues/10

* Initialize modules' variables with the values as declared in those modules
  ([#13][]).

  [#13]: https://github.com/sass/language/issues/13

* Clarify that `@forward` includes the forwarded module's CSS tree.

* Forbid whitespace in various member-reference productions.

* Explicitly indicate that extensions are dynamically scoped ([#7][]).

  [#7]: https://github.com/sass/language/issues/7

* Explicitly indicate which parts of a module are immutable.

## Draft 2.1

* Make sure nested `@import`s preserve the parent selector/at-rule context of
  the current stylesheet.

* Removed the low-level "using CSS files" goal, since this is now covered by
  [the CSS Imports proposal][].

  [the CSS Imports proposal]: ../accepted/css-imports.md

* Add "Context-independent modules" and "Increased strictness" as non-goals.

* Add a couple FAQs.

* Made the requirements for valid module URLs more explicit.

* Merged the "Using Modules" section into the "Loading Modules" algorithm.

* Added a separate section for "Determining Prefixes".

* Make the import context mandatory everywhere, to simplify logic.
  Implementations are still free to allocate them lazily, though.

* Consistently put non-normative asides in block quotes.

* Refer to "namespaces" rather than "prefixes".

* Refer to "URLs" rather than "URIs", per [the URL spec][].

  [the URL spec]: https://url.spec.whatwg.org/#goals

* Refer to "at-rules" rather than "directives".

* Refer to "style rules" rather than "CSS rules".

* Format and structure algorithms consistently with more recent proposals.

* Other small non-semantic changes.

## Draft 2

* The namespace separator is now a period rather than a hyphen.

* Placeholder selectors are no longer considered members of modules. They are
  still allowed to be marked private, however.

* Be explicit about how URIs are canonicalized and thus compared.

* Allow `@forward` and `@use` to be intermingled.

* Allow `@extend` to affect downstream modules as well as upstream ones.
  Downstream modules inherently share the same semantics for selector names, and
  extensions are an aspect of those semantics.

* Don't allow module mixin arguments to be passed by position.

* To `@forward` a module mixin, use the module's prefix rather than its URI.

* Modules now export *all* variables that have global definitions, even if those
  definitions weren't executed. This preserves the invariant that modules'
  member sets are statically knowable.

* Add new functions for module introspection.

* Add a `$module` parameter to `global-variable-exists()`, `function-exists()`,
  and `mixin-exists()`.

## Draft 1

* Initial draft.
