## Draft 9

* Don't add imported module members to the global scope for a nested import.

## Draft 8

* Remove `adjust-hue()` from the `sass:color` module. `color.adjust($hue: ...)`
  should be used instead.

## Draft 7

* Change the syntax for namespaced variables from `$namespace.name` to
  `namespace.$name`.

* Make `module-variables()` and `module-functions()` normalize underscores to
  hyphens.

* Update the release timeline.

## Draft 6

* Require `ForwardRule`'s `AsClause` to come before `ShowClose` or `HideClause`,
  rather than after. This ensures that the clause with unbounded length comes
  last if both are present.

* Imported forwarded members now take precedence over members that were defined
  in the local file prior to the `@import`.

* Modules can now extend CSS from modules they forward but do not use.

* Only allow variables defined at the top level of a stylesheet to be configured
  with `@use ... with`.

* Allow variables imported by a stylesheet to be configured with `@use ...
  with`.

* Move `keywords()` from `sass:map` to `sass:meta`.

* Add `extend()` to `sass:selector`. This is the same as the global
  `selector-extend()` function.

* The `sass:color` functions `grayscale()`, `invert()`, `alpha()`, and
  `opacity()` no longer allow non-color arguments.

* Make `get-function()` throw an error if `$module` and `$css` are both passed.

* Describe how to resolve built-in global functions and mixins.

## Draft 5

* Drop the `lighten()`, `darken()`, `saturate()`, `desaturate()`, `opacify()`,
  and `transparentize()` functions from the `color` module. The use of these
  functions is discouraged, and they're just shorthands for the `color.adjust()`
  function so including them isn't necessary to allow migration.

* Give files with the suffix `".css"` lower precedence than `".sass"` and
  `".scss"` files even in `@use`. This accommodates the use-case of `@use`ing a
  file that's also being compiled in-place to a CSS file.

* Add a `$module` parameter to `get-function()`.

* Load CSS from modules used by imported files.

* Clarify the behavior of the first law of extend when multiple modules extend
  the same selector.

* Remove missing outdated entries from the table of contents.

## Draft 4.2

* Add "static analysis" as a low-level goal.

* Ensure that plain CSS imports always appear at the beginning of the generated
  CSS.

* Other than plain CSS imports, always emit the CSS for a given module
  (including comments) to the same location, rather than splitting it up if a
  comment appeared above a `@use`.

* Forbid diamond extensions from interacting with one another. That is, if two
  modules use the same upstream modules but don't use one another, ensure that
  they cannot extend one another's selectors.

* Explicitly indicate that only selectors explicitly written by hand are exempt
  from being optimized away when resolving extensions.

* Always add `!global` variables to a module's variable set, even if those
  variable declarations aren't evaluated.

* Explicitly define that `*-exists()` functions should throw an error for
  conflicting member names from global modules.

* Explicitly define how members are resolved locally versus globally.

* Fix some faulty logic around resolving namespaceless members. If a member is
  defined in both the import context and a global module that's now an error.

* Syntactically forbid namespaced references to private identifiers (as in
  `foo.-bar`). This is never valid, so making it a syntax error rather than just
  a runtime error ensures that the user will be notified as eagerly as possible.

* Fix the logic for import-only files so that `@import "foo.scss"` doesn't try
  to load `foo.scss.import` and `@import "foo"` doesn't try to load
  `foo.import/index.scss`.

* Rename configuration variables when they're passed to a forwarded module with
  an `AsClause`.

* Only allow top-level members to shadow forwarded members.

* Add an imported file's members to the current module.

* Make config resolution part of evaluating a `!default` variable assignment
  rather than part of resolving any variable.

* Clean up the way "Forwarding Modules" and "Importing Files" are invoked.

* Fix a few broken links.

## Draft 4.1

* Make the release timeline more concrete.

* Fix a broken link in the table of contents.

## Draft 4

* Allow `@forward "module" as prefix-*` to add a prefix to forwarded module.

* Rename `math.unitless()` to `math.is-unitless()`, to match
  `list.is-bracketed()` and make it clear that it doesn't remove units from a
  number.

* Rename `math.comparable()` to `math.compatible()`, to make it clear that it
  also tests for compatibility for addition and subtraction.

* Add the missing `content-exists()` function.

* Move `meta.unique-id()` to `string.unique-id()`.

* Add "code splitting" as a non-goal.

## Draft 3

* Limit extensions to affecting only modules transitively used by the module in
  which the `@extend` appears.

* Replace module mixins with a built-in `load-css()` mixin that dynamically
  includes the CSS for a module with a given URL.

* Add support for configuring modules using a new `with` clause.

* Update the `module-variables()` and `module-functions()` functions to return
  maps from names to values, rather than just lists of names.

* Remove the `module-mixins()` function until Sass supports first-class mixins.

* Add support for `_file.import.scss` as a file that only `@import`s will see.

* Change the syntax for a `@use` rule without a namespace to `@use "..." as *`.

* Initialize modules' variables with the values as declared in those modules.

* Allow comments to be emitted before dependencies' CSS.

* Show or hide variables with their `$` prefixes in `@forward`.

* Define a source file as an AST plus a canonical URL. This means that built-in
  modules are no longer source files, which seems more reasonable.

* Clarify that `@forward` includes the forwarded module's CSS tree.

* Pass configuration for a module to any modules it `@forward`s.

* Forbid whitespace in various member-reference productions.

* Explicitly indicate that extensions are dynamically scoped.

* Explicitly indicate which parts of a module are immutable.

* Explicitly describe how variable declarations are resolved.

* Explicitly describe how a configuration affects file evaluation.

* Explicitly mention that variable declarations are allowed before `@use`.

* Loading a module with configuration variables it doesn't expose is now an
  error.

* Don't make nested mixin and function declarations part of a module's API.

* Re-organize "Resolving Extensions" to make its behavior clearer.

* Link to the existing import spec rather than redefining terms.

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
