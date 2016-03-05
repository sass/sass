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

## Draft 1

* Initial draft.
