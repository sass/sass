# `@forward`

The `@forward` rule loads a [module][] from a URL and adds its members to the
public API of the current module without making them available to use within the
current stylesheet.

[module]: ../modules.md#module

## Table of Contents

* [Syntax](#syntax)
* [Semantics](#semantics)

## Syntax

The grammar for the `@forward` rule is as follows:

<x><pre>
**ForwardRule** ::= '@forward' QuotedString AsClause? (ShowClause | HideClause)?
**AsClause**    ::= 'as' [\<ident-token>][] '\*'
**ShowClause**  ::= 'show' MemberName (',' MemberName)\*
**HideClause**  ::= 'hide' MemberName (',' MemberName)\*
**MemberName**  ::= '$'? [\<ident-token>][]
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram

`@forward` rules must be at the top level of the document, and must come before
any rules other than `@charset` or `@use`. The `QuotedString`'s contents, known
as the rule's *URL*, must be a [valid URL string][] (for non-[special][] base
URL). No whitespace is allowed after `$` in `MemberName`, or before `*` in
`AsClause`.

[valid URL string]: https://url.spec.whatwg.org/#valid-url-string
[special]: https://url.spec.whatwg.org/#special-scheme

## Semantics

> Note that `@forward` *does not* make any APIs available to the current module;
> that is purely the domain of `@use`. It *does* include the forwarded module's
> CSS tree, but it's not visible to `@extend` without also using the module.

To execute a `@forward` rule `rule`:

* If `rule` has an `AsClause` with identifier `prefix`:

  * Let `rule-config` be an empty [configuration][].

  * For each variable `variable` in [the current configuration][]:

    * If `variable`'s name begins with `prefix`:

      * Let `suffix` be the portion of `variable`'s name after `prefix`.

      * Add a variable to `rule-config` with the name `suffix` and with the
        same value as `variable`.

  [configuration]: ../modules.md#configuration
  [the current configuration]: ../spec.md#current-configuration

* Otherwise, let `rule-config` be the current configuration.

* Let `forwarded` be the result of [loading the module][] with `rule`'s URL
  string and `rule-config`.

  [loading the module]: ../modules.md#loading-a-module

* For every member `member` in `forwarded`:

  * Let `name` be `member`'s name.
  
  * If `rule` has an `AsClause` `as`, prepend `as`'s identifier to `name` (after
    the `$` if `member` is a variable).

  * If there's a member defined at the top level of [the current source file][]
    named `name` with the same type as `member`, do nothing.

  * Otherwise, if `rule` has a `show` clause that doesn't include `name`
    (including `$` for variables), do nothing.

    > It's not possible to show/hide a mixin without showing/hiding the
    > equivalent function, or to do the reverse.

  * Otherwise, if `rule` has a `hide` clause that does include `name` (including
    `$` for variables), do nothing.

  * If another `@forward` rule's module has a member named `name` with the same
    type as `member`:

    * If the other member is [identical to][] `member`, do nothing.

    * Otherwise, throw an error.

  * Otherwise, add `member` to [the current module][] with the name `name`.

    > It's possible for the same member to be added to a given module multiple
    > times if it's forwarded with different prefixes. All of these names refer
    > to the same logical member, so for example if a variable gets set that
    > change will appear for all of its names.
    >
    > It's also possible for a module's members to have multiple prefixes added,
    > if they're forwarded with prefixes multiple times.

  [the current source file]: ../spec.md#current-source-file
  [identical to]: ../modules.md#member
  [the current module]: ../spec.md#current-module
