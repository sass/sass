# `@content` Arguments: Draft 1

*([Issue](https://github.com/sass/sass/issues/871))*

## Table of Contents

* [Summary](#summary)
  * [Design Decisions](#design-decisions)
* [Syntax](#syntax)
* [Semantics](#semantics)

## Summary

> This section is non-normative.

This proposal adds the ability for mixins to pass arguments to their `@content`
blocks. These arguments are passed using the normal argument syntax, and
accepted using a new `using` syntax for `@include`:

```scss
@mixin accepts-content {
  @for $i from 1 to 5 {
    @content($i, 360deg * $i / 5);
  }
}

@include accepts-content using ($number, $hue) {
  .color-#{$number} {
    background-color: hsl($hue, 75%, 90%);
  }
}
```

The argument list for `using` supports all the features of a normal argument list
declaration.

### Design Decisions

While the syntax for passing arguments to `@content` was pretty obvious, a
number of different syntaxes were considered for declaring which arguments are
accepted by an `@include`, including:

```scss
// Adding a new at-rule:
@include accepts-content {
  @receive ($number, $hue);
  // ...
}

// Adopting Ruby's block syntax:
@include accepts-content { |$number, $hue|
  // ...
}

// A Haskell-like punctuation syntax:
@include accepts-content -> ($number, $hue) {
  // ...
}
```

We decided on `using` over `@receive` because it reads clearer to have the
arguments be part of the `@include` syntax rather than in the mixin body. We
chose it over Ruby- or Haskell-style because we generally prefer the use of
words over punctuation in Sass.

We decided to use `using` as opposed to another word because it's relatively
terse while still being clear. We considered `as` instead, but decided the
meaning of the word didn't match the semantics; and we considered `with`, but
decided that it was both semantically ambiguous and confusing because the
[module system proposal][] uses to declare an argument *invocation* rather than
an argument *declaration*.

[module system proposal]: module-system.md

## Syntax

This proposal updates the syntax for `@content` and `@include` as follows:

<x><pre>
**ContentRule**      ::= '@content' ArgumentInvocation?
**IncludeRule**      ::= '@include' Identifier ArgumentInvocation? ContentBlock?
**ContentBlock**     ::= UsingDeclaration? '{' Statements '}'
**UsingDeclaration** ::= 'using' ArgumentDeclaration
</pre></x>

## Semantics

This proposal defines a new algorithm for evaluating a `@content` rule. Given a
`@content` rule `content` within a mixin that's invoked with an `@include` rule
`include`:

* Let `invocation` be `content`'s `ArgumentInvocation`, or an invocation with no
  arguments if `content` has no `ArgumentInvocation`.

  > This means that `@content` and `@content()` are interpreted identically.

* Let `declaration` be `include`'s `UsingDeclaration`'s `ArgumentDeclaration`,
  or a declaration with no arguments if `include` has no `UsingDeclaration`.

  > This means that `@include foo { ... }` and `@include foo using () { ... }`
  > are interpreted identically.

* Let `arguments` be the result of applying `invocation` to `declaration`.

  > This means `arguments` is a mapping from variable names to values. If
  > `invocation` isn't a valid invocation of `declaration`, this will throw an
  > error that should be surfaced to the user.

* If `include` has no `ContentBlock`, do nothing.

  > Exiting here rather than earlier means that `@content(value)` is an error if
  > `include` has no content block.

* Otherwise, let `scope` be a new scope that's a child of `include`'s scope.

* For each pair `variable` and `value` in `arguments`:

  * Set `variable` to `value` in `scope`.

* Evaluate `include`'s `ContentBlock` statements in `scope`.
