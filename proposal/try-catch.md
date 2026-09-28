# Support `@try` and `@catch` flow controls for error handling: Draft 1

*[(Issue)](https://github.com/sass/sass/issues/2619)*

## Table of Contents

## Background

> This section is non-normative.

In many situations, particularly when writing tests, it's useful to catch error
and warning messages without breaking compilation. By adding `@try` and `@catch`
at-rules similar to other control-flow syntax, authors will have more control
over error handling, and the ability to test for error-cases.

Many large Sass projects currently use wrapper functions and mixins to catch and
manage errors, with a parameter to toggle between throwing or returning the
error message. But that relies on the special error handling to be built into
every individual function or mixin that might error. By adding global controls,
error-handling can happen either where the error is generated, or where it is
defined.

## Summary

> This section is non-normative.

This proposal defines a pair of new at-rule directives that must be used
together as a form of control. The `@try { ... } @catch { ... }` syntax captures
any thrown output (error or warning messages) from inside the `@try` block, and
allows the parser to continue compilation. The `@catch` block executes whenever
an error or warning has been captured, allowing authors to determine how the
error should be processed.

```scss
@try {
  $test: my.function('bad arg');
} @catch $error {
  @include true.assert-equal(
    $error,
    "'bad arg' is not a valid argument for 'function'"
  );
}
```

## Syntax

<x><pre>
**TryCatchRule**    ::= '@try' '{'
&#32;                     TryStatements
&#32;                   '}' '@catch' exceptionVar? '{'
&#32;                     CatchStatements
&#32;                   '}'
**exceptionVar**    ::= '$' Identifier
</pre></x>

## Semantics

To execute a `TryCatchRule` `rule`:

* Let `exceptions`

* Execute the contents `try` of `rule`'s `TryStatements`

  * If an `@error` or `@warn` rule `throw` is encountered:

    * Let `exception` be a map with the same identifier as `exceptionVar`,
      a 'name' `throw`s at-rule
      
      > Currently either `@error` or `@warn`

    * Let `message` be the value of the `throw`'s `expression`

    * If `name` is `@error`, ignore any remaining statements in `try`

    * Otherwise complete compilation of the `TryStatements`

    * If `exceptionVar` is defined
      
      * Let `exception` be a map variable with the same name as `exceptionVar`'s
        identifier.

      * Let `exception` have a key 'name' with the value of `name`

      * Let `exception` have a key 'messsage' with the value of `message`

      > assign `message` to identifier `catchMessage`...

    > Execute the contents of `rule`'s `CatchStatements` with `catchMessage` and
    >  `catchType`...

  * Otherwise, ignore the remainder of the `TryCatchRule`

