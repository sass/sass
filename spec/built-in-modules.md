# Built-In Modules

Sass provides a number of built-in [modules] that may be loaded from URLs with
the scheme `sass`. These modules have no extensions, CSS trees, dependencies, or
source files. Their canonical URLs are the same as the URLs used to load them.

[modules]: modules.md#module

## Built-In Functions and Mixins

Each function and mixin defined in a built-in modules is specified with a
signature of the form

<x><pre>
[\<ident-token>] [ParameterList]
</pre></x>

[\<ident-token>]: https://drafts.csswg.org/css-syntax-3/#ident-token-diagram
[ParameterList]: syntax.md#parameterlist

followed by a procedure. It's available as a member (either function or mixin)
in the module whose name is the value of the `<ident-token>`. When it's executed
with `args`:

* With an empty scope with no parent as the [current scope]:

  [current scope]: spec.md#scope

  * Evaluate `args` with the signature's `ParameterList`.

  * Run the procedure, and return its result if this is a function.

Built-in mixins don't accept content blocks unless explicitly specified
otherwise.

By convention, in these procedures `$var` is used as a shorthand for "the value
of the variable `var` in the current scope".

> In other words, `$var` is the value passed to the argument `$var`.
