# The Next-Generation Sass Module System

This repository houses a proposal for the `@use` directive and associated module
system, which is intended to be the headlining feature for Sass 4. This is a
*living proposal*: it's intended to evolve over time, and is hosted on GitHub to
encourage community collaboration and contributions. Any suggestions or issues
can be brought up and discussed on [the issue tracker][issues].

[issues]: https://github.com/sass/proposal.module-system

*Note: at the time of writing, the initial draft of the proposal is not yet
complete*.

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
