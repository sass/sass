# Embedded Stack Trace: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4249))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Embedded Protocol](#embedded-protocol)
  * [`StackFrame`](#stackframe)
  * [`CompileFailure`](#compilefailure)
  * [`CompileFailure`](#compilefailure-1)

## Background

> This section is non-normative.

Currently, `CompileResponse.CompileFailure` and `LogEvent` each have both
`stack_trace` and `formatted` fields. The `stack_trace` field is largely
redundant: it provides no guaranteed format, so it's only useful for displaying
to the user, but the `formatted` field *already* includes the stack trace (as
well as the message and span) in a format intended to display to the user.

In addition, some embedded hosts want to handle and display stack traces in
formats that are more native to the host context. Parsing these from the
human-readable format is inconsistent and [error-prone], as human-friendly
optimizations like converting absolute URLs to relative paths make the trace
potentially ambiguous for a mechanical parser.

[error-prone]: https://github.com/sass/dart-sass/issues/2775

## Summary

> This section is non-normative.

This proposal replaces the existing string `stack_trace` field with a full
message-based list of stack frames intended for machine consumption.

## Embedded Protocol

This proposal involves a breaking change to the embedded protocol and as such
increases the version number to 4.0.0.

### `StackFrame`

This proposal adds a new top-level `StackFrame` message:

```proto
// A single frame of a Sass stack trace, indicating a portion of the stylesheet
// currently being executed.
message StackFrame {
  // A human-friendly description of what's being executed. Often, but not
  // always, a mixin or function name.
  string description = 1;
  
  // The 0-based line number of this frame within the source file.
  uint32 line = 2;

  // The 0-based column number of this frame within its line.
  uint32 column = 3;

  // The URL of the stylesheet in which this frame appears.
  //
  // This may be empty, indicating that the frame appears in a
  // `CompileRequest.StringInput` file that doesn't specify a URL.
  string url = 4;
}
```

### `CompileFailure`

Replace `string stack_trace = 3;` with

```proto
/// The Sass stack trace of this error, ordered from innermost to
/// outermost stack frames.
repeated StackFrame stack_trace = 5;
```

### `CompileFailure`

Replace `string stack_trace = 5;` with

```proto
/// The Sass stack trace at the point at which this log was generated, ordered
/// from innermost to outermost stack frames.
repeated StackFrame stack_trace = 8;
```
