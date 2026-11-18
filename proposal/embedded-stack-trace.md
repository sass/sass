# Embedded Stack Trace: Draft 1.0

*([Issue](https://github.com/sass/sass/issues/4249))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
  * [Design Decisions](#design-decisions)
    * [Formatted Offset Fields](#formatted-offset-fields)
* [Embedded Protocol](#embedded-protocol)
  * [`StackFrame`](#stackframe)
  * [`CompileFailure`](#compilefailure)
  * [`LogEvent`](#logevent)

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

### Design Decisions

#### Formatted Offset Fields

A [requested feature] of this design was a way to exclude the stack trace from
the formatted message so that a stack trace with custom host formatting could be
presented instead. There were several possible ways to accomplish this, but each
had its downsides:

[requested feature]: https://github.com/sass/sass/pull/4250#issuecomment-5723109018

* We could require the host to manually collate a formatted message if they
  wanted to customize any aspect of it. This would mean that they'd have to
  abandon the compiler's formatting of the message and span, though, which can
  be fairly complex and difficult to reproduce, especially across versions.

* The host could manually remove the stack trace from the formatted message, but
  this would involve making assumptions about the format of the message which is
  explicitly unspecified.

* We could split the formatted message into three different parts, one each for
  the message, source span, and stack trace, and require the host to assemble
  them to its liking. However, this makes it harder to implement a simple host.
  In particular, the added friction could encourage users to surface only the
  unformatted message (available as a simple field) rather than assembling and
  surfacing the formatted one, which would be a poor outcome for our users.

As such, we decided on the slightly odd approach of including offsets into the
formatted message indicating where the source span and stack trace begin. This
preserves the formatted message as a single, easy-to-surface field while still
providing more advanced users the ability to slice it up as needed.

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
// The Sass stack trace of this error, ordered from innermost to
// outermost stack frames.
repeated StackFrame stack_trace = 5;
```

Add the following fields:

```proto
// The 0-based offset in `formatted` of the beginning of the formatted source
// span (after the previous newline). If no span exists, this is the same as
// `formatted_stack_trace_offset`.
//
// This is intended to make it possible for hosts to re-order or remove segments
// of the formatted message without being tightly coupled to its specific
// formatting.
uint32 formatted_span_offset = 6;

// The 0-based offset in `formatted` of the beginning of the formatted stack
// trace (after the previous newline).
//
// This is intended to make it possible for hosts to re-order or remove segments
// of the formatted message without being tightly coupled to its specific
// formatting.
uint32 formatted_stack_trace_offset = 7;
```

### `LogEvent`

Replace `string stack_trace = 5;` with

```proto
// The Sass stack trace at the point at which this log was generated, ordered
// from innermost to outermost stack frames.
repeated StackFrame stack_trace = 8;
```

Add the following fields:

```proto
// The 0-based offset in `formatted` of the beginning of the formatted source
// span (after the previous newline). If no span exists, this is the same as
// `formatted_stack_trace_offset`.
//
// This is intended to make it possible for hosts to re-order or remove segments
// of the formatted message without being tightly coupled to its specific
// formatting.
uint32 formatted_span_offset = 9;

// The 0-based offset in `formatted` of the beginning of the formatted stack
// trace (after the previous newline).
//
// This is intended to make it possible for hosts to re-order or remove segments
// of the formatted message without being tightly coupled to its specific
// formatting.
uint32 formatted_stack_trace_offset = 10;
```
