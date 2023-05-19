# Embedded Protocol Version 2

*([Issue 1](https://github.com/sass/sass/issues/3579),
[Issue 2](https://github.com/sass/sass/issues/3575),
[Issue 3](https://github.com/sass/sass/issues/3577))*

## Table of Contents

* [Background](#background)
* [Summary](#summary)
* [Overview](#overview)
* [RPCs](#rpcs)
  * [Type Definitions](#type-definitions)
  * [ID Requirements](#id-requirements)
  * [Optional and Mandatory Fields](#optional-and-mandatory-fields)
* [Error Handling](#error-handling)
* [Protocol Buffer](#protocol-buffer)
  * [ID Fields](#id-fields)
  * [Optional Fields](#optional-fields)
  * [Loaded URLs](#loaded-urls)

## Background

> This section is non-normative.

Since the [embedded Sass protocol] was first released, a number of improvements
have been suggested by embedded host authors that would either be
backwards-incompatible, or would require cumbersome redundancies in the protocol
to make them backwards-compatible.

[embedded Sass protocol]: ../spec/embedded-protocol.md

In the meantime, to provide an efficient Sass CLI along with embedded hosts, the
Dart Sass embedded compiler was merged into the main Dart Sass executable.
Because this is effectively also a breaking change in how hosts obtain and
invoke the compiler, it seemed like a good opportunity to make the other
breaking protocol changes that had been piling up.

## Summary

> This section is non-normative.

This proposal makes three breaking changes to the embedded Sass protocol:

* Replace the compilation IDs in the protobufs with one directly encoded in the
  wire protocol so that compiler implementations can efficiently route requests
  to different workers without reparsing messages.

* Use optional fields as defined in protocol buffers 3.15.0 instead of treating
  default field values as absent.

* Move `CompileSuccess.loaded_urls` to `CompileResult.loaded_urls` so it's
  available even when compilation fails.

## Overview

Replace the last paragraph of the [embedded protocol overview] with:

[embedded protocol overview]: ../spec/embedded-protocol.md#overview

Each message in the embedded protocol is sent as a _packet_ which contains two
values: an unsigned [varint] up to 32 bits long known as the "compilation ID",
and a protocol buffer that contains the protobuf message. For streams (like
standard input and output) that don't have built-in message boundaries, every
packet must begin with another unsigned varint indicating the length in bytes of
the remaining message (_including the compilation ID_). This matches the best
practice described in [the protocol buffer documentation].

Because JavaScript can't easily represent integers larger than 2^53 - 1, the
length may be no more than 2^53 - 1. Because it's so unlikely that this will
come up in practice, implementations are not required to verify it.

[varint]: https://developers.google.com/protocol-buffers/docs/encoding#varints
[the protocol buffer documentation]: https://developers.google.com/protocol-buffers/docs/techniques#streaming

For a length-delimited stream, each packet has the following structure:

```
╔══════════╦══════════════════╗
║ varint   ║ Length           ║
╠══════════╬══════════════════╣
║ varint   ║ Compilation ID   ║
╠══════════╬══════════════════╣
║ protobuf ║ Protobuf Message ║
╚══════════╩══════════════════╝
```

## RPCs

### Type Definitions

Replace the following RPC type definitions:

* *Requests* usually include a mandatory `uint32 id` field so that the other
  endpoint can respond, except for `CompilationRequest` which uses the
  compilation ID as its ID. All request message types end in `Request`.

* *Responses* usually include a mandatory `uint32 id` field whose value must be
  the same as their associated request's `id`, except for `CompilationResponse`
  which uses the compilation ID as its ID. All response message types begin with
  the corresponding request name and end with `Response`.

### ID Requirements

Replace the paragraph that beings "Implementations must guarantee that they use
a unique `id` for every request" with:

Each endpoint must guarantee that each request's `id` doesn't match the `id` of
any other outstanding request from that endpoint, although the same `id` may be
used for an inbound request and an outbound request. The host must similarly
guarantee that a `CompileRequest`'s compilation ID doesn't match the compilation
ID of any other outstanding `CompileRequest`. The compiler must ensure that all
outbound requests' compilation IDs match that of the `CompileRequest` that
triggered its associated compilation.

The compilation ID 0 is reserved for `VersionRequest` and `VersionResponse`,
since they're not specific to any individual compilation.

The compilation ID and normal request `id` `4294967295` is reserved for [error
handling]. (This is the maximum number representable by a `uint32`.)

[error handling]: #error-handling

### Optional and Mandatory Fields

Replace the paragraphs about optional and mandatory fields with:

If a field is not optional, the the endpoint that sends that message must
guarantee that it's set to a meaningful value, and the endpoint that receives it
must reject the message if it's not set.

## Error Handling

Append to the first paragraph:

The compilation ID must match the compilation ID of the request or response that
triggered the error.

## Protocol Buffer

### ID Fields

Remove the following fields:

* `CompileRequest.id`
* `CompileResponse.id`
* `LogEvent.compilation_id`
* `CanonicalizeRequest.compilation_id`
* `ImportRequest.compilation_id`
* `FileImportRequest.compilation_id`
* `FunctionCallRequest.compilation_id`

### Optional Fields

Mark the following fields as optional, and update each one's specification to
check if they're unset instead of the default values:

* `CanonicalizeResponse.result`
* `ImportSuccess.source_map_url`
* `ImportResponse.result`
* `FileImportResponse.result`
* `LogEvent.span`
* `SourceSpan.end`

### Loaded URLs

Remove `CompileSuccess.loaded_urls` and add `CompileResponse.loaded_urls` with
the same specification.
